import Data.Char (toLower)
import Data.IORef
import Data.List
import Data.Maybe
import Data.Tree (flatten)
import Network
import System.Random
import System.Environment (getArgs, getProgName)
--import System.CPUTime
--import System.Directory
import System.Exit
import System.IO
import System.IO.Error
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception
import Control.Monad.Reader
import Text.Printf
--import Text.Regex.TDFA
import Prelude

import NLP.WordNet
import NLP.WordNet.Prims (indexLookup, senseCount, getSynset, getWords, getGloss)
import NLP.WordNet.PrimTypes

wndir     = "/usr/share/wordnet/dict/"
channels  = ["#lolbots"]

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
data Bot = Bot {
    socket :: Handle,
    wne :: WordNetEnv,
    nick :: IORef String,
    owner :: IORef String,
    rejoinkick :: IORef Int,
    maxchanlines :: IORef Int
    }

-- Stuff for changing bot operating parameters.
data Parameter = RejoinKick | RejoinTimeout | Rudeness | Verbosity | OpControl | MaxChanLines | UnknownParam
    deriving (Eq, Ord, Show)

allParams = [RejoinKick ..]

instance Enum Parameter where
    toEnum 1 = RejoinKick
    toEnum 2 = RejoinTimeout
    toEnum 3 = Rudeness
    toEnum 4 = Verbosity
    toEnum 5 = OpControl
    toEnum 6 = MaxChanLines
    toEnum 7 = UnknownParam
    fromEnum RejoinKick     = 1
    fromEnum RejoinTimeout  = 2
    fromEnum Rudeness       = 3
    fromEnum Verbosity      = 4
    fromEnum OpControl      = 5
    fromEnum MaxChanLines   = 6
    fromEnum UnknownParam   = 7
    enumFrom i = enumFromTo i UnknownParam
    enumFromThen i j = enumFromThenTo i j UnknownParam

readParam :: String -> Parameter
readParam a | (map toLower a) == "rejoinkick"      = RejoinKick
readParam a | (map toLower a) == "rejointimeout"   = RejoinTimeout
readParam a | (map toLower a) == "rudeness"        = Rudeness
readParam a | (map toLower a) == "verbosity"       = Verbosity
readParam a | (map toLower a) == "opcontrol"       = OpControl
readParam a | (map toLower a) == "maxchanlines"    = MaxChanLines
readParam _                                        = UnknownParam

-- Set up actions to run on start and end, and run the main loop.
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = do hClose . socket ; closeWordNet . wne
    loop st    = catchIOError (runReaderT run st) (const $ return ())

-- Get command line options.
cmdLine :: IO [String]
cmdLine = do
    -- server, port, nick, owner, initial channels, Wordnet directory
    args <- getArgs
    prog <- getProgName
    let l            = length args
    let serverPos    = (maximum $ elemIndices "-server" args) + 1
    let server       = if l > serverPos then args !! serverPos else ""
    let portPos      = (maximum $ elemIndices "-port" args) + 1
    let port         = if l > portPos then args !! portPos else ""
    let nickPos      = (maximum $ elemIndices "-nick" args) + 1
    let nick         = if l > nickPos then args !! nickPos else ""
    let ownerPos     = (maximum $ elemIndices "-owner" args) + 1
    let owner        = if l > ownerPos then args !! ownerPos else ""
    return (server : port : nick : owner : [])

-- Connect to the server and return the initial bot state.  Initialize WordNet.
connect :: IO Bot
connect = notify $ do
    args <- cmdLine
    let server  = args !! 0
    let port    = read $ args !! 1
    let nick'   = args !! 2
    let owner'  = args !! 3
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    w <- initializeWordNetWithOptions (return wndir :: Maybe FilePath) 
      (Just (\e f -> putStrLn (e ++ show (f :: SomeException))))
    n <- newIORef nick'
    o <- newIORef owner'
    rk <- newIORef 0
    m <- newIORef 2
    return (Bot h w n o rk m)
  where
    notify a = bracket_
        (printf "Connecting to wherever... " >> hFlush stdout)
        (putStrLn "done.")
        a

-- Return owner.
botOwner :: Net String
botOwner = do
    o <- asks owner
    oo <- io $ readIORef o
    return oo

-- Return nick.
botNick :: Net String
botNick = do
    n <- asks nick
    nn <- io $ readIORef n
    return nn

-- Join (or leave) a list of channels.
joinChannel :: String -> [String] -> Net ()
joinChannel _  []    = return () :: Net ()
joinChannel [] b     = joinChannel "join" b
joinChannel a (x:xs) = do
    if a == "JOIN" || a == "PART" then do
      write a x
      joinChannel a xs
    else return ()

-- Change bot operating parameters.
changeParam :: String -> String -> Net ()
changeParam a b = do
    rk <- asks rejoinkick
    m <- asks maxchanlines
    case (readParam a) of
      -- Rude         -> io $ writeIORef r
      -- OpControl    -> io $ writeIORef o
      RejoinKick   -> io $ writeIORef rk (read b)
      MaxChanLines -> io $ writeIORef m (read b)
      _            -> return ()

-- Join a channel, and start processing commands.
run :: Net ()
run = do
    n <- botNick
    write "NICK" n
    write "USER" (n ++" 0 * :user")
    joinChannel "JOIN" channels
    asks socket >>= listen

-- Process each line from the server (this needs flood prevention somewhere).
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    n <- botNick
    o <- botOwner
    io (putStrLn s)
    if ping s then pong s else processLine n o (words s)
  where
    forever a = a >> forever a
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)

-- getCPUTimeSecs :: IO Double
-- getCPUTimeSecs = do
--    a <- getCPUTime
--    return (fromInteger a / 1000000000)

-- Get the actual message.
getMsg :: [String] -> [String]
getMsg [] = []
getMsg a
    | (length a) < 5                 = []
    | (head $ drop 1 a) == "PRIVMSG" = (drop 1 (a!!3)) : (drop 4 a)
    | otherwise                      = []

-- Who is speaking to us?
getNick :: [String] -> String
getNick = drop 1 . takeWhile (/= '!') . head

-- Which channel is message coming from?  Also could be private message.
getChannel :: [String] -> String
getChannel = head . drop 2

-- Are we being spoken to?
spokenTo :: String -> [String] -> Bool
spokenTo n []            = False
spokenTo n b
    | c == n          = True
    | c == (n ++ ":") = True
    | otherwise       = False
  where
    c = (head b)

-- Is this a private message?
-- isPM :: String -> [String] -> Bool
-- isPM [] = False
-- isPM a b
--     | getChannel b == a = True
--     | otherwise         = False

-- Have we been kicked from a channel?
beenKicked :: String -> [String] -> String
beenKicked _ [] = []
beenKicked n a
    | (head $ drop 1 a) == "KICK" = if (head $ drop 3 a) == n then getChannel a else []
    | otherwise                   = []

-- Possibly rejoin a channel depending on RejoinKick time.
rejoinChannel :: String -> Net ()
rejoinChannel [] = return () :: Net ()
rejoinChannel chan = do
    h <- asks socket
    rk <- asks rejoinkick
    rtime <- io $ readIORef rk
    if rtime == 0 then return () else io (rejoin' rtime chan h) >> return ()
  where
    rejoin' rtime chan h = forkIO (threadDelay (rtime * 1000000) >> hPrintf h "JOIN %s\r\n" chan)

-- Process IRC line.
processLine :: String -> String -> [String] -> Net ()
processLine _ _ [] = return ()
processLine n o a
    | (not $ null $ beenKicked n a) = rejoinChannel $ beenKicked n a
    | null msg            = return () -- Ignore because not PRIVMSG.
    | chan == n           = if (head $ head msg) == '!' then evalCmd who who o msg -- Evaluate command (the double "who" is significant).
                            else reply [] who msg -- Respond to PM.
    | spokenTo n msg      = if (head $ head $ tail msg) == '!'
                            then evalCmd chan who o (joinWords '"' (tail msg)) -- Evaluate command.
                            else reply chan who (tail msg) -- Respond upon being addressed.
    | otherwise           = return ()
    -- | otherwise         = processMsg chan who msg -- Process message.
    -- | otherwise         = reply chan [] msg -- Testing.
  where
    msg  = getMsg a
    who  = getNick a
    chan = getChannel a

-- Reply to message.
reply :: String -> String -> [String] -> Net ()
reply []   who msg = privMsg who "Eh?" -- PM.
reply chan []  msg = chanMsg chan $ reverse $ unwords msg -- Cheesy reverse gimmick, for testing.  Channel talk.
reply chan who msg = replyMsg chan who $ reverse $ unwords msg

-- Process messages.
--processMsg :: String -> String -> [String] -> ReaderT Bot IO ()
--processMsg chan' who' msg' =

-- Evaluate commands.
evalCmd :: String -> String -> String -> [String] -> Net ()
evalCmd _ b o (x:xs) | x == "!quit"      = if b == o then write "QUIT" ":Exiting" >> io (exitWith ExitSuccess) else return ()
evalCmd _ b o (x:xs) | x == "!join"      = if b == o then joinChannel "JOIN" xs else return ()
evalCmd _ b o (x:xs) | x == "!part"      = if b == o then joinChannel "PART" xs else return ()
evalCmd a b o (x:xs) | x == "!setparam"  = if b == o then case (length xs) of
                                                              2 -> changeParam (xs!!0) (xs!!1)
                                                              _ -> replyMsg a b "Usage: !setparam parameter value"
                                                       else return ()
evalCmd a b o (x:xs) | x == "!params"    = if b == o then replyMsg a b (init (concat $ map (++ " ") $ map show $ init allParams)) else return ()
evalCmd a b o (x:xs) | x == "!related"   =
    case (length xs) of
      3 -> wnRelated a b (xs!!0) (xs!!1) (xs!!2)
      2 -> wnRelated a b (xs!!0) (xs!!1) []
      1 -> wnRelated a b (xs!!0) []      []
      _ -> replyMsg a b "Usage: !related word [form] [part-of-speech]"
evalCmd a b o (x:xs) | x == "!closure"   =
    case (length xs) of
      3 -> wnClosure a b (xs!!0) (xs!!1) (xs!!2)
      2 -> wnClosure a b (xs!!0) (xs!!1) []
      1 -> wnClosure a b (xs!!0) []      []
      _ -> replyMsg a b "Usage: !closure word [form] [part-of-speech]"
evalCmd a b o (x:xs) | x == "!gloss"     =
    case (length xs) of
      2 -> wnGloss a b (xs!!0) (xs!!1)
      1 -> wnGloss a b (xs!!0) []
      _ -> replyMsg a b "Usage: !gloss word [part-of-speech]"
evalCmd a b o (x:xs) | x == "!meet"      =
    case (length xs) of
      3 -> wnMeet a b (xs!!0) (xs!!1) (xs!!2)
      2 -> wnMeet a b (xs!!0) (xs!!1) []
      _ -> replyMsg a b "Usage: !meet word word [part-of-speech]"
evalCmd a b o (x:xs) | x == "!forms"     = replyMsg a b (init (concat $ map (++ " ") $ map show $ init allForm))
evalCmd a b o (x:xs) | x == "!parts"     = replyMsg a b (init (concat $ map (++ " ") $ map show allPOS))
evalCmd a b o (x:xs) | x == "!help"      =
    if b == o then replyMsg a b "Commands: !related !closure !gloss !meet !forms !parts !params !setparam !join !part !quit"
    else replyMsg a b "Commands: !related !closure !gloss !meet !forms !parts"
evalCmd _ _ _ _                         = return ()

-- Send a message to the channel.
chanMsg :: String -> String -> Net ()
chanMsg chan' msg = write "PRIVMSG" (chan' ++ " :" ++ msg)

-- Send a reply message.
replyMsg :: String -> String -> String -> Net ()
replyMsg chan' nick' msg
    | chan' == nick'  = write "PRIVMSG" (nick' ++ " :" ++ msg) -- PM.
    | otherwise       = write "PRIVMSG" (chan' ++ " :" ++ nick' ++ ": " ++ msg)

-- Send a private message.
privMsg :: String -> String -> Net ()
privMsg nick' msg = write "PRIVMSG" (nick' ++ " :" ++ msg)

-- Send a message out to the server we're currently connected to.
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

-- Convenience.
io :: IO a -> Net a
io = liftIO

-- Strip characters from string.
strip :: Eq a => a -> [a] -> [a]
strip _ [] = []
strip a (x:xs)
    | x == a    = strip a xs
    | otherwise = x : strip a xs

-- Replace items in list.
replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace a b (x:xs)
    | x == a    = b : replace a b xs
    | otherwise = x : replace a b xs

-- Join list items together if quoted.
joinWords :: Char -> [String] -> [String]
joinWords _ [] = []
joinWords a (x:xs)
    | (head x) == a   = unwords (x : (take num xs)) : joinWords a (drop num xs)
    | otherwise       = x : joinWords a xs
  where num = (fromMaybe 0 (elemIndex a $ map last xs)) + 1

-- Fix search words for Wordnet.
wnFixWord :: String -> String
wnFixWord = strip '"' . replace ' ' '_'

-- Try to determine most common POS for word.
wnPartString :: String -> Net String
wnPartString a = do
    w <- asks wne
    ind1 <- io $ indexLookup w a Noun
    ind2 <- io $ indexLookup w a Verb
    ind3 <- io $ indexLookup w a Adj
    ind4 <- io $ indexLookup w a Adv
    return (type' ((count' ind1) : (count' ind2) : (count' ind3) : (count' ind4) : []))
  where
    count' a = if isJust a then senseCount (fromJust a) else 0
    type' [] = "Other"
    type' a
      | fromMaybe (-1) (elemIndex (maximum a) a) == 0 = "Noun"
      | fromMaybe (-1) (elemIndex (maximum a) a) == 1 = "Verb"
      | fromMaybe (-1) (elemIndex (maximum a) a) == 2 = "Adj"
      | fromMaybe (-1) (elemIndex (maximum a) a) == 3 = "Adv"
      | otherwise                                     = "Other"

-- Try to determine most common POS for word.
wnPartPOS :: String -> Net POS
wnPartPOS a = do
    w <- asks wne
    ind1 <- io $ indexLookup w a Noun
    ind2 <- io $ indexLookup w a Verb
    ind3 <- io $ indexLookup w a Adj
    ind4 <- io $ indexLookup w a Adv
    return (type' ((count' ind1) : (count' ind2) : (count' ind3) : (count' ind4) : []))
  where
    count' a = if isJust a then senseCount (fromJust a) else 0
    type' [] = Adj
    type' a
      | fromMaybe (-1) (elemIndex (maximum a) a) == 0 = Noun
      | fromMaybe (-1) (elemIndex (maximum a) a) == 1 = Verb
      | fromMaybe (-1) (elemIndex (maximum a) a) == 2 = Adj
      | fromMaybe (-1) (elemIndex (maximum a) a) == 3 = Adv
      | otherwise                                     = Adj

-- Wordnet search.
wnRelated :: String -> String -> String -> String -> String -> Net ()
wnRelated a b [] _ _  = return () :: Net ()
wnRelated a b c  d [] = do
    wnPos <- wnPartString (wnFixWord c) -- POS not given so use most common.
    wnRelated a b c d wnPos
wnRelated a b c [] _  = wnRelated a b c "Hypernym" []
wnRelated a b c d  e  = do
    w <- asks wne
    m <- asks maxchanlines
    mm <- io $ readIORef m
    let wnForm = readForm d
    let wnPos = fromEPOS $ readEPOS e
    let result = fromMaybe [[]] (runs w (relatedByList wnForm (search (wnFixWord c) wnPos AllSenses)))
    if (null result) || (null $ concat result) then return "Nothing!" >>= replyMsg a b else
      if (length result) > mm then wnRelated' b b result else wnRelated' a b result -- Redirect reply to prevent channel spam.
  where
    wnRelated' _ _ []     = return ()
    wnRelated' a b (x:xs) = do
      if (null x) then return () -- Redundant?
      else return (replace '_' ' ' $ unwords $ map (++ "\"") $ map ('"' :) $ concat $ map (getWords . getSynset) x) >>= replyMsg a b
      wnRelated' a b xs

-- Wordnet search.
wnClosure :: String -> String -> String -> String -> String -> Net ()
wnClosure a b [] _ _  = return () :: Net ()
wnClosure a b c  d [] = do
    wnPos <- wnPartString (wnFixWord c) -- POS not given so use most common.
    wnClosure a b c d wnPos
wnClosure a b c [] _  = wnClosure a b c "Hypernym" []
wnClosure a b c d  e  = do
    w <- asks wne
    m <- asks maxchanlines
    mm <- io $ readIORef m
    let wnForm = readForm d
    let wnPos = fromEPOS $ readEPOS e
    let result = runs w (closureOnList wnForm (search (wnFixWord c) wnPos AllSenses)) -- [Maybe (Tree SearchResult)]
    if (null result) then return "Nothing!" >>= replyMsg a b else
      if (length result) > mm then wnClosure' 0 b b result else wnClosure' 0 a b result -- Redirect reply to prevent channel spam.
  where
    wnClosure' _  _ _ []     = return ()
    wnClosure' 20 _ _ _      = return () -- "20" here is a recursion limit (just in case).
    wnClosure' a  b c (x:xs) = do
      if isNothing x then return ()
      else return (replace '_' ' ' $ unwords $ map (++ "\"") $ map ('"' :) $ nub $ concat $ map (getWords . getSynset)
             (flatten (fromJust x))) >>= replyMsg b c
      wnClosure' (a+1) b c xs

-- Wordnet search.
wnGloss :: String -> String -> String -> String -> Net ()
wnGloss _ _ [] _ = return () :: Net ()
wnGloss a b c [] = do
    wnPos <- wnPartString (wnFixWord c) -- POS not given so use most common.
    wnGloss a b c wnPos
wnGloss a b c d = do
    w <- asks wne
    m <- asks maxchanlines
    mm <- io $ readIORef m
    let wnPos = fromEPOS $ readEPOS d
    let result = map (getGloss . getSynset) (runs w (search (wnFixWord c) wnPos AllSenses))
    if (null result) then return "Nothing!" >>= replyMsg a b else
      if (length result) > mm then wnGloss' b b result else wnGloss' a b result -- Redirect reply to prevent channel spam.
  where
    wnGloss' _ _ []     = return ()
    wnGloss' a b (x:xs) = do
      return x >>= replyMsg a b
      wnGloss' a b xs

-- Wordnet search.
wnMeet :: String -> String -> String -> String -> String -> Net ()
wnMeet _ _ [] _ _ = return () :: Net ()
wnMeet _ _ _ [] _ = return () :: Net ()
wnMeet a b c d [] = do
    wnPos <- wnPartString (wnFixWord c) -- POS not given so use most common.
    wnMeet a b c d wnPos
wnMeet a b c d e  = do
    w <- asks wne
    let wnPos = fromEPOS $ readEPOS e
    let result = runs w (meet emptyQueue (head $ search (wnFixWord c) wnPos 1) (head $ search (wnFixWord d) wnPos 1))
    if (isNothing result) then return "Nothing!" >>= replyMsg a b else
      return (replace '_' ' ' $ unwords $ map (++ "\"") $ map ('"' :) $ getWords $ getSynset (fromJust result)) >>= replyMsg a b
