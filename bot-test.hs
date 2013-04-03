import Data.List
import Data.Maybe
import Data.Tree (flatten)
import Network
--import System.CPUTime
import System.Directory
import System.IO
import System.IO.Error
import System.Exit
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Text.Printf
--import Text.Regex.TDFA
import Prelude

--import NLP.Nerf
import NLP.WordNet
import NLP.WordNet.Prims (indexLookup, senseCount, getSynset, getWords, getGloss)
import NLP.WordNet.PrimTypes

wndir  = "/usr/share/wordnet/dict/"
server = "irc.freenode.org"
port   = 6667
chan   = "#lolbots"
nick   = "wordnetbot"
owner  = "shadowdaemon"

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle, wne :: WordNetEnv }

-- Set up actions to run on start and end, and run the main loop.
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = do hClose . socket ; closeWordNet . wne
    loop st    = catchIOError (runReaderT run st) (const $ return ())

-- Connect to the server and return the initial bot state.  Initialize WordNet.
connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    w <- initializeWordNetWithOptions (return wndir :: Maybe FilePath) 
      (Just (\e f -> putStrLn (e ++ show (f :: SomeException))))
    hSetBuffering h NoBuffering
    return (Bot h w)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a

-- We're in the Net monad now, so we've connected successfully.
-- Join a channel, and start processing commands.
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :user")
    write "JOIN" chan
    asks socket >>= listen

-- Process each line from the server (this needs flood prevention somewhere).
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else processLine (words s)
  where
    forever a = a >> forever a
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)

-- Get the actual message.
getMsg :: [String] -> [String]
getMsg a
    | (head $ drop 1 a) == "PRIVMSG" = (drop 1 (a!!3)) : (drop 4 a)
    | otherwise = []

-- Who is speaking to us?
getNick :: [String] -> String
getNick = drop 1 . takeWhile (/= '!') . head

-- Which channel is message coming from?  Also could be private message.
getChannel :: [String] -> String
getChannel = head . drop 2

-- Are we being spoken to?
spokenTo :: [String] -> Bool
spokenTo []              = False
spokenTo a
    | b == nick          = True
    | b == (nick ++ ":") = True
    | otherwise          = False
  where
    b = (head a)

-- Is this a private message?
isPM :: [String] -> Bool
isPM a
    | getChannel a == nick = True
    | otherwise            = False

-- Process IRC line.
processLine :: [String] -> Net ()
processLine a
    | length a == 0     = return ()
    | length msg' == 0  = return () -- Ignore because not PRIVMSG.
    | chan' == nick     = reply [] who' msg' -- Respond to PM.
    -- | chan' == nick     = if (head $ head msg') == '!' then evalCmd chan' who' msg' -- Evaluate command (broken).
    --                       else reply [] who' msg' -- Respond to PM.
    | spokenTo msg'     = if (head $ head $ tail msg') == '!' then evalCmd chan' who' (tail msg') -- Evaluate command.
                          else reply chan' who' (tail msg') -- Respond upon being addressed.
    | otherwise         = return ()
    -- | otherwise         = processMsg chan' who' msg' -- Process message.
    -- | otherwise         = reply chan' [] msg' -- Testing.
  where
    msg' = getMsg a
    who' = getNick a
    chan' = getChannel a

-- Reply to message.
reply :: String -> String -> [String] -> Net ()
reply [] who' msg = privMsg who' "Eh?" -- PM.
reply chan' [] msg  = chanMsg chan' $ reverse $ unwords msg -- Cheesy reverse gimmick, for testing.  Channel talk.
reply chan' who' msg = replyMsg chan' who' $ reverse $ unwords msg -- Reply in channel.

-- Process messages.
--processMsg :: String -> String -> [String] -> ReaderT Bot IO ()
--processMsg chan' who' msg' =

-- Evaluate commands.
evalCmd :: String -> String -> [String] -> Net ()
evalCmd _ b (x:xs) | x == "!quit"    = if b == owner then write "QUIT" ":Exiting" >> io (exitWith ExitSuccess) else return ()
evalCmd a b (x:xs) | x == "!related" =
    case (length xs) of
      3 -> wnRelated a b (xs!!0) (xs!!1) (xs!!2)
      2 -> wnRelated a b (xs!!0) (xs!!1) []
      1 -> wnRelated a b (xs!!0) []      []
      _ -> replyMsg a b "Usage: !related word [form] [part-of-speech]"
evalCmd a b (x:xs) | x == "!closure" =
    case (length xs) of
      3 -> wnClosure (xs!!0) (xs!!1) (xs!!2) >>= replyMsg a b
      2 -> wnClosure (xs!!0) (xs!!1) []      >>= replyMsg a b
      1 -> wnClosure (xs!!0) []      []      >>= replyMsg a b
      _ -> replyMsg a b "Usage: !closure word [form] [part-of-speech]"
evalCmd a b (x:xs) | x == "!gloss"   =
    case (length xs) of
      2 -> wnGloss a b (xs!!0) (xs!!1)
      1 -> wnGloss a b (xs!!0) []
      _ -> replyMsg a b "Usage: !gloss word [part-of-speech]"
evalCmd a b (x:xs) | x == "!forms"      = replyMsg a b (init (concat $ map (++ " ") $ map show allForm))
evalCmd a b (x:xs) | x == "!parts"      = replyMsg a b (init (concat $ map (++ " ") $ map show allPOS))
evalCmd a b (x:xs) | x == "!help"       = replyMsg a b "Commands: !related !closure !gloss !forms !parts !quit"
evalCmd _ _ _                           = return ()

-- Send a message to the channel.
chanMsg :: String -> String -> Net ()
chanMsg chan' msg = write "PRIVMSG" (chan' ++ " :" ++ msg)

-- Send a reply message.
replyMsg :: String -> String -> String -> Net ()
replyMsg chan' nick' msg = write "PRIVMSG" (chan' ++ " :" ++ nick' ++ ": " ++ msg)

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

-- Replace items in list.
replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace a b (x:xs)
    | x == a    = b : replace a b xs
    | otherwise = x : replace a b xs

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
      | fromJust (elemIndex (maximum a) a) == 0 = "Noun"
      | fromJust (elemIndex (maximum a) a) == 1 = "Verb"
      | fromJust (elemIndex (maximum a) a) == 2 = "Adj"
      | fromJust (elemIndex (maximum a) a) == 3 = "Adv"
      | otherwise                               = "Other"

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
      | fromJust (elemIndex (maximum a) a) == 0 = Noun
      | fromJust (elemIndex (maximum a) a) == 1 = Verb
      | fromJust (elemIndex (maximum a) a) == 2 = Adj
      | fromJust (elemIndex (maximum a) a) == 3 = Adv
      | otherwise                               = Adj

-- Wordnet search.
wnRelated :: String -> String -> String -> String -> String -> Net ()
wnRelated a b [] _ _  = return () :: Net ()
wnRelated a b c  d [] = do
    wnPos <- wnPartString a -- POS not given so use most common.
    wnRelated a b c d wnPos
wnRelated a b c [] _  = wnRelated a b c "Hypernym" []
wnRelated a b c d  e  = do
    h <- asks socket
    w <- asks wne
    let wnForm = readForm d
    let wnPos = fromEPOS $ readEPOS e
    let result = fromMaybe [[]] (runs w (relatedByList wnForm (search c wnPos AllSenses)))
    let l = length result
    if (null result) || (null $ concat result) then return "Nothing!" >>= replyMsg a b else wnRelated' a b l l result
  where
    wnRelated' _ _ _ _ [] = return ()
    wnRelated' a b c d (x:xs) = do
      if (null x) then return ()
      else return (replace '_' ' ' $ unwords $ map (++ "\"") $ map ('"' :) $ concat $ map (getWords . getSynset) x) >>= replyMsg a b
      wnRelated' a b (c-1) d xs

-- Wordnet search.
wnClosure :: String -> String -> String -> Net String
wnClosure [] _ _  = return [] :: Net String
wnClosure a  b [] = do
    wnPos <- wnPartString a -- POS not given so use most common.
    wnClosure a b wnPos
wnClosure a [] _  = wnClosure a "Hypernym" []
wnClosure a b c = do
    h <- asks socket
    w <- asks wne
    let wnForm = readForm b
    let wnPos = fromEPOS $ readEPOS c
    -- let result = replace '_' ' ' $ unwords $ map (++ "\"") $ map ('"' :) $ nub $ concat $ map (getWords . getSynset)
    --                (concat (take 10 (map flatten (fromMaybe [] (runs w (closureOnList wnForm (search a wnPos AllSenses)))))))
    let result = replace '_' ' ' $ unwords $ map (++ "\"") $ map ('"' :) $ nub $ concat $ map (getWords . getSynset)
                   (take 10 (flatten (runs w (closureOn wnForm (head (search a wnPos 1))))))
    if (null result) then return "Nothing!" else return result

-- Wordnet search.
wnGloss :: String -> String -> String -> String -> Net ()
wnGloss _ _ [] _ = return () :: Net ()
wnGloss a b c [] = do
    wnPos <- wnPartString a -- POS not given so use most common.
    wnGloss a b c wnPos
wnGloss a b c d = do
    h <- asks socket
    w <- asks wne
    let wnPos = fromEPOS $ readEPOS d
    let result = map (getGloss . getSynset) (runs w (search c wnPos AllSenses))
    let l = length result
    if (null result) then return "Nothing!" >>= chanMsg chan else wnGloss' a b l l result
  where
    wnGloss' _ _ _ _ [] = return ()
    wnGloss' a b c d (x:xs) = do
      return x >>= replyMsg a b
      wnGloss' a b (c-1) d xs
