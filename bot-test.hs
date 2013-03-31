import Data.List
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
import Prelude

--import NLP.Nerf
import NLP.WordNet
import NLP.WordNet.Prims (getIndexString, indexLookup, indexToSenseKey, getSynsetForSense)

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
    | chan' == nick     = if (head $ head msg') == '!' then evalCmd chan' who' msg' -- Evaluate command.
                          else reply [] who' msg' -- Respond to PM.
    | spokenTo msg'     = if (head $ head $ tail msg') == '!' then evalCmd chan' who' (tail msg') -- Evaluate command.
                          else reply chan' who' (tail msg') -- Respond upon being addressed.
--    | otherwise         = processMsg chan' who' msg' -- Process message.
    | otherwise         = reply chan' [] msg' -- Testing.
  where
    msg' = getMsg a
    who' = getNick a
    chan' = getChannel a

-- Reply to message.
reply :: String -> String -> [String] -> ReaderT Bot IO ()
reply [] who' msg = privMsg who' $ reverse $ unwords msg
reply chan' [] msg  = chanMsg chan' $ reverse $ unwords msg
reply chan' who' msg = replyMsg chan' who' $ reverse $ unwords msg -- Cheesy reverse gimmick, for testing.

-- Process messages.
--processMsg :: String -> String -> [String] -> ReaderT Bot IO ()
--processMsg chan' who' msg' =

-- Evaluate commands.
evalCmd :: String -> String -> [String] -> ReaderT Bot IO ()
evalCmd _ b (x:xs) | x == "!quit"       = if b == owner then write "QUIT" ":Exiting" >> io (exitWith ExitSuccess) else return ()
evalCmd _ _ (x:xs) | x == "!search"     = wnSearch (head xs)
evalCmd _ _ (x:xs) | x == "!overview"   = wnOverview (head xs)
evalCmd _ _ (x:xs) | x == "!type"       = wnWordType (head xs)
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

wnSearch :: String -> ReaderT Bot IO b
wnSearch a = do
    h <- asks socket
    w <- asks wne
    result1 <- io $ return $ runs w (search a Noun AllSenses)
    result2 <- io $ return $ runs w (search a Verb AllSenses)
    result3 <- io $ return $ runs w (search a Adj AllSenses)
    result4 <- io $ return $ runs w (search a Adv AllSenses)
    io $ hPrintf h "PRIVMSG %s :Noun -> " chan; io $ hPrint h result1; io $ hPrintf h "\r\n"
    io $ hPrintf h "PRIVMSG %s :Verb -> " chan; io $ hPrint h result2; io $ hPrintf h "\r\n"
    io $ hPrintf h "PRIVMSG %s :Adj -> " chan; io $ hPrint h result3; io $ hPrintf h "\r\n"
    io $ hPrintf h "PRIVMSG %s :Adv -> " chan; io $ hPrint h result4; io $ hPrintf h "\r\n"

wnOverview a = do
    h <- asks socket
    w <- asks wne
    result <- io $ return $ runs w (getOverview a)
    io $ hPrintf h "PRIVMSG %s :" chan; io $ hPrint h result; io $ hPrintf h "\r\n"

wnWordType a = do
    h <- asks socket
    w <- asks wne
    result1 <- io $ (getIndexString w a Noun)
    result2 <- io $ (getIndexString w a Verb)
    result3 <- io $ (getIndexString w a Adj)
    result4 <- io $ (getIndexString w a Adv)
    io $ hPrintf h "PRIVMSG %s :Noun -> " chan; io $ hPrint h result1; io $ hPrintf h "\r\n"
    io $ hPrintf h "PRIVMSG %s :Verb -> " chan; io $ hPrint h result2; io $ hPrintf h "\r\n"
    io $ hPrintf h "PRIVMSG %s :Adj -> " chan; io $ hPrint h result3; io $ hPrintf h "\r\n"
    io $ hPrintf h "PRIVMSG %s :Adv -> " chan; io $ hPrint h result4; io $ hPrintf h "\r\n"

--wnSenseKey a = do

{-
tryDir :: FilePath -> Maybe FilePath
tryDir a
    | length a > 0 = Just a
    | otherwise    = Nothing

tryDir2 :: FilePath -> IO (Maybe FilePath)
tryDir2 a = do
    dir <- doesDirectoryExist a
--    if dir then do return (Just a) else do return (Nothing)
    do if dir then return (Just a) else return (Nothing)
--tryDir3 a = do b <- tryDir2 a ; return b

foo a = do
    w <- initializeWordNetWithOptions (return wndir :: Maybe FilePath)
      (Just (\e f -> putStrLn (e ++ show (f :: SomeException))))
    return $ runs w (search a Noun AllSenses)

foo2 :: String -> ReaderT Bot IO [SearchResult]
foo2 a = do
    w <- asks wne
    return $ runs w (search a Noun AllSenses)

tryIOError :: IO a -> IO (Either IOError a)
tryIOError = try

return "WHAT" :: Maybe String

wnOverview :: String -> IO Overview
wnOverview a = runWordNetWithOptions (tryDir wndir) (Just (\_ _ -> return ())) (getOverview a)
-}
