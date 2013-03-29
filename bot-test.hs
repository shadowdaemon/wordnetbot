import Data.List
import Network
--import System.CPUTime
import System.IO
import System.Exit
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
--import Control.OldException
import Text.Printf
import Prelude hiding (catch)

--import NLP.Nerf
import NLP.WordNet

-- Test
--foooo = NLP.Nerf.train 3 "foo" "bar" "baz"
{-fooo :: IO WordNetEnv
fooo = do
    initializeWordNetWithOptions (wndir "/usr/share/wordnet/dict/") Nothing
    runs $ searchByOverview (getOverview "dog") Noun AllSenses
-}

wnSearch1 :: String -> POS -> SenseType -> IO [SearchResult]
wnSearch1 a b c = runWordNetWithOptions (tryDir wndir) (Just (\_ _ -> return ())) (search a b c)

wnSearch2 :: String -> POS -> SenseType -> Net [SearchResult]
wnSearch2 a b c = io $ runWordNetWithOptions (tryDir wndir) (Just (\_ _ -> return ())) (search a b c)

wnOverview :: String -> IO Overview
wnOverview a = runWordNetWithOptions (tryDir wndir) (Just (\_ _ -> return ())) (getOverview a)

wndir  = "/usr/share/wordnet/dict/"
server = "irc.freenode.org"
port   = 6667
chan   = "#lolbots"
nick   = "jesusbot"
owner  = "shadowdaemon"

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle }

-- Set up actions to run on start and end, and run the main loop.
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = catchIOError (runReaderT run st) (const $ return ())
    --loop st      = catch (runReaderT run st) (\(ExitException _) -> return ()) -- *** Control.Exception with base-4

catchIOError :: IO a -> (IOError -> IO a) -> IO a
catchIOError = catch

-- Connect to the server and return the initial bot state.
connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h)
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
    if ping s then pong s else eval (words s)
  where
    forever a = a >> forever a
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)

-- Get the actual message.
getMsg :: [String] -> [String]
getMsg a
    | length (intersect a ["PRIVMSG"]) > 0 = b
    | otherwise = []
  where
    b = (drop 1 (a!!3)) : (drop 4 a)

-- Are we being spoken to?
spokenTo :: [String] -> Bool
spokenTo []              = False
spokenTo a
    | b == nick          = True
    | b == (nick ++ ":") = True
    | otherwise          = False
  where
    b = (head a)

--replyMsg

-- Is our owner speaking to us?
--spokenToOwner

-- Is this a private message?
--spokenPrivate

--replyPrivate

-- Figure shit out.
eval :: [String] -> Net ()
eval a
    | length b == 0      = return ()
    | spokenTo b == True = evalMsg $ tail b
    | otherwise          = return ()
  where
    b = getMsg a

-- Dispatch a command.
evalMsg :: [String] -> ReaderT Bot IO ()
evalMsg   []                           = return () -- Ignore because not PRIVMSG.
evalMsg   ["!quit"]                    = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
evalMsg x | "!id " `isPrefixOf` (x!!0) = chanMsg (drop 4 (x!!0))
--evalMsg   a@"!wnSearch"           = privmsg (wnSearch2 "hag" Noun AllSenses)
--evalMsg   a@"!wnSearch"           = wnSearch2 "hag" Noun AllSenses >>= privmsg
evalMsg   ["Jesus"]                    = chanMsg "Jesus!"
evalMsg     a@_                        = chanMsg $ reverse $ unwords a

-- Send a privmsg to the current chan + server.
chanMsg :: String -> Net ()
chanMsg s = write "PRIVMSG" (chan ++ " :" ++ s)

replyMsg :: String -> String -> Net ()
replyMsg s nick = write "PRIVMSG" (chan ++ " :" ++ s)

privMsg :: String -> String -> Net ()
privMsg s nick = write "PRIVMSG" (chan ++ " :" ++ s)

-- Send a message out to the server we're currently connected to.
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t
 
-- Convenience.
io :: IO a -> Net a
io = liftIO

tryDir :: FilePath -> Maybe FilePath
tryDir a
    | length a > 0 = Just a
    | otherwise    = Nothing
