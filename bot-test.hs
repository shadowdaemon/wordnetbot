import Data.List
import Network
import System.CPUTime
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

--fooTest1 :: String -> Net ()
--fooTest1 a = io (reverse a)

wndir  = "/usr/share/wordnet/dict/"
server = "irc.freenode.org"
port   = 6667
chan   = "#lolbots"
nick   = "jesusbot"

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle }

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = catchIOError (runReaderT run st) (const $ return ())
    --loop st      = catch (runReaderT run st) (\(ExitException _) -> return ()) -- *** Control.Exception with base-4

catchIOError :: IO a -> (IOError -> IO a) -> IO a
catchIOError = catch

-- Connect to the server and return the initial bot state
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
 
-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :user")
    write "JOIN" chan
    asks socket >>= listen
 
-- Process each line from the server (this needs flood prevention somewhere)
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else eval (clean s)
  where
    forever a = a >> forever a
    --clean     = drop 1 . dropWhile (/= ':') . drop 1
    --clean2 a  = drop 1 (dropWhile (/= ':') (drop 1 a))
    --clean3 a  = drop 1 $ dropWhile (/= ':') $ drop 1 a
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)

clean :: String -> String
clean a
    | length (intersect (words a) [chan]) > 0 = drop 1 $ dropWhile (/= ':') $ drop 1 a
    | otherwise = ""

-- Dispatch a command
eval :: String -> Net ()
eval     ""                    = return ()
eval     "!quit"               = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
--eval   a@"!wnSearch"           = privmsg (wnSearch2 "hag" Noun AllSenses)
--eval   a@"!wnSearch"           = wnSearch2 "hag" Noun AllSenses >>= privmsg
eval     "lol"                 = privmsg "lol"
eval     "Jesus"               = privmsg "Jesus!"
eval     a@_                   = privmsg $ reverse a
--eval     _                     = return () -- ignore everything else

-- Send a privmsg to the current chan + server
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)
 
-- Send a message out to the server we're currently connected to
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
