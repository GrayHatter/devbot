 {-# LANGUAGE OverloadedStrings #-}

import              Control.Arrow
import              Control.Exception
import              Control.Monad.Reader
import              Data.List
import qualified    Data.Text as P
import              Network
import              System.Exit
import              System.IO
import              Text.Printf
import              Text.Regex.Posix

import              GitHub.Data.Name as G
import              GitHub.Data.Id as G
import qualified    GitHub.Endpoints.Issues as G
-- import qualified    GitHub.Issues as GI


ournick = "devbot"
server  = "irc.freenode.org"
port    = 6667
chans   =   [   "#toktok"
            ,   "#toktok-status"
            ]

enabled_repos = [   "toxcore"
                ,   "py-toxcore-c"
                ,   "hs-toxcore"
                ,   "website"
                ]

regex = "(" ++ (intercalate "|" enabled_repos) ++ ")#([0-9]+)"

data Bot = Bot {
    socket :: Handle
}

type Net = ReaderT Bot IO

data User = User
    {   nick :: String
    ,   user :: String
    ,   host :: String
    }

main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect  = hClose . socket
    loop st     = runReaderT run st

connect :: IO Bot
connect = notify $ do
    irc_conn <- connectTo server $ PortNumber $ fromIntegral port
    hSetBuffering irc_conn NoBuffering
    return (Bot irc_conn)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a

run :: Net ()
run = do
    write "NICK" ournick
    write "USER" $ ournick++" 0 * :TokTok DevBot"
    mapM (write "JOIN") chans
    asks socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
    string <- init `fmap` io (hGetLine h)
    io $ putStrLn string
    if ping string
        then pong string
        else eval (sender string) (target string) (message string)
  where
    forever a = do a; forever a

    sender  = takeWhile (/= ' ') . drop 1
    target  = takeWhile (/= ' ') . dropWhile (/= '#')
    message = drop 1 . dropWhile (/= ':') . drop 1

    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" $ ':' : drop 6 x

write :: String -> String -> Net ()
write string text = do
    io $ printf    "> %s %s\n" string text
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" string text

eval :: String -> String -> String -> Net ()
eval sender target "!die" = do
    privMsg target "Sure, I'll just DIE then!"
    write "QUIT" ":My death was ordered" >> io (exitWith ExitSuccess)
eval sender target msg
    | "!echo " `isPrefixOf` msg = privMsg target $ drop 6 msg
    | msg =~ regex = do
        url <- io $ checkIssue msg
        privMsg target url
    | otherwise = return ()

privMsg :: String -> String -> Net ()
privMsg to text = write "PRIVMSG" $ to ++ " :" ++ text

checkIssue :: String -> IO String
checkIssue msg = do
    let tag = msg =~ regex -- Find supported tags
    let repo_name = (takeWhile (/= '#') tag)
    let issu_numb = read (drop 1 (dropWhile (/= '#') tag))
    possibleIssue <- G.issue "TokTok" (G.mkRepoName (P.pack repo_name)) (G.Id issu_numb)
    case possibleIssue of
        Left  err -> return (show err)
        Right url -> return (show $ G.issueHtmlUrl url)

io :: IO a -> Net a
io = liftIO
