 {-# LANGUAGE OverloadedStrings #-}

import              Control.Arrow
import              Control.Exception
import              Control.Monad.Reader
import              Data.List
import qualified    Data.Vector as V
import              Data.Maybe
import qualified    Data.Text as P
import qualified    Data.ByteString.Char8 as CHAR8
import qualified    Data.ByteString.Lazy.Char8 as LCHAR8
import              Network
import              Network.HTTP.Client
import              Network.HTTP.Client.TLS
import              Network.HTTP.Types.Status (statusCode)
import              Data.Aeson (object, (.=), encode)
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

-- urlSortener :: TODO

main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect  = hClose . socket
    loop st     = runReaderT run st

connect :: IO Bot
connect = notify $ do
    irc_conn <- connectTo server $ PortNumber $ fromIntegral Main.port
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
eval sender target "!m" = do
    text <- io (nextMileStone "TokTok" "Maybe target milestone")
    privMsg target text
eval sender target "!status iphy" = do
    privMsg target "iphy's current status :: https://img.shields.io/badge/iphy-savage-red.svg"
eval sender target msg
    | "!echo " `isPrefixOf` msg = privMsg target $ drop 6 msg
    | msg =~ regex = do
        url <- io $ checkIssue msg
        if isJust url
            then privMsg target $ fromJust url
            else return ()
    | otherwise = return ()

privMsg :: String -> String -> Net ()
privMsg to text = write "PRIVMSG" $ to ++ " :" ++ text

githubToIRC :: String -> String
githubToIRC x = x

githubShort :: String -> String -> String -> IO (Maybe String)
githubShort url pre str = do
    manager <- newManager tlsManagerSettings
    let requestText = [  ("url", CHAR8.pack url)
                      ,  ("code", CHAR8.pack (pre ++ str))
                      ]
    initRequest <- parseRequest "https://git.io"
    let request = urlEncodedBody requestText $ initRequest { method = "POST" }
    response <- httpLbs request manager
    putStrLn $ LCHAR8.unpack (responseBody response)
    putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)

    let headers = responseHeaders response
    let res = (lookup "location" headers)
    if isJust res
        then return (Just $ CHAR8.unpack $ fromJust res)
        else return (Nothing)


nextMileStone :: String -> String -> IO (String)
nextMileStone pre _ = do
    -- get from github
    str <- githubShort "https://github.com/TokTok/c-toxcore/milestone/11" pre "-v0.0.5"
    if isJust str
        then return $ fromJust str
        else return "Error getting milestone... Sorry about that"

parseAssigned :: G.Issue -> IO [String]
parseAssigned issue = do
    let assigned = V.toList $ G.issueAssignees issue
    let names = map (githubToIRC . P.unpack . G.untagName . G.simpleUserLogin) assigned
    return (names)

checkIssue :: String -> IO (Maybe String)
checkIssue msg = do
    let tag = msg =~ regex -- Find supported tags
    let repo_name = (takeWhile (/= '#') tag)
    let issu_numb = read (drop 1 (dropWhile (/= '#') tag))
    possibleIssue <- G.issue "TokTok" (G.mkRepoName (P.pack repo_name)) (G.Id issu_numb)
    case possibleIssue of
        Left  err -> return (Nothing)
        Right real_issue -> do
            users <- parseAssigned real_issue
            let user  = (intercalate " " users)
            let url   = (P.unpack . G.getUrl $ fromJust $ G.issueHtmlUrl real_issue)
            let title = (P.unpack $ G.issueTitle real_issue)
            let str  = url ++ " " ++ title ++ " (Assigned to: " ++ user ++ ")"
            return (Just str)

io :: IO a -> Net a
io = liftIO
