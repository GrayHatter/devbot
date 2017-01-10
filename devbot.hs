{-# LANGUAGE OverloadedStrings #-}

-- import              Control.Arrow
import           Control.Exception
import           Control.Monad.Reader
import qualified Data.ByteString.Char8              as CHAR8
import qualified Data.ByteString.Lazy.Char8         as LCHAR8
import           Data.List
import           Data.Maybe
import qualified Data.Text                          as P
import qualified Data.Vector                        as V
import           Network
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status          (statusCode)
import           Text.Printf
-- import              Data.Aeson (object, (.=), encode)
import           System.Exit
import           System.IO
import           Text.Printf
import           Text.Regex.Posix

import           GitHub.Data.Id                     as G
import           GitHub.Data.Milestone              as G
import           GitHub.Data.Name                   as G
import           GitHub.Data.URL                    as G
import qualified GitHub.Endpoints.Issues            as G
import qualified GitHub.Endpoints.Issues.Milestones as G
-- import qualified    GitHub.Issues as GI


ournick = "devbot"
server  = "irc.freenode.org"
port    = 6667
chans   =   [   "#toktok"
            ,   "#toktok-status"
            ,   "#utox"
            ]

enabled_repos = [   "apidsl"
                ,   "c-toxcore-hs"
                ,   "dockerfiles"
                ,   "github-tools"
                ,   "hs-msgpack"
                ,   "hs-msgpack-rpc"
                ,   "hs-schema"
                ,   "hs-toxcore"
                ,   "hs-toxcore-c"
                ,   "js-toxcore-hs"
                ,   "jvm-linters"
                ,   "jvm-macros"
                ,   "jvm-sbt-plugins"
                ,   "jvm-toxcore-api"
                ,   "jvm-toxcore-c"
                ,   "py-toxcore-c"
                ,   "qTox"
                ,   "semdoc"
                ,   "spec"
                ,   "tokstyle"
                ,   "toktok-android"
                ,   "toktok-stack"
                ,   "toktok.github.io"
                ,   "toxcore"
                ,   "utox"
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

------------------------------------------------
--  IRC connection and logic
------------------------------------------------
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
        else eval (source string) (target string) (message string)
  where
    forever a = do a; forever a

    source  = takeWhile (/= ' ') . drop 1
    target  = takeWhile (/= ' ') . dropWhile (/= '#')
    message = drop 1 . dropWhile (/= ':') . drop 1

    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" $ ':' : drop 6 x

write :: String -> String -> Net ()
write string text = do
    io $ printf    "> %s %s\n" string text
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" string text

------------------------------------------------
--  Main processor
------------------------------------------------
eval :: String -> String -> String -> Net ()
eval source target "!die" = do
    privMsg target "Sure, I'll just DIE then!"
    write "QUIT" ":My death was ordered" >> io (exitWith ExitSuccess)
eval source target "!m" = do
    text <- io $ nextMilestone "TokTok" "c-toxcore"
    privMsg target $ text
eval source target "!status iphy" = do
    privMsg target "iphy's current status :: https://img.shields.io/badge/iphy-savage-red.svg"
eval source target msg
    -- Memes and time wasters
    | "devbot" `isPrefixOf` msg && "get to work" `isInfixOf` msg = privMsg target "Sir, yes sir!!!"
    -- Actual work
    | "!echo " `isPrefixOf` msg = privMsg target $ drop 6 msg
    | msg =~ regex = do
        url <- io $ checkIssue (takeWhile (/= '-') $ drop 1 target) msg
        if isJust url
            then privMsg target $ fromJust url
            else return ()
    | otherwise = return ()


privMsg :: String -> String -> Net ()
privMsg to text = write "PRIVMSG" $ to ++ " :" ++ text

------------------------------------------------
--  Milestones
------------------------------------------------
nextMilestone :: String -> String -> IO (String)
nextMilestone group repo = do
    list <- G.milestones (G.mkOwnerName $ P.pack group) (G.mkRepoName $ P.pack repo)
    case list of
        (Left err) -> return $ "Error: " ++ show err
        (Right milestones) -> parseMilestone (milestonesToNext (V.toList milestones)) group

milestonesToNext :: [G.Milestone] -> G.Milestone
milestonesToNext mList = do
    head . sortBy G.milestoneDueOn . filter (\x -> "open" == (G.milestoneState x)) $ mList

parseMilestone :: G.Milestone -> String -> IO (String)
parseMilestone miles group = do
    let m_tag = P.unpack (G.milestoneTitle miles)
    let name  = (group ++ "-" ++ m_tag)
    let url   = P.unpack $ G.getUrl $ G.milestoneHtmlUrl miles

    let open    = G.milestoneOpenIssues miles
    let closed  = G.milestoneClosedIssues miles
    let total   = open + closed
    let percent = (fromIntegral closed) / (fromIntegral total ) :: Float
    -- Try to make it small
    str <- githubMkShort url (name)
    return $ (printf "%f%% Complete (%d of %d) " (percent * 100) closed total :: String) ++ (fromMaybe (show url) str) ++ " || " ++ "https://reviewable.io/reviews#q=" ++ m_tag


------------------------------------------------
--  Issues
------------------------------------------------
parseAssigned :: G.Issue -> IO [String]
parseAssigned issue = do
    let assigned = V.toList $ G.issueAssignees issue
    let names = map (githubToIRC . P.unpack . G.untagName . G.simpleUserLogin) assigned
    return (names)

checkIssue :: String -> String -> IO (Maybe String)
checkIssue owner msg = do
    let tag = msg =~ regex -- Find supported tags
    let repo_name = (takeWhile (/= '#') tag)
    let issu_numb = read (drop 1 (dropWhile (/= '#') tag))
    possibleIssue <- G.issue (G.mkOwnerName (P.pack owner)) (G.mkRepoName (P.pack repo_name)) (G.Id issu_numb)
    case possibleIssue of
        Left  err -> return (Nothing)
        Right real_issue -> do
            users <- parseAssigned real_issue
            let user  = (intercalate " " users)
            let url   = (P.unpack . G.getUrl $ fromJust $ G.issueHtmlUrl real_issue)
            let title = (P.unpack $ G.issueTitle real_issue)
            let str  = url ++ " " ++ title ++ " (Assigned to: " ++ user ++ ") https://reviewable.io/reviews/" ++ repo_name ++ "/" ++ owner ++ "/" ++ show issu_numb
            return (Just str)


------------------------------------------------
--  GitHub helpers
------------------------------------------------
githubToIRC :: String -> String
githubToIRC x = x

githubMkShort :: String -> String -> IO (Maybe String)
githubMkShort url str = do
    manager <- newManager tlsManagerSettings
    let requestText = [  ("url", CHAR8.pack url)
                      ,  ("code", CHAR8.pack str)
                      ]
    initRequest <- parseRequest "https://git.io"
    let request = urlEncodedBody requestText $ initRequest { method = "POST" }
    response <- httpLbs request manager
    let code = statusCode $ responseStatus response
    putStrLn $ LCHAR8.unpack (responseBody response)
    putStrLn $ "The status code was: " ++ (show code)

    let headers = responseHeaders response
    let res = (lookup "location" headers)
    if isJust res
        then return (Just $ CHAR8.unpack $ fromJust res)
        else if code == 422
            then return (Just $ "https://git.io/" ++ str)
            else return (Nothing)

io :: IO a -> Net a
io = liftIO
