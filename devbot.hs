{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

-- import              Control.Arrow
import           Control.Arrow                      (first)
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson                         (FromJSON (..), ToJSON (..), Object, withObject, decode, encode, (.:))
import qualified Data.CaseInsensitive               as CI
import           Data.List
import           Data.Maybe
import           Data.Time.Clock
import           GHC.Generics                       (Generic)
import           Network
import           Network.HTTP.Simple
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status          (statusCode)
import           System.Exit
import           System.IO
import           Text.Printf
import           Text.Regex.Posix
import qualified Data.ByteString.Char8              as CHAR8
import qualified Data.ByteString.Lazy.Char8         as LCHAR8
import qualified Data.Text                          as P
import qualified Data.Vector                        as V

import           GitHub.Data.Id                     as G
import           GitHub.Data.Milestone              as G
import           GitHub.Data.Name                   as G
import           GitHub.Data.URL                    as G
import qualified GitHub.Endpoints.Issues            as G
import qualified GitHub.Endpoints.Issues.Milestones as G
-- import qualified    GitHub.Issues as GI


ournick =   "devbot"
server  =   "irc.freenode.org"
port    =   6667
chans   =   [   "#devbot-dev"
            ,   "#utox"
            ,   "#epic_cms"
            ]

nick_password = ""


regex_GH_num    = "(#[0-9]{1,5})" :: String
regex_GH_repo   = "([a-zA-Z0-9.-]+)"   ++ regex_GH_num
regex_GH_owner  = "([a-zA-Z0-9.-]+)/"  ++ regex_GH_repo
regex_gl        = "((^|\\s)![0-9]{1,5})" :: String
regex_gli       = "((^|\\s)i[0-9]{1,5})" :: String


data Bot = Bot
    {   socket      :: Handle
    ,   channels    :: [Channel]
    ,   start_time  :: UTCTime
    }

data Channel = Channel
    {   chname          :: String
    ,   users           :: [User]
    ,   default_rown    :: String
    ,   default_repo    :: String
    } deriving (Eq, Show)

type Net  = StateT Bot IO

data User = User
    {   nick :: String
    ,   user :: String
    ,   host :: String
    } deriving (Eq, Show)

------------------------------------------------
--  IRC connection and logic
------------------------------------------------
main :: IO ()
main = bracket conn disconnect loop
  where
    disconnect  = hClose . socket
    loop st     = evalStateT run st

conn :: IO Bot
conn = do
    irc_conn <- connectTo server $ PortNumber $ Main.port
    hSetBuffering irc_conn NoBuffering
    c <- getCurrentTime
    return (Bot irc_conn [] c)
    where
        notify a = bracket_ (printf "connecting to %s..." server >> hFlush stdout) (putStrLn "connected!") a

run :: Net ()
run = do
    write "NICK" ournick
    write "USER" $ ournick++" 0 * :uTox DevBot"
    write "PRIVMSG" ("NickServ :IDENTIFY devbot " ++ nick_password)
    listen

listen :: Net ()
listen = forever $ do
    h <- gets socket
    string <- init `fmap` io (hGetLine h)
    -- TODO sanitize utf-8 for the broken version of haskell on debian
    if "PING :" `isPrefixOf` string
        then io $ hPrintf h "PONG :%s\r\n" string
        else do io $ putStrLn string
                eval (source string) (action string) (target string) (message string)
  where
    forever a = do a; forever a

    source  = takeWhile (/= ' ') . drop 1
    action  = takeWhile (/= ' ') . drop 1 . dropWhile (/= ' ')
    target  = takeWhile (/= ' ') . drop 1 . dropWhile (/= ' ') . drop 1 . dropWhile (/= ' ')
    message = drop 1 . dropWhile (/= ':') . drop 1

write :: String -> String -> Net ()
write string text = do
    io $ printf    "> %s %s\n" string text
    h <- gets socket
    io $ hPrintf h "%s %s\r\n" string text

------------------------------------------------
--  Main processor
------------------------------------------------
eval :: String -> String -> String -> String -> Net ()
eval _ "332" _ _ = io $ putStrLn "DEBUG__ GOT CHAN TOPIC"
eval _ "333" _ _ = io $ putStrLn "DEBUG__ GOT CHAN TOPIC SETBY"
eval _ "353" _ _ = io $ putStrLn "DEBUG__ GOT USER LIST"
eval _ "366" _ _ = io $ putStrLn "DEBUG__ EOF USER LIST"
eval _ "473" _ _ = io $ putStrLn "DEBUG__ UNABLE TO JOIN CHANNEL (not invited)"
eval source action target "is now your hidden host (set by services.)" = do
    mapM joinChan chans
    return()
eval _ "INVITE" _ chan = do
    joinChan chan
eval source action target "!ms" = eval source action target "!milestone"
eval _ _ target "!interject" = do
    chNick  "Stallmon"
    privMsg target "I'd just like to interject for a moment. What you're referring to as Linux, is in fact, GNU/Linux, or as I've recently taken to calling it, GNU plus Linux. Linux is not an operating system unto itself, but rather another"
    privMsg target "free component of a fully functioning GNU system made useful by the GNU corelibs, shell utilities and vital system components comprising a full OS as defined by POSIX. Many computer users run a modified version of the GNU system every day, without realizing it."
    privMsg target "Through a peculiar turn of events, the version of GNU which is widely used today is often called \"Linux\", and many of its users are not aware that it is basically the GNU system, developed by the GNU Project."
    privMsg target "There really is a Linux, and these people are using it, but it is just a part of the system they use. Linux is the kernel: the program in the system that allocates the machine's resources to the other programs that you run."
    privMsg target "The kernel is an essential part of an operating system, but useless by itself; it can only function in the context of a complete operating system. Linux is normally used in combination with the GNU operating system: the whole system is basically GNU with Linux added, or GNU/Linux. All the so-called \"Linux\" distributions are really distributions of GNU/Linux."
    chNick  ournick
eval source action target msg
    -- banned phrases
    | "allah is doing" `isInfixOf` msg = kickBan target source
    -- Actual work
    -- check commands before checking regex
    | "!" `isPrefixOf` msg = botCommand source action target msg
    -- regex searches
    | msg =~ ("[Dd]evbot[;:,]?" :: String) = evalMention source action target msg
    | msg =~ regex_GH_num = issueFinder target msg
    | msg =~ regex_gl  = do
        mapM_ (gitLabMagic target) ((getAllTextMatches $ msg =~ regex_gl) :: [String])
        return ()
    | msg =~ regex_gli = privMsg target $ "https://gitlab.com/uTox/uTox/issues/"         ++ drop 1 (msg =~ regex_gli)
    -- be funny!
    | "that's wrong!"  `isInfixOf`  msg = privMsg target "OH NO! someone is wrong on the internet! https://xkcd.com/386/"
    | otherwise = return ()

gitLabMagic :: String -> String -> Net ()
gitLabMagic x y = privMsg x $ "https://gitlab.com/uTox/uTox/merge_requests/" ++ drop 1 (dropWhile (/= '!') y)

evalMention :: String -> String -> String -> String -> Net()
evalMention _ _ t msg
    -- Memes and time wasters
    | "is fixed"    `isInfixOf` msg = privMsg t "WELL... maybe SOMEONE, should stop breaking me!"
    | "get to work" `isInfixOf` msg = privMsg t "Sir, yes sir!!!"
    | msg =~ ("[Gg]o home devbot(.)+you're drunk" :: String) = do privMsg t "oh... okay :("; partChan t
    | msg =~ ("(you're|your|is) awesome" :: String) = privMsg t "Awww... I love you too!"
    | msg =~ ("what(')?s (next|left)\\?" :: String) = eval "" "" t "!m"
    | msg =~ ("^[Dd]evbot[;:,]? " :: String) = privMsg t "You said my name!! :D"
    | otherwise = return ()


botCommand :: String -> String -> String -> String -> Net ()
botCommand _ _ target "!moose"        = privMsg target "https://www.youtube.com/watch?v=7fE0YhEFvx4"
botCommand _ _ target "!commitsudoku" = privMsg target "http://www.sudokuweb.org/"
botCommand _ _ target "!uptime"       = uptime >>= privMsg target
botCommand _ _ target "!rip"          = sayRip target
botCommand _ _ target "!m"            = io (nextMilestone True "TokTok" "c-toxcore") >>= privMsg target
botCommand _ _ target "!echo"         = privMsg target "Echo what exactly?"
botCommand source action target msg
    -- all the cool commands we do stuff with
    | "!echo " `isPrefixOf` msg = do
        privMsg target $ drop 6 msg
    | "!die" == msg = do
        privMsg target "Sure, I'll just DIE then!"
        write "QUIT" ":My death was ordered" >> io (exitWith ExitSuccess)
    | "!milestone" `isPrefixOf` msg = do
        text <- io $ nextMilestone False "TokTok" "c-toxcore"
        privMsg target $ text
    | "!status " `isPrefixOf` msg = do
        let s = chkStatus $ takeWhile (/= ' ') . drop 8 $ msg
        if isJust s
            then privMsg target $ fromJust s
            else return ()
    | "!set "    `isPrefixOf` msg = setState source target (drop 5 msg)
    | "!ghost "  `isPrefixOf` msg = privMsg ((takeWhile (/= ' ') . drop 7) msg) ((drop 1 . dropWhile (/= ' ') . drop 8) msg)
    | "!join "   `isPrefixOf` msg = joinChan ((takeWhile (/= ' ') . drop 6) msg)
    | "!build "  `isPrefixOf` msg = do
        res <- io $ ciTriggerGitlab $ (takeWhile (/= ' ') . drop 7) msg
        if res
            then privMsg target "Build Running :D => https://gitlab.com/uTox/uTox/pipelines"
            else privMsg target "Error Sending Commands :<"
    |   otherwise = return ()

setState :: String -> String -> String -> Net ()
setState src trg msg
    | "#" `isPrefixOf` trg = setStateChannel src trg msg
    | otherwise = setStateUser src trg msg

setStateChannel :: String -> String -> String -> Net ()
setStateChannel _ trg msg
    | "repo_owner " `isPrefixOf` msg = chanSetOwn  trg (drop 11 msg)
    | "repo_name "  `isPrefixOf` msg = chanSetRepo trg (drop 10 msg)
    | otherwise = return ()

setStateUser :: String -> String -> String -> Net ()
setStateUser src trg msg
    | "gitlab_token " `isPrefixOf` msg = return () -- userSetToken
    | otherwise = return ()

sayRip :: String -> Net ()
sayRip t = do
    privMsg t "Poor one out for the devs Tox has eaten"
    privMsg t "irungentoo 2013-2016"
    privMsg t "iphy 2015-2017"
    privMsg t "mannol 2014-2016"
    privMsg t "jfreegman 2014-2015"
    privMsg t "May Tox rest their soul..."

chkStatus :: String -> Maybe String
chkStatus "iphy"    = Just "iphy's current status :: https://img.shields.io/badge/iphy-rip-gray.svg"
chkStatus "toxcore" = Just "Toxcore's current status :: http://i.imgur.com/c4jt321.png"
chkStatus "devbot"  = Just "DevBot's current status :: https://utox.io/devbot.jpg"
chkStatus "haskell" = Just "Haskell's current status :: https://utox.io/haskell.png"
chkStatus "qtox"    = Just "qTox's current status :: https://utox.io/qtox.jpg :: http://qtox.rip"
chkStatus "utox"    = Just "uTox's current status :: https://utox.io/utox.png"
chkStatus _         = Nothing

kickBan :: String -> String -> Net ()
kickBan channel shitball = do
    setBan channel shitball
    kick   channel shitball

kick :: String -> String -> Net ()
kick channel shitball = write "KICK" $ channel ++ " " ++ takeWhile (/= '!') shitball

setBan :: String -> String -> Net ()
setBan channel shitball = chanMode "+b" channel (dropWhile (/= '!') shitball)

chanMode :: String -> String -> String -> Net ()
chanMode mode target user = do
    write "MODE" $ target ++ " " ++ mode ++ " " ++ user

dumpChans :: [Channel] -> Net ()
dumpChans channs = mapM_ (\x -> io $ putStrLn ((chname x) ++ " " ++ (default_repo x))) channs

chanSetRepo :: String -> String -> Net ()
chanSetRepo search repo = do
    real <- gets channels
    let (ch, other) = botPopChan search real
    if null ch
        then return ()
        else do
            let ch' = head ch -- We're just guessing here :<
            let ch2 = ch' { default_repo = repo }
            let new = nubBy (\x y -> chname x == chname y) $ ch2 : other
            modify (\bot -> bot { channels = new })

chanSetOwn :: String -> String -> Net ()
chanSetOwn search owner = do
    real <- gets channels
    let (ch, other) = botPopChan search real
    if null ch
        then return ()
        else do
            let ch' = head ch -- We're just guessing here :<
            let ch2 = ch' { default_rown = owner }
            let new = nubBy (\x y -> chname x == chname y) $ ch2 : other
            modify (\bot -> bot { channels = new })

privMsg :: String -> String -> Net ()
privMsg to text = write "PRIVMSG" $ to ++ " :" ++ text

chNick :: String -> Net ()
chNick nick = write "NICK" nick

botPopChan :: String -> [Channel] -> ([Channel], [Channel])
botPopChan search cs = partition (\x -> search == chname x) cs

joinChan :: String -> Net ()
joinChan c = do
    write "JOIN" c
    old <- gets channels
    let chan = (Channel c [] [] [])
    let new = nubBy (\x y -> chname x == chname y) $ chan : old
    -- dumpChans new
    modify (\bot -> bot { channels =  new })

partChan :: String -> Net ()
partChan chan = write "PART"  (chan ++ " :bye then...")

uptime :: Net String
uptime = do
    now  <- io getCurrentTime
    zero <- gets start_time
    return . humanTime $ diffUTCTime now zero

issueFinder :: String -> String -> Net ()
issueFinder trg msg = do
    allchan <- gets channels
    let (ch, _) = botPopChan trg allchan
    let (owner, repo, inum) = parseIssueRequest (head ch) trg msg
    gl <- io $ glIssueFind inum
    privMsg trg gl
    url <- io $ checkIssue owner repo $ read inum
    if isJust url
        then privMsg trg $ fromJust url
        else privMsg trg ("Can't find that issue (" ++ owner ++ "/" ++ repo ++ "#" ++ inum ++ ")")

parseIssueRequest :: Channel -> String -> String -> (String, String, String)
parseIssueRequest ch trg msg = do
    let owner = if msg =~ regex_GH_owner
                    then takeWhile (/= '/') $ msg =~ regex_GH_owner
                    else if default_rown ch /= "" then default_rown ch else drop 1 trg
    let repo = if msg =~ regex_GH_repo
                    then takeWhile (/= '#') $ msg =~ regex_GH_repo
                    else if default_repo ch /= "" then default_repo ch else drop 1 trg
    let iss = drop 1 (msg =~ regex_GH_num) :: String
    (owner, repo, iss)


humanTime :: NominalDiffTime -> String
humanTime td =
    unwords $ map (uncurry (++) . first show) $
    if null diffs then [(0,"s")] else diffs
    where merge (tot, acc) (sec, typ) = let (sec', tot') = divMod tot sec
                                        in (tot', (sec',typ):acc)
          metrics = [(86400, "d"), (3600, "h"), (60, "m"), (1, "s")]
          diffs   = filter ((/= 0) . fst) $ reverse $ snd $
                    foldl' merge (round td,[]) metrics

------------------------------------------------
-- GitHub Milestones
------------------------------------------------
nextMilestone :: Bool -> String -> String -> IO (String)
nextMilestone verbose group repo = do
    list <- G.milestones (G.mkOwnerName $ P.pack group) (G.mkRepoName $ P.pack repo)
    case list of
        (Left err) -> return $ "Error: " ++ show err
        (Right milestones) -> parseMilestone verbose (milestonesToNext (V.toList milestones)) group

milestonesToNext :: [G.Milestone] -> G.Milestone
milestonesToNext mList = do
    head . sortOn G.milestoneDueOn . filter (\x -> "open" == (G.milestoneState x)) $ mList

parseMilestone :: Bool -> G.Milestone -> String -> IO (String)
parseMilestone verbose miles group = do
    let m_tag = P.unpack (G.milestoneTitle miles)
    let name  = (group ++ "-" ++ m_tag)
    let url   = P.unpack $ G.getUrl $ G.milestoneHtmlUrl miles

    let open    = G.milestoneOpenIssues miles
    let closed  = G.milestoneClosedIssues miles
    let total   = open + closed
    let percent = (fromIntegral closed) / (fromIntegral total ) :: Float
    now <- getCurrentTime
    let time    = diffUTCTime (fromJust $ G.milestoneDueOn miles) now

    -- Try to make it small
    str <- githubMkShort url (name)

    if verbose
        then return $ (printf "Milestone %.3f%% (%d of %d) " (percent * 100) closed total :: String) ++ (fromMaybe (show url) str) ++
                        " || " ++ "https://reviewable.io/reviews#q=" ++ m_tag ++ " || Due in " ++ humanTime time
        else return $ (printf           "%.0f%% (%d of %d) " (percent * 100) closed total :: String) ++ (fromMaybe (show url) str)

------------------------------------------------
--  GitHub Issues
------------------------------------------------
parseAssigned :: G.Issue -> IO [String]
parseAssigned issue = do
    let assigned = V.toList $ G.issueAssignees issue
    let names = map (githubToIRC . P.unpack . G.untagName . G.simpleUserLogin) assigned
    return (names)

checkIssue :: String -> String -> Int -> IO (Maybe String)
checkIssue o r inum = do
    possibleIssue <- G.issue owner repo issID
    case possibleIssue of
        Left  _          -> return (Nothing)
        Right real_issue -> realIssue r o inum real_issue
    where
        owner = (G.mkOwnerName (P.pack o))
        repo  = (G.mkRepoName  (P.pack r))
        issID = (G.Id inum)

realIssue :: String -> String -> Int -> G.Issue -> IO (Maybe String)
realIssue repo_name owner issu_numb issue = do
    a_users <- parseAssigned issue
    let a_user = (intercalate " "  (" Assigned to:" : a_users))
    let o_user = (P.unpack . G.untagName . G.simpleUserLogin $ G.issueUser  issue)
    let title  = (P.unpack $ G.issueTitle issue)
    let url    = (P.unpack . G.getUrl $ fromJust $ G.issueHtmlUrl issue)
    let str    = title ++ " (Owner: " ++ (githubToIRC o_user) ++ a_user ++ ") " ++ url ++ " "
    if "pull" `isInfixOf` url
        then return (Just (str ++ "|| https://reviewable.io/reviews/" ++ owner ++ "/" ++ repo_name ++ "/" ++ show issu_numb))
        else return (Just str)


------------------------------------------------
--  GitLab helpers
------------------------------------------------
data GitLab_Issue = GitLab_Issue
    {   iD                      :: Int
    ,   issueId                 :: Int
    ,   issueProjectId          :: Int
    ,   issueTitle              :: String
    ,   issueDescription        :: String
    ,   issueLabels             :: [String]
    -- ,   issueMilestone          :: Maybe Milestone
    -- ,   issueAssignee           :: Maybe SimpleUser
    -- ,   issueAuthor             :: SimpleUser
    ,   issueState              :: String
    ,   issueUpdatedAt          :: UTCTime
    ,   issueCreatedAt          :: UTCTime
    ,   webURL                  :: String
  } deriving (Show, Generic)

instance FromJSON GitLab_Issue where
    parseJSON = withObject "GitLab_Issue" $ \o -> GitLab_Issue
        <$> o .: "id"
        <*> o .: "iid"
        <*> o .: "project_id"
        <*> o .: "title"
        <*> o .: "description"
        <*> o .: "labels"
        <*> o .: "state"
        <*> o .: "updated_at"
        <*> o .: "created_at"
        <*> o .: "web_url"

instance ToJSON   GitLab_Issue

devbot_token = ""
utox_utox_project_id = "2748139"

glIssueFind :: String -> IO (String)
glIssueFind i = do
    (code, body) <- gitlabAPIpull ("https://gitlab.com/api/v3/projects/2748139/issues?iid=" ++ i) [("PRIVATE-TOKEN", devbot_token)]

    putStrLn $ "The status code was: " ++ (show code)

    let list = decode body :: Maybe [GitLab_Issue]
    if isJust list
        then do let issue = head (fromJust list)
                return ("Issue is " ++ issueState issue ++ " " ++ issueTitle issue ++ " " ++ webURL issue)
        else return ("No Issue Found :<")

ciTriggerGitlab :: String -> IO (Bool)
ciTriggerGitlab ref = do
    let gl_ci_target = "https://gitlab.com/api/v3/projects/2748139/trigger/builds"
    let gl_ci_token  = ""
    let d = [("token", gl_ci_token), ("ref", ref)]
    (code, body) <- gitlabAPIpush gl_ci_target [] d

    putStrLn $ LCHAR8.unpack body
    putStrLn $ "The status code was: " ++ (show code)

    if code == 201
        then return (True)
        else return (False)

gitlabAPIpull :: String -> [(CI.CI CHAR8.ByteString, CHAR8.ByteString)] -> IO (Int, LCHAR8.ByteString)
gitlabAPIpull url hders = do
    initRequest <- parseRequest url
    let request = setRequestHeaders hders $ initRequest
    putStrLn $ show request
    manager <- newManager tlsManagerSettings
    response <- Network.HTTP.Client.httpLbs request manager

    let code = statusCode $ responseStatus response
    let body = responseBody response
    return (code, body)

gitlabAPIpush :: String -> [(CI.CI CHAR8.ByteString, CHAR8.ByteString)] -> [(String, String)] -> IO (Int, LCHAR8.ByteString)
gitlabAPIpush url hders rdata = do
    let p_data = map (\(x, y) -> ((CHAR8.pack x), (CHAR8.pack y))) $ rdata
    initRequest <- parseRequest url
    let request = setRequestHeaders hders $ urlEncodedBody p_data initRequest
    putStrLn $ show request
    manager <- newManager tlsManagerSettings
    response <- Network.HTTP.Client.httpLbs request manager

    let code = statusCode $ responseStatus response
    let body = responseBody response
    return (code, body)


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
    response <- Network.HTTP.Client.httpLbs request manager
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
