{-# LANGUAGE OverloadedStrings #-}

-- import              Control.Arrow
import           Control.Arrow                      (first)
import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import           Data.Maybe
import           Data.Time.Clock
import           Network
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
            ,   "#toktok"
            ,   "#tox-dev"
            ]

nick_password = ""

enabled_repos = [   -- TokTok repos
                    "apidsl"
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
                ,   "semdoc"
                ,   "spec"
                ,   "tokstyle"
                ,   "toktok-android"
                ,   "toktok-stack"
                ,   "toktok.github.io"
                ,   "toxcore"
                ,   "website"
                    -- uTox/ repos
                ,   "utox"
                ]

enabled_owners =    [   "utox"
                    ,   "toktok"
                    ,   "irungentoo"
                    ] :: [String]

regex_GH_num    = "#([0-9]{1,5})" :: String
regex_GH_repo   = "(" ++ (intercalate "|" enabled_repos)  ++ ")"  ++ regex_GH_num
regex_GH_owner  = "(" ++ (intercalate "|" enabled_owners) ++ ")/" ++ regex_GH_repo
regex_gl        = "(![0-9]{1,5})" :: String
regex_gli       = "(i[0-9]{1,5})" :: String


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
    }

type Net  = StateT Bot IO

data User = User
    {   nick :: String
    ,   user :: String
    ,   host :: String
    }

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
    -- Memes and time wasters
    | "devbot" `isPrefixOf` msg && "get to work" `isInfixOf` msg = privMsg target "Sir, yes sir!!!"
    | "devbot is fixed" `isPrefixOf` msg = privMsg target "WELL... maybe SOMEONE, should stop breaking me!"
    | "devbot you're awesome" `isInfixOf` msg = privMsg target "Awww... I love you too!"
    | "devbot you're awesome" `isInfixOf` msg = privMsg target "Awww... I love you too!"
    | "devbot is awesome" `isInfixOf` msg = privMsg target "Awww... I love you too!"
    | "that's wrong!" `isInfixOf` msg = privMsg target "OH NO! someone is wrong on the internet! https://xkcd.com/386/"
    | "o home devbot" `isInfixOf` msg && "you're drunk" `isInfixOf` msg = do
        privMsg target "oh... okay :("
        partChan target
    -- banned phrases
    | "allah is doing" `isInfixOf` msg = kickBan target source
    -- Actual work
    | "what's next?" `isInfixOf` msg = eval source action target "!ms"
    | "whats next?" `isInfixOf` msg = eval source action target "!ms"
    | "what's left?" `isInfixOf` msg = eval source action target "!ms"
    | "whats left?" `isInfixOf` msg = eval source action target "!ms"
    -- check commands before checking regex
    | "!" `isPrefixOf` msg = evalCommand source action target msg
    -- regex searches
    | msg =~ regex_GH_num = issueFinder source action target msg
    | msg =~ regex_gl  = do
        mapM_ (privMsg' target) ((getAllTextMatches $ msg =~ regex_gl) :: [String])
        return ()
    | msg =~ regex_gli = privMsg target $ "https://gitlab.com/uTox/uTox/issues/"         ++ drop 1 (msg =~ regex_gli)
    | otherwise = return ()

privMsg' :: String -> String -> Net ()
privMsg' x y = privMsg x $ "https://gitlab.com/uTox/uTox/merge_requests/" ++ drop 1 y

evalCommand :: String -> String -> String -> String -> Net ()
evalCommand source action target msg
    -- all the cool commands we do stuff with
    | "!echo " `isPrefixOf` msg = do
        privMsg target $ drop 6 msg
    | "!echo" == msg = do
        privMsg target "Echo what exactly?"
    | "!die" == msg = do
        privMsg target "Sure, I'll just DIE then!"
        write "QUIT" ":My death was ordered" >> io (exitWith ExitSuccess)
    | "!m"   == msg = do
        text <- io $ nextMilestone True "TokTok" "c-toxcore"
        privMsg target $ text
    | "!milestone" `isPrefixOf` msg = do
        text <- io $ nextMilestone False "TokTok" "c-toxcore"
        privMsg target $ text
    | "!status " `isPrefixOf` msg = do
        let s = chkStatus $ takeWhile (/= ' ') . drop 8 $ msg
        if isJust s
            then privMsg target $ fromJust s
            else return ()
    | "!moose"  `isPrefixOf` msg = privMsg target "https://www.youtube.com/watch?v=7fE0YhEFvx4"
    | "!commitsudoku" == msg = privMsg target "http://www.sudokuweb.org/"
    | "!uptime" == msg = uptime >>= privMsg target
    | "!build "  `isPrefixOf` msg = do
        res <- io $ ciTriggerGitlab $ (takeWhile (/= ' ') . drop 7) msg
        if res
            then privMsg target "Build Running :D => https://gitlab.com/uTox/uTox/pipelines"
            else privMsg target "Error Sending Commands :<"
    |   otherwise = return ()

chkStatus :: String -> Maybe String
chkStatus "iphy"    = Just "iphy's current status :: https://img.shields.io/badge/iphy-savage-red.svg"
chkStatus "toxcore" = Just "Toxcore's current status :: https://img.shields.io/badge/toxcore-rip-red.svg"
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
    let (ch, other) = (botPopChan search real)
    let ch' = if isJust ch then (fromJust ch) else Channel search [] repo repo -- We're just guessing here :<
    let ch' = ch' { default_repo = repo }
    let new = other ++ [ch']
    -- dumpChans new
    modify (\bot -> bot { channels = new })

privMsg :: String -> String -> Net ()
privMsg to text = write "PRIVMSG" $ to ++ " :" ++ text

chNick :: String -> Net ()
chNick nick = write "NICK" nick

botPopChan :: String -> [Channel] -> (Maybe Channel, [Channel])
botPopChan search cs = do
    if isJust found
        then (found, other)
        else (Nothing, cs)
    where
        found = find   (\x -> search == chname x) cs
        other = filter (\x -> search /= chname x) cs

joinChan :: String -> Net ()
joinChan c = do
    write "JOIN" c
    old <- gets channels
    let chan = (Channel c [] [] [])
    let new = (old ++ [chan])
    -- dumpChans new
    modify (\bot -> bot { channels =  new })

partChan :: String -> Net ()
partChan chan = write "PART"  (chan ++ " :bye then...")

uptime :: Net String
uptime = do
    now  <- io getCurrentTime
    zero <- gets start_time
    return . humanTime $ diffUTCTime now zero

issueFinder :: String -> String -> String -> String -> Net ()
issueFinder src act trg msg
    -- Issue finder
    | msg =~ regex_GH_owner = do
        let set_own = takeWhile (/= '/') $ msg =~ regex_GH_owner
        url <- io $ checkIssue set_own repo $ read inum
        if isJust url
            then do privMsg trg $ fromJust url
                    chanSetRepo trg repo
            else privMsg trg ("Can't find that issue (" ++ set_own ++ "/" ++ repo ++ "#" ++ inum ++ ")")
    | msg =~ regex_GH_repo = do
        url <- io $ checkIssue owner repo $ read inum
        if isJust url
            then do privMsg trg $ fromJust url
                    chanSetRepo trg repo
            else privMsg trg ("Can't find that issue (" ++ owner ++ "/" ++ repo ++ "#" ++ inum ++ ")")
    | msg =~ regex_GH_num = do
        allchan <- gets channels
        let (ch, _) = botPopChan trg allchan
        let last_repo = if isJust ch then default_repo (fromJust ch) else ""
        url <- io $ checkIssue owner last_repo $ read inum
        if isJust url
            then privMsg trg $ fromJust url
            else privMsg trg ("Can't find that issue (_/" ++ last_repo ++ "#" ++ inum ++ ")")
    | otherwise = return ()
    where
        owner = drop 1 trg
        repo  = (takeWhile (/= '#') $ msg =~ regex_GH_repo)
        inum  = drop 1 (dropWhile (/= '#') $ msg =~ regex_GH_num)

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
        Left _ -> if any (== owner) $ map (G.mkOwnerName . P.pack) enabled_owners
                    -- Swap once (Hacky)
                    then return (Nothing)
                    else case owner of
                        "toktok" -> checkIssue "utox" r inum
                        "utox"   -> checkIssue "toktok" r inum
                        _        -> return (Nothing)
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
gl_ci_token  = ""
gl_ci_target = ""

ciTriggerGitlab :: String -> IO (Bool)
ciTriggerGitlab ref = do
    manager <- newManager tlsManagerSettings
    initRequest <- parseRequest gl_ci_target

    let requestText = [("token", CHAR8.pack gl_ci_token), ("ref", CHAR8.pack ref)]
    let request = urlEncodedBody requestText $ initRequest { method = "POST" }
    response <- httpLbs request manager

    let code = statusCode $ responseStatus response
    putStrLn $ LCHAR8.unpack (responseBody response)
    putStrLn $ "The status code was: " ++ (show code)

    let headers = responseHeaders response
    let res = (lookup "location" headers)
    if code == 201
        then return (True)
        else return (False)

gitlabAPI :: String -> IO (Bool)
gitlabAPI ref = do
    manager <- newManager tlsManagerSettings
    initRequest <- parseRequest gl_ci_target

    let requestText = [("token", CHAR8.pack gl_ci_token), ("ref", CHAR8.pack ref)]
    let request = urlEncodedBody requestText $ initRequest { method = "POST" }
    response <- httpLbs request manager

    let code = statusCode $ responseStatus response
    putStrLn $ LCHAR8.unpack (responseBody response)
    putStrLn $ "The status code was: " ++ (show code)

    let headers = responseHeaders response
    let res = (lookup "location" headers)
    if code == 201
        then return (True)
        else return (False)


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
