data MyContext = MyContext
    { stdinVar :: MVar String
    }

data StdinAndTime = StdinAndTime
    { stdin_ :: String
    , currentTime_ :: ZonedTime
    , count_ :: Int
    }

data StdoutOrExit = StdoutOrExit
    { stdout_ :: String
    , exit_ :: Maybe ()
    }

instance Context MyContext where
    initContext = do
        stdinMVar <- newEmptyMVar
        forkIO $ forever $ do
            line <- getLine
            putMVar stdinMVar line
            yield
            takeMVar stdinMVar
        return $ MyContext
            { stdinVar = stdinMVar
            }


stdin :: MyContext -> InPlace String
stdin (MyContext stdinVar) = InPlace (\_ -> readMVar stdinVar)

currentTime :: MyContext -> InPlace ZonedTime
currentTime (MyContext _) = InPlace (\_ -> getZonedTime)

stdinAndTime :: MyContext -> InPlace Int -> InPlace StdinAndTime
stdinAndTime context counterPlace = StdinAndTime
    <$> stdin context
    <*> currentTime context
    <*> counterPlace

stdout :: MyContext -> OutPlace String
stdout _ = OutPlace putStrLn

stdoutOrExit :: MyContext -> OutPlace () -> OutPlace StdoutOrExit
stdoutOrExit context exit = 
    spread 
        [ stdout_ >$< stdout context
        , exit_ >$< maybeOut exit
        ]

userEntersInput :: Event MyContext
userEntersInput = 
    let 
        action :: MyContext -> IO ()
        action (MyContext stdinVar) = 
            void $ readMVar stdinVar
    in
    Event action

quit :: String -> Maybe ()
quit input = 
    if input == "quit" then Just () else Nothing

echoOrQuit :: String -> StdoutOrExit
echoOrQuit userInput =
    StdoutOrExit
        { stdout_ = userInput
        , exit_ = quit userInput
        }

echoWithTimestamp :: StdinAndTime -> (String, Int)
echoWithTimestamp (StdinAndTime stdin_ currentTime_ count_) =
    ( show currentTime_ ++ ": " ++ stdin_ ++ " (" ++ show count_ ++ ")"
    , count_ + 1
    )


myProgram :: OutPlace () -> IO ()
myProgram exit = do
    counterMVar <- newMVar 0
    context <- initContext
    let onEvent' = onEvent context
        counterIn = asInPlace counterMVar
        counterOut = asOutPlace counterMVar
    onStart (\_ -> "Hello!") fromNowhere (stdout context)
    onEvent' userEntersInput quit (stdin context) (maybeOut $ exit)
    onEvent' userEntersInput echoWithTimestamp (stdinAndTime context counterIn) (pair (stdout context, counterOut))

main = exitable myProgram