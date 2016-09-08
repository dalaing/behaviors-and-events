% Functional Reactive Programming with Events and Behaviours
% Dave Laing

# Introduction

##

"True" FRP means:

##

- Events and behaviours

## 

- Denotational semantics

## 

- The ability to handle continuous time

##

What does it give us: composable systems of time-varying state and logic.

##

We are looking at `reactive-banana` today.

##

Part 2 will cover `reflex`.

##

Worth looking at `sodium` if you want this goodness in an other-than-Haskell flavour.

# Events

##

```haskell
Event a
```

```haskell
instance Functor Event
```

##

Events fire at single logical points in time.

##

```haskell
Event a ~ [(Time,a)]
```

##

Events are push-based.

## 

Each firing of an event is a new logical point in time.

## 

There can be multiple different events active at the same logical point in time.

## 

Outputs active at the same points in time as the inputs:

```haskell
doubler :: Event Int -> Event Int
doubler eNumber =
  let
    eOtherNumber = (* 2) <$> eNumber
  in
    eOtherNumber
```

##

There are ways of filtering and splitting events:

```haskell
filterE :: (a -> Bool) -> Event a -> Event a
split :: Event (Either a b) -> (Event a, Event b)
-- ... and more that we'll see later
```

##

Outputs active at the same points in time as the inputs - when the outputs are active at all:

```haskell
commands :: Event String -> (Event String, Event ())
commands eLine =
  let
    eMessage = filterE (/= "/quit") eLine
    eQuit    = filterE (== "/quit") eLine
  in
    (eMessage, eQuit)
```

##

We need to be aware of the potential for simultaneous events when we combine them

##

The combining function is only used if the two inputs events are simultaneous:
```haskell
unionWith :: (a -> a -> a) 
          -> Event a 
          -> Event a 
          -> Event a
```

##

Can build a useful helper from this if you know you're not dealing with simultaneous events:
```haskell
leftmost :: [Event a] -> Event a
leftmost = foldl (unionWith const) never
```

##

There are other options for composing events:
```haskell
unions :: [Event (a -> a)] -> Event (a -> a)
```
##

```haskell
multiple :: Int -> Event Int -> Event Int
multiple m = 
  filterE (\x -> x `mod` m == 0)

importantWork :: Event Int -> Event String
importantWork eCount =
  let
    eFizz = "Fizz" <$ multiple 3 eCount
    eBuzz = "Buzz" <$ multiple 5 eCount
    eFizzBuzz = 
      unionWith (\_ _ -> "FizzBuzz") 
      eFizz 
      eBuzz
  in
    eFizzBuzz
```

##

How do we get _new_ logical points in time?

##

They come from outside the event network.

##

How do we know we're dealing with something that effects something other than the current logical point in time?

##

You'll see a `Moment` or `MomentIO` context in the type signature.

##

These 'moments' are referred to as 'transactions' in the `sodium` literature.

##

```haskell
newAddHandler :: IO (AddHandler a, a -> IO ()) 
```

##

```haskell
data EventSource a = EventSource {
    addHandler :: AddHandler a
  , fire       :: a -> IO ()
  }
```

```haskell
mkEventSource :: IO (EventSource a)
mkEventSource =
  uncurry EventSource <$> newAddHandler
```

##

There are as many observable logical points in time as there are calls to the various `fire` functions.

##

From inside the event network, we can register an event handler:
```haskell
fromAddHandler :: AddHandler a -> MomentIO (Event a) 
```

##

From inside the event network, we can do some `IO` when an event occurs:
```haskell
reactimate :: Event (IO ()) -> MomentIO () 
```

##

Now we can put together an event network.

##

```haskell
networkDescription :: EventSource Int -> MomentIO ()
networkDescription c = do
  eCount <- fromAddHandler . addHandler $ c

  let
    eFizz = "Fizz" <$ multiple 3 eCount
    eBuzz = "Buzz" <$ multiple 5 eCount
    eWrite =
      unionWith (\_ _ -> "FizzBuzz")
      eFizz
      eBuzz
    showCount x =
      putStrLn $ "count: " ++ show x

  reactimate $ showCount <$> eCount
  reactimate $ putStrLn  <$> eWrite
```

##

We need an event loop to fire events from outside of the event network.

##

```haskell
eventStep :: EventSource Int -> Int -> IO ()
eventStep e i = do
  fire e i
  threadDelay 1000000

eventLoop :: EventSource Int -> IO ()
eventLoop e =
  traverse_ (eventStep e) [0..]
```

##

```haskell
go :: IO ()
go = do
  input <- mkEventSource
  network <- compile $ networkDescription input
  actuate network
  eventLoop input
```

##

Some of the logic is outside of the event network...

##

What we really want:
```haskell
eventLoop :: EventSource () -> IO ()
eventLoop e =
  forever $ do
    threadDelay 1000000
    fire e ()
```

##

```haskell
accumE :: MonadMoment m => a -> Event (a -> a) -> m (Event a) 
```

##

```haskell
counter :: MonadMoment m => Event () -> m (Event Int)
counter eTick = accumE 0 ((+ 1) <$ eTick)
```

##

```haskell
networkDescription :: EventSource () -> MomentIO ()
networkDescription t = do
  eTick <- fromAddHandler . addHandler $ t

  eCount <- accumE 0 ((+ 1) <$ eTick)

  let
    eFizz = "Fizz" <$ multiple 3 eCount
    eBuzz = "Buzz" <$ multiple 5 eCount
    eWrite =
      unionWith (\_ _ -> "FizzBuzz")
      eFizz
      eBuzz
    showCount x =
      putStrLn $ "count: " ++ show x

  reactimate $ showCount <$> eCount
  reactimate $ putStrLn  <$> eWrite
```

# A simple command line application

##

We're going to start with a program that echoes the input from the user.

##

```haskell
eventLoop :: EventSource String -> IO ()
eventLoop i =
  forever $ do
    x <- getLine
    fire i x
```

##

```haskell
networkDescription :: EventSource String -> MomentIO ()
networkDescription i = do
  eRead <- fromAddHandler . addHandler $ i

  let
    eWrite = eRead

  reactimate $ putStrLn <$> eWrite
```

##

Variant: Add the ability to quit

##

```haskell
import System.Exit (exitSuccess) 

networkDescription :: EventSource String -> MomentIO ()
networkDescription i = do
  eRead <- fromAddHandler . addHandler $ i

  let
    eMessage = 
      filterE (/= "/quit") eRead
    eQuit = 
      () <$ filterE (== "/quit") eRead

  reactimate $ putStrLn    <$> eMessage
  reactimate $ exitSuccess <$ eQuit
```

##

Variant: Add a parting message

##

```haskell
networkDescription :: EventSource String -> MomentIO ()
networkDescription i = do
  eRead <- fromAddHandler . addHandler $ i

  let
    eMessage = 
      filterE (/= "/quit") eRead
    eQuit = 
      () <$ filterE (== "/quit") eRead
    eWrite = leftmost [
        eMessage
      , "Bye" <$ eQuit
      ]

  reactimate $ putStrLn    <$> eWrite
  reactimate $ exitSuccess <$  eQuit
```

## 

Variant: Add a greeting message

##

```haskell
data InputSources = InputSources {
    isOpen :: EventSource ()
  , isRead :: EventSource String
  }

mkInputSources :: IO InputSources
mkInputSources =
  InputSources <$> mkEventSource <*> mkEventSource
```

##

```haskell
eventLoop :: InputSources -> IO ()
eventLoop (InputSources o r) = do
  fire o ()
  forever $ do
    x <- getLine
    fire r x
```

##

```haskell
networkDescription :: InputSources -> MomentIO ()
networkDescription (InputSources o r) = do
  eOpen <- fromAddHandler . addHandler $ o
  eRead <- fromAddHandler . addHandler $ r

  let
    eMessage = 
      filterE (/= "/quit") eRead
    eQuit = 
      () <$ filterE (== "/quit") eRead
    eWrite = leftmost [
        "Hi" <$ eOpen
      , eMessage
      , "Bye" <$ eQuit
      ]

  reactimate $ putStrLn    <$> eWrite
  reactimate $ exitSuccess <$  eQuit
```

##

Variant: Add a help command

##

```haskell
...
  let
    eMessage = filterE (/= "/" . take 1) eRead
    eHelp    = () <$ filterE (== "/help") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite = leftmost [
      ...
      , helpMessage <$ eHelp
      ...
      ]
...
```

##

Variant: Deal with unknown commands

##

```haskell
...
  let
    eMessage = 
      filterE ((/= "/") . take 1) eRead
    eCommand = 
      fmap (drop 1) . filterE ((== "/") . take 1) $ eRead

    eHelp    = () <$ filterE (== "help") eCommand
    eQuit    = () <$ filterE (== "quit") eCommand

    commands = ["help", "quit"]
    eUnknown = filterE (`notElem` commands) eCommand

    eWrite = leftmost [
      ...
      , unknownMessage <$ eUnknown
      ...
      ]
...
```

# Some refactorings

##

Let's separate out the bits of the event network that deal with `IO` from the bits that don't.

##

```haskell
data InputIO = InputIO {
    ioeOpen :: Event ()
  , ioeRead :: Event String
  }

handleInput :: InputSources -> MomentIO InputIO
handleInput (InputSources iso isr) = do
  eOpen <- fromAddHandler . addHandler $ iso
  eRead <- fromAddHandler . addHandler $ isr
  return $ InputIO eOpen eRead
```

##

```haskell
data OutputIO = OutputIO {
    ioeWrite :: Event String
  , ioeClose :: Event ()
  }

handleOutput :: OutputIO -> MomentIO ()
handleOutput (OutputIO eWrite eClose) = do
  reactimate $ putStrLn <$> eWrite
  reactimate $ exitSuccess <$ eClose
```

##

```haskell
mkNetwork :: (InputIO -> Moment OutputIO) -> InputSources -> MomentIO ()
mkNetwork fn input = do
  i <- handleInput input
  o <- liftMoment $ fn i
  handleOutput o
```

##

Old version:

## 

```haskell
networkDescription :: InputSources -> MomentIO ()
networkDescription (InputSources o r) = do
  eOpen <- fromAddHandler . addHandler $ o
  eRead <- fromAddHandler . addHandler $ r

  let
    eMessage = 
      filterE (/= "/quit") eRead
    eQuit = 
      () <$ filterE (== "/quit") eRead
    eWrite = leftmost [
        "Hi" <$ eOpen
      , eMessage
      , "Bye" <$ eQuit
      ]

  reactimate $ putStrLn    <$> eWrite
  reactimate $ exitSuccess <$  eQuit
```

##

New version:

##

```haskell
pureNetworkDescription :: InputIO -> Moment OutputIO 
pureNetworkDescription (InputIO eOpen eRead) =
  let
    eMessage = 
      filterE (/= "/quit") eRead
    eQuit = 
      () <$ filterE (== "/quit") eRead
    eWrite = leftmost [
        "Hi" <$ eOpen
      , eMessage
      , "Bye" <$ eQuit
      ]
  in
    return $ OutputIO eWrite eQuit
```

##

Plus a wrapper:
```haskell
networkDescription :: InputSources -> MomentIO ()
networkDescription = 
  mkNetwork pureNetworkDescription
```

##

We're still dealing with things in terms of `IO`.

##

Let's set up some domain specific input events.

##

```haskell
data Inputs = Inputs {
    ieOpen           :: Event ()
  , ieMessage        :: Event String
  , ieHelp           :: Event ()
  , ieQuit           :: Event ()
  , ieUnknownCommand :: Event String
  }
```

##

```haskell
fanOut :: InputIO -> Inputs
fanOut (InputIO eOpen eRead) =
  let
    eReadNonEmpty = filterE (not . null) eRead

    isMessage = (/= "/") . take 1
    eMessage = filterE isMessage eReadNonEmpty

    isCommand = (== "/") . take 1
    mkCommand = fmap (drop 1) . filterE isCommand
    eCommand = mkCommand eReadNonEmpty

    eHelp = () <$ filterE (== "help") eCommand
    eQuit = () <$ filterE (== "quit") eCommand

    commands = ["help", "quit"]
    eUnknown = filterE (`notElem` commands) eCommand
  in
    Inputs eOpen eMessage eHelp eQuit eUnknown
```

##

We'll do something similar for the outputs.

##

```haskell
data Outputs = Outputs {
    oeWrite :: [Event String]
  , oeClose :: [Event ()]
  }
```

##

```haskell
fanIn :: Outputs -> OutputIO
fanIn (Outputs eWrites eCloses) =
  let
    addLine x y = 
      x ++ '\n' : y
    eCombinedWrites = 
      foldr (unionWith addLine) never eWrites
    eCombinedCloses = 
      () <$ leftmost eCloses
  in
    OutputIO eCombinedWrites eCombinedCloses
```

## 

```haskell
domainNetworkDescription :: Inputs -> Moment Outputs
domainNetworkDescription input =
  let
    eWrites = [
        greetingMsg <$ ieOpen input
      , ieMessage input
      , helpMessage <$ ieHelp input
      , partingMessage <$ ieQuit input
      , unknownMessageFn <$> ieUnknown input
      ]
    eQuits = [
        ieQuit input
      ]
  in
    return $ Outputs eWrites eQuits
```

##

```haskell
pureNetworkDescription :: InputIO -> Moment OutputIO
pureNetworkDescription i = do 
  o <- domainNetworkDescription . fanOut $ i
  return $ fanIn o
```

##

We still have a big ball of mud in the middle.

##

It can be useful to come up with several components to break things up.

##

```haskell
data OpenInput = 
  OpenInput { oieOpen  :: Event () }
data OpenOutput = 
  OpenOutput { ooeWrite :: Event String }

handleOpen :: OpenInput -> Moment OpenOutput
handleOpen (OpenInput eOpen) =
  let
    eWrite = "Hi (type /help for instructions)" <$ eOpen
  in
    return $ OpenOutput eWrite
```

##

```haskell
data MessageInput = 
  MessageInput  { mieRead  :: Event String } 
data MessageOutput = 
  MessageOutput { moeWrite :: Event String }

handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) =
  return $ MessageOutput eMessage
```

##

```haskell
data HelpInput = 
  HelpInput  { hieHelp  :: Event () }
data HelpOutput = 
  HelpOutput { hoeWrite :: Event String }

helpMessage :: String
helpMessage = intercalate "\n" [
    "/help displays this message"
  , "/quit exits the program"
  ]

handleHelp :: HelpInput -> Moment HelpOutput
handleHelp (HelpInput eHelp) =
  let
    eWrite = helpMessage <$ eHelp
  in
    return $ HelpOutput eWrite
```

##

```haskell
data QuitInput = 
  QuitInput { qieQuit :: Event () }

data QuitOutput = QuitOutput { 
    qoeWrite :: Event String
  , qoeQuit  :: Event ()
  }

handleQuit :: QuitInput -> Moment QuitOutput
handleQuit (QuitInput eQuit) =
  let
    eWrite = "Bye" <$ eQuit
  in
    return $ QuitOutput eWrite eQuit
```

##

```haskell
data UnknownInput = 
  UnknownInput  { ucieCommand :: Event String }
data UnknownOutput = 
  UnknownOutput { ucoeWrite   :: Event String }

unknownMessageFn :: String -> String
unknownMessageFn x =
  "Unknown command: " ++ x ++ 
  " (type /help for instructions)"

handleUnknown :: UnknownInput -> Moment UnknownOutput
handleUnknown (UnknownInput eUnknown) =
  return . UnknownOutput $ unknownMessageFn <$> eUnknown
```

##

```haskell
domainNetworkDescription :: Inputs -> Moment Outputs
domainNetworkDescription input = do
  OpenOutput eoWrite <- 
    handleOpen $ OpenInput (ieOpen input)

  MessageOutput emWrite <- 
    handleMessage $ MessageInput (ieMessage input)

  HelpOutput ehWrite <- 
    handleHelp $ HelpInput (ieHelp input)

  QuitOutput eqWrite eqQuit <- 
    handleQuit $ QuitInput (ieQuit input)

  UnknownOutput euWrite <- 
    handleUnknown $ UnknownInput (ieUnknown input)

  return $ Outputs 
    [eoWrite, emWrite, ehWrite, eqWrite, euWrite] [eqQuit]
```

# Behaviors

##

```haskell
Behavior a
```

```haskell
instance Functor Behavior
instance Applicative Behavior
```

##

Behaviors have a value at every point in time.

##

```haskell
Behavior a ~ (Time -> a)
```

##

Behaviors are pull-based.

##

If you squint, it looks a bit like `State a`.

##

Some of the time you'll use `Behavior` to model `State`.

##

Some of the time you'll use `Behavior` to pass around values that could be changed by parts of the program you don't care about.

##

Build `Behavior`s with `Event`s:
```haskell
stepper :: MonadMoment m => a -> Event a -> m (Behavior a)
```

##

```haskell
logInHandler :: Event () 
             -> Event () 
             -> Moment (Behavior LogInState)
logInHandler eLogIn eLogOut =
  stepper LoggedOut . leftmost $ [
      LoggedIn  <$ eLogIn
    , LoggedOut <$ eLogOut
    ]
```

##

Sample `Behavior`s with `Event`s:
```haskell
(<@>) :: Behavior (a -> b) -> Event a -> Event b
(<@)  :: Behavior b        -> Event a -> Event b 
```

##

```haskell
threeArgFn <$> bBehavior1 <*> bBehavior2 <@> eEvent
```

```haskell
twoArgFn   <$> bBehavior1 <*> bBehavior2 <@ eTrigger
```

##

Filter `Event`s with `Behavior`s:
```haskell
whenE       :: Behavior Bool -> Event a -> Event a
filterApply :: Behavior (a -> Bool) -> Event a -> Event a
```

##

```haskell
logIn :: LogInState -> Either LogInError LogInState
logIn LoggedIn  = Left AlreadyLoggedIn
logIn LoggedOut = Right LoggedIn

logOut :: LogInState -> Either LogInError LogInState
logOut LoggedOut = Left NotLoggedIn
logOut LoggedIn  = Right LoggedOut
```

##

```haskell
logInHandler :: Event () 
             -> Event () 
             -> Moment ( Behavior LogInState
                       , Event    LogInError
                       )
logInHandler eLogIn eLogOut = mdo
  bLogInState <- stepper LoggedOut eLogInState
  (eLogInError, eLogInState) = split . leftmost $ [
      logIn  <$> bLogInState <@ eLogIn
      logOut <$> bLogInState <@ eLogOut
    ]
  return (bLogInState, eLogInError)
```

##

Let's use behaviors in our echo application to keep track of message history.

##

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- stepper "" eMessage
  let
    format l m = m ++ " (last message: " ++ l ++ ")"
    eOut = format <$> bMessages <@> eMessage
  return $ MessageOutput eOut
```

##

One message isn't all that interesting.

##

```haskell
accumB :: MonadMoment m 
       => a 
       -> Event (a -> a) 
       -> m (Behavior a) 
```

##

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] $ (\x xs -> x : xs) <$> eMessage
  let
    format ms m = 
      m ++ 
      " (previous messages: " ++ show ms ++ ")"
    eOut = format <$> bMessages <@> eMessage
  return $ MessageOutput eOut
```

##

That prints a weird message when the message history is empty.

##

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] $ (\x xs -> x : xs) <$> eMessage
  let
    format ms m = 
      m ++ 
      " (previous messages: " ++ show ms ++ ")"
    bHasMessages = (not . null) <$> bMessages
    eMessageWithHistory = whenE bHasMessage eMessage
    eOut = leftmost [
        format <$> bMessages <@> eMessageWithHistory
      , eMessage
      ]
  return $ MessageOutput eOut
```

##

We should trim that history a little.

##

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] $ (\x xs -> x : xs) <$> eMessage
  let
    bTrimmed = take 3 <$> bMessages
    format ms m = 
      m ++ 
      " (previous messages: " ++ show ms ++ ")"
    bHasMessages = (not . null) <$> bMessages
    eMessageWithHistory = whenE bHasMessage eMessage
    eOut = leftmost [
        format <$> bMessages <@> eMessageWithHistory
      , eMessage
      ]
  return $ MessageOutput eOut
```

##

Hard-coding that 3 should make us a bit queasy.

##

```haskell
data LimitInput = LimitInput {
    lieLimitUp   :: Event ()
  , lieLimitDown :: Event ()
  }

data LimitOutput = LimitOutput {
    lobLimit :: Behavior Int
  }
```

##

`unions` is handy when accumulating from multiple events:
```haskell
accumE :: MonadMoment m 
       => a 
       -> Event (a -> a) 
       -> m (Event a) 

accumB :: MonadMoment m 
       => a 
       -> Event (a -> a) 
       -> m (Behavior a) 

unions :: [Event (a -> a)] -> Event (a -> a)
```

## 

```haskell
handleLimit :: LimitInput -> Moment LimitOutput
handleLimit (LimitInput eUp eDown) = do
  bLimit <- accumB 1 . unions $ [
      succ <$ eUp
    , pred <$ eDown
    ]
  return $ LimitOutput bLimit
```

##

Or, with `RecursiveDo`:

## 

```haskell
{-# LANGUAGE RecursiveDo #-}
handleLimit :: LimitInput -> Moment LimitOutput
handleLimit (LimitInput eUp eDown) = mdo
  let
    eDownNonNegative = whenE ((> 0) <$> bLimit) eDown
  bLimit <- accumB 1 . unions $ [
      succ <$ eUp
    , pred <$ eDownNonNegative
    ]
  return $ LimitOutput bLimit
```

##

Or, with both an `Event` and a `Behavior` for the limit:

##

```haskell
data LimitOutput = LimitOutput {
    loeLimit :: Event Int
  , lobLimit :: Behavior Int
  }

handleLimit :: LimitInput -> Moment LimitOutput
handleLimit (LimitInput eUp eDown) = do
   let
     apBoth f x = (f x, f x)
     eChange = unions [
         succ <$ eUp
       , (max 0 . pred) <$ eDown
       ]
  (eLimit, bLimit) <- mapAccum 1 $ apBoth <$> eChange
  return $ LimitOutput eLimit bLimit
```

##

Having both the `Event` and the the `Behavior` can lead to some efficiency wins.

##

Now we can make use of the `Behavior Int` that comes out of the limit component, without having to know any more about it.

##

```haskell
data MessageInput = MessageInput {
    mieRead  :: Event String
  , mibLimit :: Behavior Int
  }
```

##

```haskell
addMessage :: Int -> String -> [String] -> [String]
addMessage n m ms =
  take n (m : ms)

handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage bLimit) = do
  bMessage <- accumB [] $
    addMessage <$> bLimit <@> eMessage
  let
    format ms m = 
      m ++ 
      " (previous messages: " ++ show ms ++ ")"
    bHasMessages = (not . null) <$> bMessages
    eMessageWithHistory = whenE bHasMessage eMessage
    eOut = leftmost [
        format <$> bMessages <@> eMessageWithHistory
      , eMessage
      ]
  return $ MessageOutput eOut
```

# Components for a chat server

##

We want to prompt the user for a nickname, and then start processing commands.

##

We want the option to either stream notifications to the display, or to gather them all up until the user asks for them.

#

We have a few restrictions on nicknames:

- they can't be empty
- they should be one word
- they should not contain a '/' character, since we're using those for commands
- they should not be the same as the nickname of any of the other users



# Filtering and switching

##

TODO

# FRP and garbage collection

##

TODO

# A socket based server

##

TODO

# Conclusion

##

Still lots more to cover:

##

- Plenty of testing goodness

##

- Using this event network for a webserver

##

- The differences between `reactive-banana` and `reflex`

##

- Using `reflex` and `reflex-dom` for the front-end.
