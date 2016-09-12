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

What does it give us?

##

Composable systems of time-varying state and logic.

##

We are looking at `reactive-banana` today.

##

Part 2 will cover `reflex`.

##

Worth looking at `sodium` if you want this goodness in an other-than-Haskell flavour.

# Events

##

```haskell
data Event a = ...
```

```haskell
instance Functor Event
```

##

Events fire at infinitely-thin logical points in time.

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

The `Functor` instance demonstrates this - the output of `fmap` is active at the same points in time as the input.

##

\colsbegin

\column{.5\textwidth}

```haskell
flipper :: Event Colour 
        -> Event Colour
flipper eInput =
  let
    eOutput = 
      flip <$> eInput
  in
    eOutput
```
\column{.5\textwidth}

![](../images/fmap-flip.png)\

\colsend

##

\colsbegin

\column{.5\textwidth}

```haskell
blue :: Event Colour 
     -> Event Colour
blue eInput =
  let
    eOutput = 
      Blue <$ eInput
  in
    eOutput
```
\column{.5\textwidth}

![](../images/fmap-const.png)\

\colsend

##

There are functions to filter and split events.

##

The outputs are active at the same points in time as the inputs - when the outputs are active at all.

##

```haskell
filterE :: (a -> Bool) -> Event a -> Event a
```

\colsbegin

\column{.5\textwidth}

```haskell
red :: Event Colour 
    -> Event Colour
red eInput =
  let
    eOutput = 
      filterE isRed eInput
  in
    eOutput
```
\column{.5\textwidth}

![](../images/filterE.png)\

\colsend

##

```haskell
split :: Event (Either a b) -> (Event a, Event b)
```

\colsbegin

\column{.5\textwidth}

```haskell
type C = Colour
splitter :: Event (Either C C)
         -> (Event C, Event C)
splitter eInput =
  let
    (eLeft, eRight) = 
      split eInput
  in
    (eLeft, eRight)
```
\column{.5\textwidth}

![](../images/split.png)\

\colsend

##

We need to be aware of the potential for simultaneous events when we combine them.

##

```haskell
unionWith :: (a -> a -> a) 
          -> Event a -> Event a -> Event a
```

\colsbegin

\column{.5\textwidth}

```haskell
mixer :: Event Colour
      -> Event Colour
      -> Event Colour
mixer eInput1 eInput2 =
  let
    eOutput = 
      unionWith 
        mix 
        eInput1 
        eInput2
  in
    eOutput
```
\column{.5\textwidth}

![](../images/unionWith.png)\

\colsend

##

We can build a useful helper from this if you know you're not dealing with simultaneous events or if there is a clear priority between them.

##

```haskell
leftmost :: [Event a] -> Event a
leftmost = foldl (unionWith const) never
```

\colsbegin

\column{.5\textwidth}

```haskell
lister :: [Event Colour]
       -> Event Colour
lister eInput =
  let
    eOutput = 
      leftmost 
        eInput
  in
    eOutput
```
\column{.5\textwidth}

![](../images/leftmost.png)\

\colsend

##

There are other options for composing events:
```haskell
unions :: [Event (a -> a)] -> Event (a -> a)
```
which we'll look at later.

##

Let's do something significant with this.

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
    eFizzBuzz = unionWith (++) eFizz eBuzz
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

`Moment`/`MomentIO` is a builder monad for the event network.

##

These 'moments' are referred to as 'transactions' in the `sodium` literature.

##

We need a way to build a bridge between the 'inside' and 'outside' of an event network.

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
    eWrite = unionWith (++) eFizz eBuzz
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



    
```

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
accumE :: MonadMoment m 
       => a 
       -> Event (a -> a) 
       -> m (Event a) 
```

## 

\colsbegin

\column{.5\textwidth}

```haskell
toggler :: MonadMoment m 
        => Event ()
        -> m (Event Colour)
toggler eInput = do
  eOutput <- 
    accumE Red (flip <$ eInput)
  return eOutput
```
\column{.5\textwidth}

![](../images/accumE-flip.png)\

\colsend

##

\colsbegin

\column{.5\textwidth}

```haskell
counter :: MonadMoment m 
        => Event () 
        -> m (Event Int)
counter eInc = do
  eOutput <- 
    accumE 0 ((+ 1) <$ eInc)
  return eOutput
```

\column{.5\textwidth}

![](../images/accumE-count.png)\

\colsend

##

This is a good place to compare `leftmost` and `unions`.

## 

\colsbegin

\column{.5\textwidth}

```haskell
counter2 :: MonadMoment m 
         => Event () 
         -> Event () 
         -> m (Event Int)
counter2 eInc eDouble = do
  eOutput <- 
    accumE 0 $ lefmost [
        (+ 1) <$ eInc
      , (* 2) <$ eDouble
      ]
  return eOutput
```

\column{.5\textwidth}

![](../images/accumE-count-leftmost.png)\

\colsend

## 

\colsbegin

\column{.5\textwidth}

```haskell
counter3 :: MonadMoment m 
         => Event () 
         -> Event () 
         -> m (Event Int)
counter3 eInc eDouble = do
  eOutput <- 
    accumE 0 $ unions [
        (+ 1) <$ eInc
      , (* 2) <$ eDouble
      ]
  return eOutput
```

\column{.5\textwidth}

![](../images/accumE-count-unions.png)\

\colsend

##

Let's put that counter to use.

##

```haskell
networkDescription :: EventSource Int -> MomentIO ()
networkDescription c = do
  eCount <- fromAddHandler . addHandler $ c



  let
    eFizz = "Fizz" <$ multiple 3 eCount
    eBuzz = "Buzz" <$ multiple 5 eCount
    eWrite = unionWith (++) eFizz eBuzz
    showCount x =
      putStrLn $ "count: " ++ show x

  reactimate $ showCount <$> eCount
  reactimate $ putStrLn  <$> eWrite
```

##

```haskell
networkDescription :: EventSource ()  -> MomentIO ()
networkDescription t = do
  eTick  <- fromAddHandler . addHandler $ t



  let
    eFizz = "Fizz" <$ multiple 3 eCount
    eBuzz = "Buzz" <$ multiple 5 eCount
    eWrite = unionWith (++) eFizz eBuzz
    showCount x =
      putStrLn $ "count: " ++ show x

  reactimate $ showCount <$> eCount
  reactimate $ putStrLn  <$> eWrite
```
##

```haskell
networkDescription :: EventSource ()  -> MomentIO ()
networkDescription t = do
  eTick  <- fromAddHandler . addHandler $ t

  eCount <- accumE 0 ((+ 1) <$ eTick)

  let
    eFizz = "Fizz" <$ multiple 3 eCount
    eBuzz = "Buzz" <$ multiple 5 eCount
    eWrite = unionWith (++) eFizz eBuzz
    showCount x =
      putStrLn $ "count: " ++ show x

  reactimate $ showCount <$> eCount
  reactimate $ putStrLn  <$> eWrite
```

# A simple command line application

##

We're going to incrementally put together a program that echoes the input from the user.

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


networkDescription :: EventSource String -> MomentIO ()
networkDescription i = do
  eRead <- fromAddHandler . addHandler $ i

  let
    eMessage =                            eRead


  reactimate $ putStrLn    <$> eMessage
  
```

##

```haskell


networkDescription :: EventSource String -> MomentIO ()
networkDescription i = do
  eRead <- fromAddHandler . addHandler $ i

  let
    eMessage =       filterE (/= "/quit") eRead


  reactimate $ putStrLn    <$> eMessage
  
```

##

```haskell


networkDescription :: EventSource String -> MomentIO ()
networkDescription i = do
  eRead <- fromAddHandler . addHandler $ i

  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead

  reactimate $ putStrLn    <$> eMessage
  
```

##

```haskell
import System.Exit (exitSuccess) 

networkDescription :: EventSource String -> MomentIO ()
networkDescription i = do
  eRead <- fromAddHandler . addHandler $ i

  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead

  reactimate $ putStrLn    <$> eMessage
  reactimate $ exitSuccess <$  eQuit
```

##

Variant: Add a parting message

##

```haskell
networkDescription :: EventSource String -> MomentIO ()
networkDescription i = do
  eRead <- fromAddHandler . addHandler $ i

  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead



      

  reactimate $ putStrLn    <$> eMessage
  reactimate $ exitSuccess <$  eQuit
```

##

```haskell
networkDescription :: EventSource String -> MomentIO ()
networkDescription i = do
  eRead <- fromAddHandler . addHandler $ i

  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite =
                 eMessage
        


  reactimate $ putStrLn    <$> eMessage
  reactimate $ exitSuccess <$  eQuit
```

##

```haskell
networkDescription :: EventSource String -> MomentIO ()
networkDescription i = do
  eRead <- fromAddHandler . addHandler $ i

  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite =
                 eMessage
        


  reactimate $ putStrLn    <$> eWrite
  reactimate $ exitSuccess <$  eQuit
```

##

```haskell
networkDescription :: EventSource String -> MomentIO ()
networkDescription i = do
  eRead <- fromAddHandler . addHandler $ i

  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
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
networkDescription :: EventSource String -> MomentIO ()
networkDescription i                  = do

  eRead <- fromAddHandler . addHandler $ i

  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite = leftmost [

                 eMessage
      , "Bye" <$ eQuit
      ]

  reactimate $ putStrLn    <$> eWrite
  reactimate $ exitSuccess <$  eQuit
```

##

```haskell
networkDescription :: InputSources       -> MomentIO ()
networkDescription (InputSources o r) = do

  eRead <- fromAddHandler . addHandler $ i

  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite = leftmost [

                 eMessage
      , "Bye" <$ eQuit
      ]

  reactimate $ putStrLn    <$> eWrite
  reactimate $ exitSuccess <$  eQuit
```

##

```haskell
networkDescription :: InputSources       -> MomentIO ()
networkDescription (InputSources o r) = do

  eRead <- fromAddHandler . addHandler $ r

  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite = leftmost [

                 eMessage
      , "Bye" <$ eQuit
      ]

  reactimate $ putStrLn    <$> eWrite
  reactimate $ exitSuccess <$  eQuit
```

##

```haskell
networkDescription :: InputSources       -> MomentIO ()
networkDescription (InputSources o r) = do
  eOpen <- fromAddHandler . addHandler $ o
  eRead <- fromAddHandler . addHandler $ r

  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite = leftmost [

                 eMessage
      , "Bye" <$ eQuit
      ]

  reactimate $ putStrLn    <$> eWrite
  reactimate $ exitSuccess <$  eQuit
```

##

```haskell
networkDescription :: InputSources       -> MomentIO ()
networkDescription (InputSources o r) = do
  eOpen <- fromAddHandler . addHandler $ o
  eRead <- fromAddHandler . addHandler $ r

  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite = leftmost [
        "Hi"  <$ eOpen
      ,          eMessage
      , "Bye" <$ eQuit
      ]

  reactimate $ putStrLn    <$> eWrite
  reactimate $ exitSuccess <$  eQuit
```

##

Variant: Add a help command

##

```haskell
helpMessage :: String
helpMessage = 
  "/help              - displays this message\n" ++
  "/quit              - exits the program"
```

##

```haskell
  let
    eMessage =       filterE (/= "/quit") eRead

    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite = leftmost [
        "Hi"        <$ eOpen
      ,                eMessage

      , "Bye"       <$ eQuit
      ]
```

##

```haskell
  let
    eMessage =  filterE (/= "/" . take 1) eRead

    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite = leftmost [
        "Hi"        <$ eOpen
      ,                eMessage

      , "Bye"       <$ eQuit
      ]
```

##

```haskell
  let
    eMessage =  filterE (/= "/" . take 1) eRead
    eHelp    = () <$ filterE (== "/help") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite = leftmost [
        "Hi"        <$ eOpen
      ,                eMessage

      , "Bye"       <$ eQuit
      ]
```

##

```haskell
  let
    eMessage =  filterE (/= "/" . take 1) eRead
    eHelp    = () <$ filterE (== "/help") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite = leftmost [
        "Hi"        <$ eOpen
      ,                eMessage

      , "Bye"       <$ eQuit
      ]
```

##

```haskell
  let
    eMessage =  filterE (/= "/" . take 1) eRead
    eHelp    = () <$ filterE (== "/help") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite = leftmost [
        "Hi"        <$ eOpen
      ,                eMessage
      , helpMessage <$ eHelp
      , "Bye"       <$ eQuit
      ]
```

##

Variant: Deal with unknown commands

##

```haskell
type Message = String
type Command = String

command :: String -> Either Message Command
command ('/':xs) = Right xs
command xs       = Left xs
```

##

```haskell
unknownCommand :: Command -> String
unknownCommand cmd =
  let
    helpPrompt = "\nType /help for options."
    commandError = case cmd of
    case cmd of
      "" -> 
        "Command can not be an empty string."
      cmd ->
        "Unknown command: " ++ cmd ++ "."
  in
    commandError ++ helpPrompt
```

##

```haskell
  let
    eMessage =       filterE (/= "/" . take 1) eRead

    eHelp    =   () <$ filterE (== "/help")    eRead
    eQuit    =   () <$ filterE (== "/quit")    eRead
    
    
    
    
    eWrite = leftmost [
        "Hi"           <$  eOpen
      ,                    eMessage
      , helpMessage    <$  eHelp

      , "Bye"          <$  eQuit
      ]
```

##

```haskell
  let
    (eMessage, eCommand) = split $ command <$> eRead

    eHelp    =   () <$ filterE (== "/help")    eRead
    eQuit    =   () <$ filterE (== "/quit")    eRead
    
    
    
    
    eWrite = leftmost [
        "Hi"           <$  eOpen
      ,                    eMessage
      , helpMessage    <$  eHelp

      , "Bye"          <$  eQuit
      ]
```

##

```haskell
  let
    (eMessage, eCommand) = split $ command <$> eRead

    eHelp    =   () <$ filterE (== "/help") eCommand
    eQuit    =   () <$ filterE (== "/quit") eCommand
    
    
    
    
    eWrite = leftmost [
        "Hi"           <$  eOpen
      ,                    eMessage
      , helpMessage    <$  eHelp

      , "Bye"          <$  eQuit
      ]
```

##

```haskell
  let
    (eMessage, eCommand) = split $ command <$> eRead

    eHelp    =   () <$ filterE (== "help")  eCommand
    eQuit    =   () <$ filterE (== "quit")  eCommand
    
    
    
    
    eWrite = leftmost [
        "Hi"           <$  eOpen
      ,                    eMessage
      , helpMessage    <$  eHelp

      , "Bye"          <$  eQuit
      ]
```

##

```haskell
  let
    (eMessage, eCommand) = split $ command <$> eRead

    eHelp    =   () <$ filterE (== "help")  eCommand
    eQuit    =   () <$ filterE (== "quit")  eCommand

    commands = ["help", "quit"]
    eUnknown = filterE (`notElem` commands) eCommand

    eWrite = leftmost [
        "Hi"           <$  eOpen
      ,                    eMessage
      , helpMessage    <$  eHelp

      , "Bye"          <$  eQuit
      ]
```

##

```haskell
  let
    (eMessage, eCommand) = split $ command <$> eRead

    eHelp    =   () <$ filterE (== "help")  eCommand
    eQuit    =   () <$ filterE (== "quit")  eCommand

    commands = ["help", "quit"]
    eUnknown = filterE (`notElem` commands) eCommand

    eWrite = leftmost [
        "Hi"           <$  eOpen
      ,                    eMessage
      , helpMessage    <$  eHelp
      , unknownCommand <$> eUnknown
      , "Bye"          <$  eQuit
      ]
```

# Refactorings and API options

##

FRP code is usually pretty easy to refactor.

##

There isn't much information out there about what you should be refactoring towards.

##

Let's look at some options.

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
mkNetwork :: (InputIO -> Moment OutputIO) 
          -> InputSources 
          -> MomentIO ()
mkNetwork fn input = do
  i <- handleInput input
  o <- liftMoment $ fn i
  handleOutput o
```

## 

```haskell
networkDescription :: InputSources -> MomentIO ()
networkDescription (InputSources o r)    = do
  eOpen <- fromAddHandler . addHandler $ o
  eRead <- fromAddHandler . addHandler $ r

  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite = leftmost [
        "Hi" <$ eOpen
      , eMessage
      , "Bye" <$ eQuit
      ]



  reactimate $ putStrLn    <$> eWrite
  reactimate $ exitSuccess <$  eQuit
```

##

```haskell
networkDescription :: InputIO      -> MomentIO ()
networkDescription (InputSources o r)    = do
  eOpen <- fromAddHandler . addHandler $ o
  eRead <- fromAddHandler . addHandler $ r

  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite = leftmost [
        "Hi" <$ eOpen
      , eMessage
      , "Bye" <$ eQuit
      ]



  reactimate $ putStrLn    <$> eWrite
  reactimate $ exitSuccess <$  eQuit
```

##

```haskell
networkDescription :: InputIO      -> MomentIO ()
networkDescription (InputIO eOpen eRead) = do
  eOpen <- fromAddHandler . addHandler $ o
  eRead <- fromAddHandler . addHandler $ r

  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite = leftmost [
        "Hi" <$ eOpen
      , eMessage
      , "Bye" <$ eQuit
      ]



  reactimate $ putStrLn    <$> eWrite
  reactimate $ exitSuccess <$  eQuit
```

##

```haskell
networkDescription :: InputIO      -> MomentIO ()
networkDescription (InputIO eOpen eRead) = do



  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite = leftmost [
        "Hi" <$ eOpen
      , eMessage
      , "Bye" <$ eQuit
      ]



  reactimate $ putStrLn    <$> eWrite
  reactimate $ exitSuccess <$  eQuit
```

##

```haskell
networkDescription :: InputIO      -> Moment OutputIO
networkDescription (InputIO eOpen eRead) = do



  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite = leftmost [
        "Hi" <$ eOpen
      , eMessage
      , "Bye" <$ eQuit
      ]



  reactimate $ putStrLn    <$> eWrite
  reactimate $ exitSuccess <$  eQuit
```

##

```haskell
networkDescription :: InputIO      -> Moment OutputIO 
networkDescription (InputIO eOpen eRead) =



  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite = leftmost [
        "Hi" <$ eOpen
      , eMessage
      , "Bye" <$ eQuit
      ]
  in
    return $ 
      OutputIO 
                               eWrite 
                               eQuit
```
##

```haskell



networkDescription  :: InputIO      -> Moment OutputIO 
networkDescription  = ...
```

##

```haskell



networkDescription' :: InputIO      -> Moment OutputIO 
networkDescription' = ...
```

##

```haskell
networkDescription :: InputSources -> MomentIO ()
networkDescription = mkNetwork networkDescription'

networkDescription' :: InputIO      -> Moment OutputIO 
networkDescription' = ...
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
    (eMessage, eCommand) = split $ command <$> eRead

    eHelp    =   () <$ filterE (== "help")  eCommand
    eQuit    =   () <$ filterE (== "quit")  eCommand

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
data Behavior a = ...
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

If you squint, it looks a bit like `State a`.

##

Behaviors are pull-based.

##

Some of the time you'll use `Behavior` to model `State`.

##

Some of the time you'll use `Behavior` to pass around values that could be changed by parts of the program you don't care about.

##

We build `Behavior`s with `Event`s.

```haskell
stepper :: MonadMoment m 
        => a 
        -> Event a 
        -> m (Behavior a)
```

##

\colsbegin

\column{.5\textwidth}

```haskell
holder :: MonadMoment m 
       => Event Colour 
       -> m (Behavior Colour)
holder eInput = do
  eOutput <-
    stepper Blue eInput
  return eOutput
```
\column{.5\textwidth}

![](../images/stepper.png)\

\colsend

##

We sample `Behavior`s with `Event`s:
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

\colsbegin

\column{.5\textwidth}

```haskell
mixer :: Behavior Colour 
      -> Event Colour
      -> Event Colour
mixer bInput eInput =
  let
    eOutput = 
      mix <$> 
        bInput <@> 
        eInput
  in
    eOutput
```
\column{.5\textwidth}

![](../images/sample-mix.png)\

\colsend

##

\colsbegin

\column{.5\textwidth}

```haskell
tagger :: Behavior Colour 
       -> Event ()
       -> Event Colour
tagger bInput eInput =
  let
    eOutput = 
      bInput <@ eInput
  in
    eOutput
```
\column{.5\textwidth}

![](../images/sample-const.png)\

\colsend


##

We can filter `Event`s with `Behavior`s:
```haskell
whenE       :: Behavior Bool -> Event a -> Event a
filterApply :: Behavior (a -> Bool) -> Event a -> Event a
```

##

\colsbegin

\column{.5\textwidth}

```haskell
sifter :: Behavior Bool 
       -> Event Colour
       -> Event Colour
sifter bInput eInput =
  let
    eOutput = 
      whenE bInput eInput
  in
    eOutput
```
\column{.5\textwidth}

![](../images/whenE.png)\

\colsend

##

Let's look at a little example.

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

Now let's add some error handling.

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
{-# LANGUAGE RecursiveDo #-}
```

## 

```haskell
logIn  :: LogInState -> Either LogInError LogInState
logOut :: LogInState -> Either LogInError LogInState

logInHandler :: Event () 
             -> Event () 
             -> Moment ( Behavior LogInState
                       , Event    LogInError
                       )
logInHandler eLogIn eLogOut = mdo

  
  
  
  
  return (???        , ???        )
```
## 

```haskell
logIn  :: LogInState -> Either LogInError LogInState
logOut :: LogInState -> Either LogInError LogInState

logInHandler :: Event () 
             -> Event () 
             -> Moment ( Behavior LogInState
                       , Event    LogInError
                       )
logInHandler eLogIn eLogOut = mdo
  bLogInState <- stepper LoggedOut ???
  
  
  
  
  return (???        , ???        )
```

##

```haskell
logIn  :: LogInState -> Either LogInError LogInState
logOut :: LogInState -> Either LogInError LogInState

logInHandler :: Event () 
             -> Event () 
             -> Moment ( Behavior LogInState
                       , Event    LogInError
                       )
logInHandler eLogIn eLogOut = mdo
  bLogInState <- stepper LoggedOut ???
  
  
  
  
  return (bLogInState, ???        )
```

##

```haskell
logIn  :: LogInState -> Either LogInError LogInState
logOut :: LogInState -> Either LogInError LogInState

logInHandler :: Event () 
             -> Event () 
             -> Moment ( Behavior LogInState
                       , Event    LogInError
                       )
logInHandler eLogIn eLogOut = mdo
  bLogInState <- stepper LoggedOut ???
  (eLogInError, eLogInState) = split . leftmost $ [
      logIn  <$> bLogInState <@ eLogIn
      logOut <$> bLogInState <@ eLogOut
    ]
  return (bLogInState, ???        )
```

##

```haskell
logIn  :: LogInState -> Either LogInError LogInState
logOut :: LogInState -> Either LogInError LogInState

logInHandler :: Event () 
             -> Event () 
             -> Moment ( Behavior LogInState
                       , Event    LogInError
                       )
logInHandler eLogIn eLogOut = mdo
  bLogInState <- stepper LoggedOut ???
  (eLogInError, eLogInState) = split . leftmost $ [
      logIn  <$> bLogInState <@ eLogIn
      logOut <$> bLogInState <@ eLogOut
    ]
  return (bLogInState, eLogInError)
```

##

```haskell
logIn  :: LogInState -> Either LogInError LogInState
logOut :: LogInState -> Either LogInError LogInState

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

\colsbegin

\column{.5\textwidth}

```haskell
toggler :: MonadMoment m 
        => Event ()
        -> m (Behavior Colour)
toggler eInput = do
  eOutput <- 
    accumB Red (flip <$ eInput)
  return eOutput
```
\column{.5\textwidth}

![](../images/accumB-flip.png)\

\colsend

## 

\colsbegin

\column{.5\textwidth}

![](../images/accumE-flip.png)\

\column{.5\textwidth}

![](../images/accumB-flip.png)\

\colsend

##

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- stepper "" 
                          eMessage

  let
    format l  m = 
      m ++ 
      " (last message: " ++ l ++ ")"
      

    eOut = 
        format <$> bMessages <@> eMessage
        
        
  return $ MessageOutput eOut
```

##

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] $
    (\x xs -> x : xs) <$> eMessage
  let
    format l  m = 
      m ++ 
      " (last message: " ++ l ++ ")"
      
      
    eOut = 
        format <$> bMessages <@> eMessage
        
        
  return $ MessageOutput eOut
```

##

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] $ 
    (\x xs -> x : xs) <$> eMessage
  let
    format ls m = 
      m ++ 
      " (previous messages: " ++ show ls ++ ")"
      
      
    eOut = 
        format <$> bMessages <@> eMessage
        
        
  return $ MessageOutput eOut
```

##

That prints a weird message when the message history is empty.

##

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] $
    (\x xs -> x : xs) <$> eMessage
  let
    format ls m = 
      m ++ 
      " (previous messages: " ++ show ls ++ ")"
      
      
    eOut = 
        format <$> bMessages <@> eMessage
        
        
  return $ MessageOutput eOut
```

##

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] $
    (\x xs -> x : xs) <$> eMessage
  let
    format ls m = 
      m ++ 
      " (previous messages: " ++ show ls ++ ")"
    bHasMessages = (not . null) <$> bMessages
      
    eOut = 
        format <$> bMessages <@> eMessage
        
        
  return $ MessageOutput eOut
```

##

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] $
    (\x xs -> x : xs) <$> eMessage
  let
    format ls m = 
      m ++ 
      " (previous messages: " ++ show ls ++ ")"
    bHasMessages = (not . null) <$> bMessages
    eMessageWithHistory = whenE bHasMessage eMessage
    eOut = 
        format <$> bMessages <@> eMessage
        
        
  return $ MessageOutput eOut
```

##

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] $
    (\x xs -> x : xs) <$> eMessage
  let
    format ls m = 
      m ++ 
      " (previous messages: " ++ show ls ++ ")"
    bHasMessages = (not . null) <$> bMessages
    eMessageWithHistory = whenE bHasMessage eMessage
    eOut = 
        format <$> bMessages <@> eMessageWithHistory


  return $ MessageOutput eOut
```

##

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] $
    (\x xs -> x : xs) <$> eMessage
  let
    format ls m = 
      m ++ 
      " (previous messages: " ++ show ls ++ ")"
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
  bMessages <- accumB [] $
    (\x xs ->         x : xs ) <$> eMessage
  let
    format ls m = 
      m ++ 
      " (previous messages: " ++ show ls ++ ")"
    bHasMessages = (not . null) <$> bMessages
    eMessageWithHistory = whenE bHasMessage eMessage
    eOut = leftmost [
        format <$> bMessages <@> eMessageWithHistory
      , eMessage
      ]
  return $ MessageOutput eOut
```
##

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] $
    (\x xs -> take 3 (x : xs)) <$> eMessage
  let
    format ls m = 
      m ++ 
      " (previous messages: " ++ show ls ++ ")"
    bHasMessages = (not . null) <$> bMessages
    eMessageWithHistory = whenE bHasMessage eMessage
    eOut = leftmost [
        format <$> bMessages <@> eMessageWithHistory
      , eMessage
      ]
  return $ MessageOutput eOut
```

##

We do the trimming while creating the `Behavior`, to keep the storage size bounded.

##

Hard-coding that 3 should make us a bit queasy.

##

Digression: Let's build a component that will provide the message history limit.

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
    , (max 0 . pred) <$ eDown
    ]
  return $ LimitOutput bLimit
```

##

Or, with `RecursiveDo`:

## 

```haskell
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

  }
```

##

```haskell
data MessageInput = MessageInput {
    mieRead  :: Event String
  , mibLimit :: Behavior Int
  }
```

##

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage       ) = do
  bMessages <- accumB [] $
    (\  x xs ->        x : xs ) <$>             eMessage
  let
    format ls m = 
      m ++ 
      " (previous messages: " ++ show ls ++ ")"
    bHasMessages = (not . null) <$> bMessages
    eMessageWithHistory = whenE bHasMessage eMessage
    eOut = leftmost [
        format <$> bMessages <@> eMessageWithHistory
      , eMessage
      ]
  return $ MessageOutput eOut
```

##

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage bLimit) = do
  bMessages <- accumB [] $
    (\  x xs ->        (x : xs)) <$>            eMessage
  let
    format ls m = 
      m ++ 
      " (previous messages: " ++ show ls ++ ")"
    bHasMessages = (not . null) <$> bMessages
    eMessageWithHistory = whenE bHasMessage eMessage
    eOut = leftmost [
        format <$> bMessages <@> eMessageWithHistory
      , eMessage
      ]
  return $ MessageOutput eOut
```

##

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage bLimit) = do
  bMessages <- accumB [] $
    (\  x xs ->        (x : xs)) <$> bLimit <@> eMessage
  let
    format ls m = 
      m ++ 
      " (previous messages: " ++ show ls ++ ")"
    bHasMessages = (not . null) <$> bMessages
    eMessageWithHistory = whenE bHasMessage eMessage
    eOut = leftmost [
        format <$> bMessages <@> eMessageWithHistory
      , eMessage
      ]
  return $ MessageOutput eOut
```

##

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage bLimit) = do
  bMessages <- accumB [] $
    (\n x xs -> take n (x : xs)) <$> bLimit <@> eMessage
  let
    format ls m = 
      m ++ 
      " (previous messages: " ++ show ls ++ ")"
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

##

We have a few restrictions on nicknames:

- they can't be empty
- they should be one word
- they should not contain a '/' character, since we're using those for commands
- they should not be the same as the nickname of any of the other users

## 

```haskell
data NameInput = NameInput {
    nieOpen     :: Event ()
  , nieRead     :: Event String
  , nibGreeting :: Behavior String
  , nibNames    :: Behavior (S.Set String)
  }

data NameOutput = NameOutput {
    noeWrite :: Event String
  , noeName  :: Event String
  }
```
##

```haskell
data NameError =
    EmptyName
  | MultiWordName String
  | IllegalCharInName String
  | NameInUse String
  | NameNotInUse String
  deriving (Eq, Ord, Show)

nameErrorMessage :: NameError -> String

checkValidName :: String -> Either NameError String

checkValidNameNotInUse :: S.Set String 
                       -> String 
                       -> Either NameError String
checkValidNameInUse    :: S.Set String 
                       -> String 
                       -> Either NameError String
```

##

```haskell
handleName :: NameInput -> Moment NameOutput
handleName (NameInput eOpen eRead bGreeting bNames) = do
  let
    bPrompt = 
      pure "Please enter your name:"

    eGreeting = 
      mkGreeting <$> bGreeting <*> bPrompt <@ eOpen

    (eNameInvalid, eNameValid) = 
      split $ checkValidNameNotInUse <$> bNames <@> eRead

    eNameErrorMessage = 
      mkNameErrorMessage <$> bPrompt <@> eNameInvalid

    eWrite = 
      leftmost [eGreeting , eNameErrorMessage]

  return $ NameOutput eWrite eNameValid
```

##

```haskell
data CommandInput = CommandInput {
    cieOpen   :: Event String
  , cieRead   :: Event String
  , cieNotify :: Event Notification
  , cibNames  :: Behavior (S.Set User)
  , cibName   :: Behavior User
  }

data CommandOutput = CommandOutput {
    coeWrite  :: Event String
  , coeClose  :: Event ()
  , coeNotify :: Event Notification
  , coeKick   :: Event User
  }
```

##

```haskell
data Notification =
    NJoin User
  | NMessage User Message
  | NTell User User Message
  | NKick User User
  | NQuit User
  deriving (Eq, Ord, Show)
```

##

```haskell
data DomainInput = DomainInput {
    dieOpen    :: Event User
  , dieFetch   :: Event ()
  , dieMessage :: Event Message
  , dieTell    :: Event PrivateMessage
  , dieKick    :: Event User
  , dieHelp    :: Event ()
  , dieQuit    :: Event ()
  , dieUnknown :: Event String
  , dieNotify  :: Event Notification
  , dibNames   :: Behavior (S.Set User)
  , dibName    :: Behavior User
  }
```

##

```haskell
data DomainOutput = DomainOutput {
    doeWrite    :: [Event String]
  , doeClose    :: [Event ()]
  , doeNotifies :: [Event Notification]
  , doeKick     :: Event User
  }
```

##

```haskell
networkDescription :: DomainInput 
                   -> Moment DomainOutput
networkDescription nt di = do
  -- set up all the components
  let
    eWrites = [
      -- gather all the output that
      -- only goes to the user
      ]
    eCloses = [
      -- gather all of the close events
      ]
    eNotifies = [ 
      -- gather all of the outgoing notifications
      ]

  return $ 
    DomainOutput eWrites eCloses eNotifies eKickValid
```
##

```haskell
data MessageInput = MessageInput {
    mibName    :: Behavior User
  , mieMessage :: Event String
  }

data MessageOutput = MessageOutput {
    moeNotify :: Event Notification
  }

handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput bName eMessage) = do
  let
    eNotify = NMessage <$> bName <@> eMessage
  return $ MessageOutput eNotify
```

##

```haskell
data KickInput = KickInput {
    kibNames :: Behavior (S.Set User)
  , kibName  :: Behavior User
  , kieName  :: Event String
  }

data KickOutput = KickOutput {
    koeKickValid :: Event User
  , koeNotify    :: Event Notification
  , koeError     :: Event String
  }
```

##

```haskell
checkNotSelf :: User -> User -> Either String User
checkNotSelf name target
  | name == target = Left "Stop kicking yourself"
  | otherwise      = Right target

checkValidNameInUse    :: S.Set String 
                       -> String 
                       -> Either NameError String
```

##

```haskell
handleKick :: KickInput -> Moment KickOutput
handleKick (KickInput bNames bName eName) = do
  let
    (eKickSelfError, eNotSelf) = 
      split $ checkNotSelf <$> bName <@> eName
    (eNameInvalid, eNameValid) = 
      split $ checkValidNameInUse <$> bNames <@> eNotSelf

    eNotify = NKick <$> bName <@> eNameValid
    eError = leftmost [
        eKickSelfError
      , nameErrorMessage <$> eNameInvalid
      ]
  return $ KickOutput eNameValid eNotify eError
```

##

```haskell
data NotificationType =
    Stream
  | Batch (Behavior Int)

data NotificationInput = NotificationInput {
    nibName         :: Behavior User
  , nieFetch        :: Event ()
  , nieNotification :: Event Notification
  }

data NotificationOutput = NotificationOutput {
    noeNotifications :: Event String
  }
```

##

```haskell
handleNotificationStream :: NotificationInput 
                         -> Moment (Event [Notification])
handleNotificationStream input =
  return $ pure <$> nieNotification input
```

##

```haskell
addToBoundedList :: Int -> a -> [a] -> [a]
addToBoundedList limit x xs =
  take limit (x : xs)

handleNotificationBatch :: Behavior Int 
                        -> NotificationInput 
                        -> Moment (Event [Notification])
handleNotificationBatch bLimit input = do
  bNotifications <- accumB [] . unions $ [
      addToBoundedList <$> 
        nibLimit input <@> 
        nieNotification input
    , const [] <$ nieFetch input
    ]
  return $ reverse <$> bNotifications <@ nieFetch input
```

# Filtering and switching

##

First we use the name prompting component.

##

Then we use the command processing component.

##

The first way we can do this is to filter the input events.

##

```haskell
networkDescription :: InputIO -> Moment OutputIO
networkDescription (InputIO eOpen eRead) = mdo
  let
    bGreeting = pure "Welcome to the chat server."
    enRead = whenE ((== NamePrompting) <$> bPhase) eRead

  NameOutput enWrite eName <- 
    handleName $ NameInput eOpen enRead bGreeting bNames

  bNames <- accumB 
    (S.fromList ["root", "admin"]) 
    (S.insert <$> eName)
  bName <- stepper "" eName

  bPhase <- stepper PreOpen . leftmost $ [
      NamePrompting <$ eOpen
    , CommandProcessing <$ eName
    ]
  ...
```

##

```haskell
  ...
  CommandOutput ecWrite eClose ecNotify _ <- 
    handleCommand Stream $ 
      CommandInput eName ecRead ecNotify bNames bName

  let
    ecRead = 
      whenE ((== CommandProcessing) <$> bPhase) eRead

  let
    eWrite = leftmost [enWrite, ecWrite]
  return $ OutputIO eWrite eClose
```

##

TODO - intro switching

##

TODO switchB

##

TODO switchE

##

TODO switching in other libraries

##

TODO Switch typeclass

##

TODO Switch instance for OutputIO

##

```haskell
  ...
  let
    nameOut = OutputIO enWrite never
    cmdOut = OutputIO ecWrite ecClose

  switch nameOut (cmdOut <$ eName)
```

##

Important to remember to switch out the event that does the switching.

##

A better option is to filter inputs _and_ to switch the outputs.

##

This is often used when you are changing between implementations of an interface based on an event.

##

TODO - switching blocks

# FRP and garbage collection

##

TODO - cover the rules rather than going through how I worked them out

# A socket based server

##

TODO - hooking up the IO

##

TODO - the bit of the network that coordinates the collection of components-per-client

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

