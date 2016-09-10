---
title: An echo program
published: 2016-09-01 12:00:00+10:00
---
  
[Previously](./events.html) we looked at how events work in FRP, and built up a little program to test it out.
  
# An echo program

We're going to start small, building a command line application that echos the input it receives, and we're going to iterate from there.

## Starting at the beginning
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Example1.hs)

Given what we're trying to achieve, our event loop is straightforward.

We read a line, fire an event with the line we read, and repeat:
```haskell
eventLoop :: EventSource String -> IO ()
eventLoop i =
  forever $ do
    x <- getLine
    fire i x
```

The event network is probably the simplest one that we'll see:
```haskell
networkDescription :: EventSource String -> MomentIO ()
networkDescription i = do
  -- register for events when we have read a line
  eRead <- fromAddHandler . addHandler $ i

  let
    -- we want to write whatever we have read
    eWrite = eRead

  -- write the line 
  reactimate $ putStrLn <$> eWrite
```

We glue these two together in the usual manner:
```haskell
go :: IO ()
go = do
  input <- mkEventSource
  network <- compile $ networkDescription input
  actuate network
  eventLoop input
```

We can improve this program quite a bit.

## Adding the ability to quit
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Example2.hs)

Let us start by adding the ability to quit the program when the user types "/quit":
```haskell
import System.Exit (exitSuccess) 

networkDescription :: EventSource String -> MomentIO ()
networkDescription i = do
  eRead <- fromAddHandler . addHandler $ i

  let
    eMessage = filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead

  reactimate $ putStrLn <$> eMessage
  reactimate $ exitSuccess <$ eQuit
```

So far, so good.

## Printing a message on exit
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Example3.hs)

Now let's say goodbye to the user before we go.

We're going to introduce some helper functions here, to help deal with multiple non-simultaneous events.
We know they're non-simultaneous because they're all going to be derived from `eRead`.

First we have
```haskell
orElse :: Event a -> Event a -> Event a
orElse = unionWith const
```
which picks the first of two events if they are activated simultaneously.

We can extend that to a list of events with
```haskell
leftmost :: [Event a] -> Event a
leftmost = foldl orElse never
```
which I tend to use more than `orElse`, since I tend to refactor FRP programs and it's quicker to add or remove things from a list than it is to chain together a heap of events using `orElse`.

We're using `never` in there, which is from `reactive-banana`, and is an event that never fires.
It's handy for things like the `leftmost`.

The change to print a message on exit is pretty small:
```haskell
networkDescription :: EventSource String -> MomentIO ()
networkDescription i = do
  eRead <- fromAddHandler . addHandler $ i

  let
    eMessage = filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead

  reactimate $ fmap putStrLn . leftmost $ [
      eMessage
    , "Bye" <$ eQuit
    ]
  reactimate $ exitSuccess <$ eQuit
```

## Printing a greeting
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Example4.hs)

We've said goodbye, so we should probably also say hello.

In order to do that, our event network is going to need to know when the program has started.

We'll add a new `EventSource` for that, and we'll collect the `EventSource`s together into a data structure:
```haskell
data InputSources = InputSources {
    isOpen :: EventSource ()
  , isRead :: EventSource String
  }
```

It's pretty easy to build one of these:
```haskell
mkInputSources :: IO InputSources
mkInputSources =
  InputSources <$> mkEventSource <*> mkEventSource
```

From the outside of the event network, we need to fire the open event from the event loop:
```haskell
eventLoop :: InputSources -> IO ()
eventLoop (InputSources o r) = do
  fire o ()
  forever $ do
    x <- getLine
    fire r x
```

From the inside of the event network, we need to register for open events and use those events to print a greeting:
```haskell
networkDescription :: InputSources -> MomentIO ()
networkDescription (InputSources o r) = do
  eOpen <- fromAddHandler . addHandler $ o
  eRead <- fromAddHandler . addHandler $ r

  let
    eMessage = filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead

  reactimate $ fmap putStrLn . leftmost $ [
      "Hi" <$ eOpen
    , eMessage
    , "Bye" <$ eQuit
    ]
  reactimate $ exitSuccess <$ eQuit
```

Then we just need to hook everything up:
```haskell
go :: IO ()
go = do
  input <- mkInputSources
  network <- compile $ networkDescription input
  actuate network
  eventLoop input
```

## Adding a help command
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Example5.hs)

Next we're going to add a help command:
```haskell
networkDescription :: InputSources -> MomentIO ()
networkDescription (InputSources o r) = do
  eOpen <- fromAddHandler . addHandler $ o
  eRead <- fromAddHandler . addHandler $ r

  let
    eMessage = filterE (/= "/" . take 1) eRead
    eHelp    = () <$ filterE (== "/help") eRead
    eQuit    = () <$ filterE (== "/quit") eRead

  reactimate $ fmap putStrLn . leftmost $ [
      "Hi (type /help for instructions)" <$ eOpen
    , eMessage
    , "/help displays this message\n/quit exits the program" <$ eHelp
    , "Bye" <$ eQuit
    ]
  reactimate $ exitSuccess <$ eQuit
```

This involves altering `eMessage` so that it stays out of the way of the other commands.

## Detecting the use of unknown commands
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Example6.hs)

To top it off the functionality that we're going to start with, we're going to detect the use of unknown commands.

```haskell
networkDescription :: InputSources -> MomentIO ()
networkDescription (InputSources o r) = do
  eOpen <- fromAddHandler . addHandler $ o
  eRead <- fromAddHandler . addHandler $ r

  let
    eMessage = filterE ((/= "/") . take 1) eRead
    eCommand = fmap (drop 1) . filterE ((== "/") . take 1) $ eRead
    eHelp    = () <$ filterE (== "help") eCommand
    eQuit    = () <$ filterE (== "quit") eCommand

    commands        = ["help", "quit"]
    eUnknownCommand = filterE (`notElem` commands) eCommand

  reactimate $ fmap putStrLn . leftmost $ [
      "Hi (type /help for instructions)" <$ eOpen
    , eMessage
    , "/help displays this message\n/quit exits the program" <$ eHelp
    , "Bye" <$ eQuit
    , (\x -> "Unknown command: " ++ x ++ " (type /help for instructions)") <$> eUnknownCommand
    ]
  reactimate $ exitSuccess <$ eQuit
```

We've also cleaned up a few bits and pieces along the way.

## Separating out the IO
##### [(From this point on, some of the common pieces of code have been pulled out and placed here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Common.hs)
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Example7.hs)

We are currently doing IO whenever we like in our event network.

This can be _really_ handy in some circumstances, but it can be a bit of a burden if you have an event network that you want to test.

In aid of that, we're going to separate out the bits of the event network that deal with IO and the bits of the event network that are pure.

We did that a little bit in the first post, although it was done in a pretty ad-hoc manner.

For the inputs, the only IO that we're doing is registering the event handlers.
We can can take care of that with a data structure and a function to populate it:
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

The outputs all caused by `reactimate` and occur in the `MomentIO` monad.
We handle this in a similar fashion to how we packaged up the inputs:
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

From there, we'll write a function to package these up.

We want something like this:
```haskell
mkNetwork :: ??? m => (InputIO -> m OutputIO) -> InputSources -> MomentIO ()
mkNetwork fn input = do
  i <- handleInput input
  o <- ??? $ fn i
  handleOutput o
```
where the typeclass constraint has instances for both `Moment` and `MomentIO`.

We've already seen:
```haskell
class Monad m => MonadMoment m where
  liftMoment :: Moment a -> m a
```
with instances for `Moment` and `MomentIO`.

This is used that the various combinators can be written once in terms of `Moment` and will work in either pure event networks - the `Moment` monad - or in event networks that also do some IO - the `MomentIO` monad.

It is for converting a common input to a parametised output.

We have the opposite problem here.
We want to convert a parametised input to a common output.

So we'll write a typeclass:
```haskell
class MonadMoment m => MonadMomentIO m where
  toMomentIO :: m a -> MomentIO a
```
and the instances that we need:
```haskell
instance MonadMomentIO Moment where
  toMomentIO = liftMoment

instance MonadMomentIO MomentIO where
  toMomentIO = id
```

After that, we plug it in and hit the switch:
```haskell
mkNetwork :: MonadMomentIO m => (InputIO -> m OutputIO) -> InputSources -> MomentIO ()
mkNetwork fn input = do
  i <- handleInput input
  o <- toMomentIO $ fn i
  handleOutput o
```

The event network that we are currently working with could actually have the type `InputIO -> OutputIO`, but we're adding the `MonadMoment` context into that function to be able to handle a wider variety of event networks.

Our network description is now free from IO:
```haskell
pureNetworkDescription :: InputIO -> Moment OutputIO 
pureNetworkDescription (InputIO eOpen eRead) =
  let
    eMessage = filterE ((/= "/") . take 1) eRead
    eCommand = fmap (drop 1) . filterE ((== "/") . take 1) $ eRead
    eHelp    = () <$ filterE (== "help") eCommand
    eQuit    = () <$ filterE (== "quit") eCommand

    commands        = ["help", "quit"]
    eUnknownCommand = filterE (`notElem` commands) eCommand

    eWrite = leftmost [
        "Hi (type /help for instructions)" <$ eOpen
      , eMessage
      , "/help displays this message\n/quit exits the program" <$ eHelp
      , "Bye" <$ eQuit
      , (\x -> "Unknown command: " ++ x ++ " (type /help for instructions)") <$> eUnknownCommand
      ]
  in
    return $ OutputIO eWrite eQuit
```
and we can recover our old network description with:
```haskell
networkDescription :: InputSources -> MomentIO ()
networkDescription = 
  mkNetwork pureNetworkDescription
```

Another option would be to give `pureNetworkDescription` a more abstract type:
```haskell
pureNetworkDescription :: MonadMoment m => InputIO -> m OutputIO 
```
to allow for reuse in more contexts, but that would force us to provide a concrete type immediately in:
```haskell
networkDescription :: InputSources -> MomentIO ()
networkDescription = 
  mkNetwork $ pureNetworkDescription :: InputIO -> Moment OutputIO
```

## A little more separation
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Example8.hs)

We're currently not doing any IO in `pureNetworkDescription`, but its boundaries are defined entirely by events related to IO.

We can add another layer of indirection here in order to get away from having to express things in those terms.

We start, as we usually do, by making a new data type.

This one collects our domain events:
```haskell
data Inputs = Inputs {
    ieOpen           :: Event ()
  , ieMessage        :: Event String
  , ieHelp           :: Event ()
  , ieQuit           :: Event ()
  , ieUnknownCommand :: Event String
  }
```

We then add separate out the code the translates the IO events to domain events:
```haskell
fanOut :: InputIO -> Inputs
fanOut (InputIO eOpen eRead) =
  let
    eReadNonEmpty =
      filterE (not . null) eRead

    isMessage =
      (/= "/") . take 1
    eMessage =
      filterE isMessage eReadNonEmpty

    isCommand =
      (== "/") . take 1
    eCommand =
      fmap (drop 1) . filterE isCommand $ eReadNonEmpty

    eHelp = () <$ filterE (== "help") eCommand
    eQuit = () <$ filterE (== "quit") eCommand

    commands =
      ["help", "quit"]
    eUnknownCommand =
      filterE (`notElem` commands) eCommand
  in
    Inputs eOpen eMessage eHelp eQuit eUnknownCommand
```

At the moment we won't be able to go quite as far with the outputs.

What we can do is handle how the various output IO events get combined, so that we don't have to think about those kind of things while we're working on the domain specific part of the event network.

If there are multiple write events at the same time, we want to concatenate those values.
If there are multiple quit events at the same time, we just want one of them.

We capture the events we want to combine in a data structure:
```haskell
data Outputs = Outputs {
    oeWrite :: [Event String]
  , oeClose :: [Event ()]
  }
```
and do the combining a function:
```haskell
fanIn :: Outputs -> OutputIO
fanIn (Outputs eWrites eCloses) =
  let
    addLine x y = x ++ '\n' : y
    eCombinedWrites = foldr (unionWith addLine) never eWrites
    eCombinedCloses = () <$ leftmost eCloses
  in
    OutputIO eCombinedWrites eCombinedCloses
```

We use these new data structures to define another version of our event network:
```haskell
domainNetworkDescription :: MonadMoment m => Inputs -> m Outputs
domainNetworkDescription (Inputs eOpen eMessage eHelp eQuit eUnknownCommand) =
  let
    eWrites = [
        "Hi (type /help for instructions)" <$ eOpen
      , eMessage
      , "/help displays this message\n/quit exits the program" <$ eHelp
      , "Bye" <$ eQuit
      , (\x -> "Unknown command: " ++ x ++ " (type /help for instructions)") <$> eUnknownCommand
      ]
    eQuits = [
        eQuit
      ]
  in
    return $ Outputs eWrites eQuits
```
which can be transformed into what we had before by using the corresponding new functions:
```haskell
pureNetworkDescription :: InputIO -> Moment OutputIO
pureNetworkDescription i = do 
  o <- domainNetworkDescription . fanOut $ i
  return $ fanIn o
```

## A lot more separation
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Example9.hs)

The middle section of `domainNetworkDescription` is still a bit of a mess.
We can clean this up by componentizing the various pieces of functionality that are in play.

Some of the FRP examples end up with pretty big event network all defined in the one place.
That can be fine once you're up to speed and now what you're doing.

One thing I took away from the Manning Functional Reactive Programming book was that you can clarify things a great deal by creating data types for the inputs and outputs of your logical components and pulling out the description of how the inputs relate to the outputs out of the main event network.

It is even more clear if you draw up block diagrams for the components.
They're a bit too labour intensive to prepare for this block, but scratching out some block diagrams on pen and paper can be really handy if you get stuck.

We're going to see some good payoffs for that kind of thinking later on in the series.

What follows is overkill for this particular problem.
You can always use fewer components with more going on inside each one.

We're also giving all of the components a `MonadMoment` context.
Some of the changes coming later will introduce that context to various components, and adding it in now means we'll be able to alter the components innards later without having to change the code that uses them.

Some of these - in particular `handleMessage` - look pretty ridiculous.
On the plus side, the various strings are encapsulated inside their components, and the program is mostly about strings so far.

We have components for handling:

- the open event:

```haskell
data OpenInput  = OpenInput  { oieOpen  :: Event () }
data OpenOutput = OpenOutput { ooeWrite :: Event String }

handleOpen :: MonadMoment m => OpenInput -> m OpenOutput
handleOpen (OpenInput eOpen) =
  let
    eWrite = "Hi (type /help for instructions)" <$ eOpen
  in
    return $ OpenOutput eWrite
```

- the message event:

```haskell
data MessageInput  = MessageInput  { mieRead  :: Event String } 
data MessageOutput = MessageOutput { moeWrite :: Event String }

handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) =
  return $ MessageOutput eMessage
```

- the help event:

```haskell
data HelpInput  = HelpInput  { hieHelp  :: Event () }
data HelpOutput = HelpOutput { hoeWrite :: Event String }

handleHelp :: MonadMoment m => HelpInput -> m HelpOutput
handleHelp (HelpInput eHelp) =
  let
    eWrite = "/help displays this message\n/quit exits the program" <$ eHelp
  in
    return $ HelpOutput eWrite
```

- the quit event:

```haskell
data QuitInput = QuitInput { 
    qieQuit :: Event ()
  }

data QuitOutput = QuitOutput { 
    qoeWrite :: Event String
  , qoeQuit  :: Event ()
  }

handleQuit :: MonadMoment m => QuitInput -> m QuitOutput
handleQuit (QuitInput eQuit) =
  let
    eWrite = "Bye" <$ eQuit
  in
    return $ QuitOutput eWrite eQuit
```

- and the unknown command event:

```haskell
data UnknownInput  = UnknownInput  { ucieCommand :: Event String }
data UnknownOutput = UnknownOutput { ucoeWrite   :: Event String }

handleUnknown :: MonadMoment m => UnknownInput -> m UnknownOutput
handleUnknown (UnknownInput eUnknown) =
  let
    msg x = "Unknown command: " ++ x ++ " (type /help for instructions)"
  in
    return . UnknownOutput $ msg <$> eUnknown
```

We can stitch all of these together like so:
```haskell
domainNetworkDescription :: MonadMoment m => Inputs -> m Outputs
domainNetworkDescription (Inputs eOpen eMessage eHelp eQuit eUnknown) = do
  OpenOutput eoWrite        <- handleOpen $ OpenInput eOpen
  MessageOutput emWrite     <- handleMessage $ MessageInput eMessage
  HelpOutput ehWrite        <- handleHelp $ HelpInput eHelp
  QuitOutput eqWrite eqQuit <- handleQuit $ QuitInput eQuit
  UnknownOutput euWrite     <- handleUnknown $ UnknownInput eUnknown
  return $ Outputs [eoWrite, emWrite, ehWrite, eqWrite, euWrite] [eqQuit]
```
and now we're free to tweak the internals of some of these without exposing everything to the body of `domainNetworkDescription.`

## Refactoring for testing
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Example10.hs)

We've already come across `interpret`:
```haskell
interpret :: (Event a -> Moment (Event b)) -> [Maybe a] -> IO [Maybe b] 
```
and used it in a setting where we only had one input event and one output event.

If we're going to make use of it in with this program though, we're going to need to do some acrobatics.

We're up for the challenge.
The first thing to do will be to make data types for our inputs and outputs:
```haskell
data InputIOCmd =
    IOOpen
  | IORead String
  deriving (Eq, Ord, Show)

data OutputIOCmd =
    IOWrite String
  | IOClose
  deriving (Eq, Ord, Show)
```

We are trying to test a network of type `InputIO -> Moment OutputIO` using `Event InputIOCmd` as inputs and `Event [OutputIOCmd]` as outputs.
The list of `OutputIOCmd`s is needed to be able to check if certain outputs are happening simultaneously.

We'll write a function to get from `Event InputIOCmd` to `InputIO`:
```haskell
fanInput :: Event InputIOCmd -> InputIO
fanInput eIn =
  let
    maybeOpen IOOpen = Just ()
    maybeOpen _    = Nothing
    eOpen = filterJust $ maybeOpen <$> eIn

    maybeRead (IORead x) = Just x
    maybeRead _ = Nothing
    eRead = filterJust $ maybeRead <$> eIn
  in
    InputIO eOpen eRead
```

This uses `filterJust`:
```haskell
filterJust :: Event (Maybe a) -> Event a 
```
which passes through the `Just` values and filters out the `Nothing` values.


We'll also write a function to get from `OutputIO` to `Event [OutputIOCmd]`
```haskell
mergeOutput :: OutputIO -> Event [OutputIOCmd]
mergeOutput (OutputIO eWrite eClose) =
  unionWith (++)
    ((\x -> [IOWrite x]) <$> eWrite)
    ([IOClose] <$ eClose)
```

We can package this up as:
```haskell
testNetwork :: Testable m => (InputIO -> m OutputIO) -> [Maybe InputIOCmd] -> IO [Maybe [OutputIOCmd]]
testNetwork fn =
  interpretEvents $ \i -> do
    o <- fn . fanInput $ i
    return $ mergeOutput o
```
making use of another typeclass that we concocted in the last post:
```haskell
class MonadMoment m => Testable m where
  interpretEvents :: (Event a -> m (Event b)) -> [Maybe a] -> IO [Maybe b]
```

With some formatting liberties in the output, it behaves admirably when we take it for a spin:
```haskell
> output <- testNetwork pureNetworkDescription [
    Just (IORead "one")
  , Nothing
  , Just (IORead "two")
  , Just (IORead "/quit")
  ]
> output
[ Just [IOWrite "one"]
, Nothing
, Just [IOWrite "two"]
, Just [IOWrite "Bye", Close]
]
```

This looks pretty handy for use with `QuickCheck` or `HUnit`.

We can also do this with the domain specific events.

The code is pretty similar to what we have above, and you can check it out in the linked sample code.

```haskell
class Fannable i where
  type ToFan i
  fanInput :: Testable m => Event (ToFan i) -> m i

class Mergable o where
  type Merged o
  mergeOutput :: o -> Event (Merged o)

testNetwork :: (Testable m, Fannable i, Mergable o) => (i -> m o) -> [Maybe (ToFan i)] -> IO [Maybe (Merged o)]
testNetwork fn =
  interpretEvents $ \i -> do
    fi <- fanInput i
    o <- fn fi
    return $ mergeOutput o
```

With that, we can do:
```haskell
> output <- testNetwork domainNetworkDescription [
    Just Open
  , Just (Message "testing...")
  , Just Quit
  ]
> output
[ Just [Write "Hi (type /help for instructions)"]
, Just [Write "testing..."]
, Just [Write "Bye",OCClose]
]
```

We can also do this for the various components we've come across so far - mostly because their inputs and outputs have been made up of events, so the fanning and merging functions are usually doable.
We'll have to get a little trickier later on though.

## Next up

The next thing we'll look at is behaviors, which will help us deal with more complex scenarios in a more modular way.

[Onwards!](./behaviors.html)
