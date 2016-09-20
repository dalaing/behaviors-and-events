---
title: An echo program
published: 2016-10-01 12:00:00+10:00
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
  eRead <- fromAddHandler . addHandler $ i    -- (1)

  let
    eWrite = eRead                            -- (2)

  reactimate $ putStrLn <$> eWrite            -- (3)
```
Notes:

1. Register for events when we have read a line.
2. We want to write whatever we have read.
3. Print the line 


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
    eMessage =       filterE (/= "/quit") eRead -- (1)
    eQuit    = () <$ filterE (== "/quit") eRead

  reactimate $ putStrLn    <$> eMessage
  reactimate $ exitSuccess <$  eQuit            -- (2)
```
Notes:

1. Life is simple when we only have one kind of command to deal with...
2. This is a terrible way to exit the event loop that is driving all of this, but we'll address that later.

So far, so good.

## Printing a message on exit
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Example3.hs)

Now let's say goodbye to the user before we go.

The change to print a message on exit is pretty small:
```haskell
networkDescription :: EventSource String -> MomentIO ()
networkDescription i = do
  eRead <- fromAddHandler . addHandler $ i

  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite   = leftmost [                        -- (1)
                 eMessage
      , "Bye" <$ eQuit 
      ]

  reactimate $ putStrLn    <$> eWrite            -- (2)
  reactimate $ exitSuccess <$  eQuit
```
Notes:

1. We have introduced an extra event here to collect the `Event String` events that we want to be printed. There are two events as inputs to this line of code and one event as an output - this is what people are talking about when they refer to an 'event network' or an 'event graph'.
2. The benefit to having the extra node in the event graph is that it keeps the code that has to interface with `IO` simple.

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
  eOpen <- fromAddHandler . addHandler $ o       -- (1)
  eRead <- fromAddHandler . addHandler $ r

  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead
    eWrite   = leftmost [
        "Hi" <$ eOpen                            -- (2)
      , eMessage
      , "Bye" <$ eQuit
      ]

  reactimate $ putStrLn    <$> eWrite
  reactimate $ exitSuccess <$  eQuit
```
Notes:

1. We have one change to the code that interfaces between `IO` and the event network. This will be the last of those for a while.
2. We have one change to the event network itself.

We just need to connect all of the pieces:
```haskell
go :: IO ()
go = do
  input <- mkInputSources                        -- (1)
  network <- compile $ networkDescription input
  actuate network
  eventLoop input
```
Notes:

1. This is the line that changes, since we've added to our set of inputs.

## Adding a help command
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Example5.hs)

Next we're going to add a help command.

```haskell
helpMessage :: String
helpMessage = 
  "/help              - displays this message\n" ++
  "/quit              - exits the program"

networkDescription :: InputSources -> MomentIO ()
networkDescription (InputSources o r) = do
  eOpen <- fromAddHandler . addHandler $ o
  eRead <- fromAddHandler . addHandler $ r

  let
    eMessage =  filterE ((/= "/") . take 1) eRead -- (1)
    eHelp    =   () <$ filterE (== "/help") eRead -- (2)
    eQuit    =   () <$ filterE (== "/quit") eRead
    eWrite = leftmost [
        "Hi"        <$ eOpen
      ,                eMessage
      , helpMessage <$ eHelp                    -- (3)
      , "Bye"       <$ eQuit
      ]

  reactimate $ putStrLn    <$> eWrite
  reactimate $ exitSuccess <$  eQuit
```
Notes:

1. We have more than one command now, so we work out whether or not something is a command by looking at the first character.
2. We need to spot the help command in our inputs.
3. We need to make the help message part of our outputs. 

## Detecting the use of unknown commands
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Example6.hs)

To top it off the functionality that we're going to start with, we're going to detect the use of unknown commands.

We'll introduce a helper function to split messages and commands - commands being the lines of inputs that start with a '/':
```haskell
type Message = String
type Command = String

command :: String -> Either Message Command
command ('/':xs) = Right xs
command xs       = Left xs
```
and another function to give us an error message if we encounter a command that we don't know how to handle:
```haskell
unknownMessage :: Command -> String
unknownMessage cmd =
  let
    commandError = case cmd of
      "" -> 
        "Command can not be an empty string."
      cmd ->
        "Unknown command: " ++ cmd ++ "."

    helpPrompt = 
      "\nType /help for options."
  in
    commandError ++ helpPrompt
```

The change to the event network is larger than the previous few:
```haskell
networkDescription :: InputSources -> MomentIO ()
networkDescription (InputSources o r) = do
  eOpen <- fromAddHandler . addHandler $ o
  eRead <- fromAddHandler . addHandler $ r

  let
    (eMessage, eCommand) = split $ command <$> eRead -- (1)

    eHelp    =   () <$ filterE (== "help")  eCommand
    eQuit    =   () <$ filterE (== "quit")  eCommand

    commands = ["help", "quit"]
    eUnknown = filterE (`notElem` commands) eCommand -- (2)

    eWrite = leftmost [
        "Hi"                  <$  eOpen
      ,                           eMessage
      , helpMessage           <$  eHelp
      , unknownCommandMessage <$> eUnknown           -- (3)
      , "Bye"                 <$  eQuit
      ]

  reactimate $ putStrLn    <$> eWrite
  reactimate $ exitSuccess <$ eQuit
```
Notes:

1. There is more event network creation happening here, as `split` has one event as an input and two events as `outputs`.  We don't have to deal with the leading '/' in the command processing anymore, since `command` strips that out.
2. Again, we spot the event of interest as it flows into our event network...
3. ... and we make sure that the relevant output flows out of our graph when that happens.

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

We have the opposite problem here - we want to convert a parametised input to a common output.

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
networkDescription' :: InputIO -> Moment OutputIO
networkDescription' (InputIO eOpen eRead) =
  let
    (eMessage, eCommand) = split $ command <$> eRead

    eHelp    =   () <$ filterE (== "help")  eCommand
    eQuit    =   () <$ filterE (== "quit")  eCommand

    commands = ["help", "quit"]
    eUnknown = filterE (`notElem` commands) eCommand

    eWrite = leftmost [
        "Hi"                  <$  eOpen
      ,                           eMessage
      , helpMessage           <$  eHelp
      , unknownCommandMessage <$> eUnknown
      , "Bye"                 <$  eQuit
      ]
  in
    return $ OutputIO eWrite eQuit
```
and we can recover our old network description with:
```haskell
networkDescription :: InputSources -> MomentIO ()
networkDescription =
  mkNetwork networkDescription'
```

Another option would be to give `pureNetworkDescription` a more abstract type:
```haskell
networkDescription' :: MonadMoment m => InputIO -> m OutputIO 
```
to allow for reuse in more contexts, but that would force us to provide a concrete type immediately in:
```haskell
networkDescription :: InputSources -> MomentIO ()
networkDescription = 
  mkNetwork $ networkDescription' :: InputIO -> Moment OutputIO
```


At this point we have gone from:

<img src="../images/photos/stuff.jpg" width="500" alt="stuff"/>

to:

<img src="../images/photos/io.jpg" width="500" alt="io"/>


## A little more separation
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Example8.hs)

We're currently not doing any IO in `networkDescription'`, but its boundaries are defined entirely by events related to IO.

We can add another layer of indirection here in order to get away from having to express things in those terms.

We start, as we usually do, by making a new data type.

This one collects our domain events:
```haskell
data Inputs = Inputs {
    ieOpen    :: Event ()
  , ieMessage :: Event String
  , ieHelp    :: Event ()
  , ieUnknown :: Event String
  , ieQuit    :: Event ()
  }
```

We then add separate out the code the translates the IO events to domain events:
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
    Inputs eOpen eMessage eHelp eUnknown eQuit
```

At the moment we won't be going to go quite as far with the outputs.

We could do something very fine-grained:
```haskell
data Outputs = Outputs {
    oeOpenWrite    :: Event String
  , oeMessageWrite :: Event String
  , oeHelpWrite    :: Event String
  , oeQuitWrite    :: Event String
  , oeUnknownWrite :: Event String
  , oeClose        :: Event ()
  }
```
and then think long and hard about how we're going to combine all of those events.

What we will do instead is handle how the various output IO events get combined, so that we don't have to think about those kind of things while we're working on the domain specific part of the event network.

- If there are multiple write events at the same time, we want to concatenate those values.
- If there are multiple quit events at the same time, we just want one of them.

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

It's an easier option for while we're prototyping, although once the dust settles you might want to refactor towards the more fine-grained output type in order to wring out any ambiguity from your event network.

We use these new data structures to simplify things event more:
```haskell
networkDescription'' :: MonadMoment m => Inputs -> m Outputs
networkDescription'' (Inputs eOpen eMessage eHelp eUnknown eQuit) =
  let
    eWrites = leftmost [
        "Hi"           <$  eOpen
      ,                    eMessage
      , helpMessage    <$  eHelp
      , unknownMessage <$> eUnknown
      , "Bye"          <$  eQuit
      ]
    eQuits = [
        eQuit
      ]
  in
    return $ Outputs eWrites eQuits
```
which can be transformed into what we had before by using the corresponding new functions:
```haskell
networkDescription' :: InputIO -> Moment OutputIO
networkDescription' i = do 
  o <- networkDescription'' . fanOut $ i
  return $ fanIn o
```

This time we have gone from:

<img src="../images/photos/io.jpg" width="500" alt="io"/>

to:

<img src="../images/photos/fan.jpg" width="500" alt="fan"/>


## A lot more separation
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Example9.hs)

The middle section of `networkDescription''` is still a bit of a mess.
We can clean this up by componentizing the various pieces of functionality that are in play.

Some of the FRP examples end up with pretty big event network all defined in the one place.
That can be fine once you're up to speed and now what you're doing.

One thing I took away from the Manning Functional Reactive Programming book was that you can clarify things a great deal by creating data types for the inputs and outputs of your logical components and pulling out the description of how the inputs relate to the outputs out of the main event network.

It is even more clear if you draw up block diagrams for the components.
We're going to see some good payoffs for that kind of thinking later on in the series.

What follows is overkill for this particular problem.
You can always use fewer components with more going on inside each one.

We're also giving all of the components a `MonadMoment` context.
Some of the changes coming later will introduce that context to various components, and adding it in now means we'll be able to alter the components innards later without having to change the code that uses them.

Some of these - in particular `handleMessage` - look pretty ridiculous.
On the plus side, the various strings are encapsulated inside their components, and the program is mostly about strings so far.
We'll be altering some of these soon, and that's where the payoff will be.

We have components for handling:

- the open event:

```haskell
data OpenInput  = OpenInput  { oieOpen  :: Event () }
data OpenOutput = OpenOutput { ooeWrite :: Event String }

handleOpen :: MonadMoment m => OpenInput -> m OpenOutput
handleOpen (OpenInput eOpen) =
  let
    eWrite = "Hi" <$ eOpen
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
    eWrite = helpMessage <$ eHelp
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
  return . UnknownOutput $ unknownMessage <$> eUnknown
```

We can stitch all of these together like so:
```haskell
domainNetworkDescription :: MonadMoment m => Inputs -> m Outputs
domainNetworkDescription (Inputs eOpen eMessage eHelp eQuit eUnknown) = do
  OpenOutput eoWrite        <- handleOpen    $ OpenInput eOpen
  MessageOutput emWrite     <- handleMessage $ MessageInput eMessage
  HelpOutput ehWrite        <- handleHelp    $ HelpInput eHelp
  QuitOutput eqWrite eqQuit <- handleQuit    $ QuitInput eQuit
  UnknownOutput euWrite     <- handleUnknown $ UnknownInput eUnknown

  return $ Outputs [eoWrite, emWrite, ehWrite, eqWrite, euWrite] [eqQuit]
```
and now we're free to tweak the internals of some of these without exposing everything to the body of `domainNetworkDescription.`


This takes us from:

<img src="../images/photos/fan.jpg" width="500" alt="fan"/>

to:

<img src="../images/photos/components.jpg" width="500" alt="components"/>

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
