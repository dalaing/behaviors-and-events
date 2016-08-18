---
title: Refactoring the echo program
published: 2016-09-01 12:00:00+10:00
---
  
[Previously](./events.html) we looked at how events work in FRP, and built up a little program to echo text to the user.
  
# An echo program

Now we're going to take that program and progressively alter it, in order to separate out the IO that the program performs, to make some of the pieces of the program easier to reuse, and to make the program testable.

As a reminder, we're starting with this:
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
## Separating out the IO
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Example1.hs)

We are currently doing IO whenever we like in our event network.

This can be _really_ handy in some circumstances, but it can be a bit of a burden if you have an event network that you want to test.

In aid of that, we're going to separate out the bits of the event network that deal with IO and the bits of the event network that are pure.

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

From there, we write a function to package these up:
```haskell
mkNetwork :: MonadMoment m => (InputIO -> m OutputIO) -> InputSources -> MomentIO ()
mkNetwork fn input = do
  i <- handleInput input
  o <- liftMoment $ fn i
  handleOutput o
```

Our current event network actually could have the type `InputIO -> OutputIO`, but we're adding the `MonadMoment` context into that function to be able to handle a wider variety of event networks.

We're making use of `liftMoment` from the `MonadMoment` class in that function to make the jump to `MomentIO`.

Our network description is now free from IO:
```haskell
pureNetworkDescription :: MonadMoment m => InputIO -> m OutputIO 
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

## A little more separation
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Example2.hs)

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
(and clean it up a little as we go).

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
pureNetworkDescription :: MonadMoment m => InputIO -> m OutputIO
pureNetworkDescription i = do 
  o <- domainNetworkDescription . fanOut $ i
  return $ fanIn o
```

## A lot more separation
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Example3.hs)

The middle section of `domainNetworkDescription` is still a bit of a mess.
We can clean this up by componentizing the various pieces of functionality that are in play.

Some of the FRP examples end up with pretty big event network all defined in the one place.
That can be fine once you're up to speed and now what you're doing.

One thing I took away from the Manning Functional Reactive Programming book was that you can clarify things a great deal by creating data types for the inputs and outputs of your logical components and pulling out the description of how the inputs relate to the outputs out of the main event network.

It is even more clear if you draw up block diagrams for the components.
They're a bit too labour intensive to prepare for this block, but scratching out some block diagrams on pen and paper can be really handy if you get stuck.

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
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part2/Example4.hs)

There's a tantalizing function available in `reactive-banana`.
```haskell
interpret :: (Event a -> Moment (Event b)) -> [Maybe a] -> IO [Maybe b] 
```
According to the haddocks, the IO is an implementation detail but you can otherwise treat it as a pure function.

Also in the haddocks for `interpret`: "Useful for testing."
That was like a red rag to a bull for me.

It looks like we're going to need two data types to use it - one for our input events and one for our output events.

We're up to the challenge:
```haskell
data InputCmd =
    Open
  | Read String
  deriving (Eq, Ord, Show)

data OutputCmd =
    Write String
  | Close
  deriving (Eq, Ord, Show)
```

We are trying to test a network of type `InputIO -> Moment Output IO` using `Event InputCmd` as inputs and `Event [OutputCmd]` as outputs.

The list of `OutputCmd`s is need to be able to check if certain outputs are happening simultaneously.

So we write a function to get from `Event InputCmd` to `InputIO`:
```haskell
fanInput :: Event InputCmd -> InputIO
fanInput eIn =
  let
    maybeOpen Open = Just ()
    maybeOpen _    = Nothing
    eOpen = filterJust $ maybeOpen <$> eIn

    maybeRead (Read x) = Just x
    maybeRead _ = Nothing
    eRead = filterJust $ maybeRead <$> eIn
  in
    InputIO eOpen eRead
```
and a function to get from `OutputIO` to `Event [OutputCmd]`
```haskell
mergeOutput :: OutputIO -> Event [OutputCmd]
mergeOutput (OutputIO eWrite eClose) =
  unionWith (++)
    ((\x -> [Write x]) <$> eWrite)
    ([Close] <$ eClose)
```
and we package it up neatly:
```haskell
testNetwork :: (InputIO -> Moment OutputIO) -> [Maybe InputCmd] -> IO [Maybe [OutputCmd]]
testNetwork fn =
  interpret $ \i -> do
    o <- fn . fanInput $ i
    return $ mergeOutput o
```

From that we can create a testing function:
```haskell
testIOCmds :: [Maybe InputCmd] -> IO [Maybe [OutputCmd]]
testIOCmds = testNetwork pureNetworkDescription
```
without having to modify our network description at all.

With some formatting liberties in the output, it behaves admirably when we take it for a spin:
```haskell
> output <- testIOCmds [
    Just (Read "one")
  , Nothing
  , Just (Read "two")
  , Just (Read "/quit")
  ]
> output
[ Just [Write "one"]
, Nothing
, Just [Write "two"]
, Just [Write "Bye", Close]
]
```

This looks pretty handy for use with `QuickCheck` or `HUnit`.

We can also do this for any of the components we've come across so far - mostly because their inputs and outputs have been made up of events, so the fanning and merging functions are usually doable.

## Next up

The next thing we'll look at is behaviors, which will help us deal with more complex scenarios in a more modular way.

[Onwards!](./behaviors.html)
