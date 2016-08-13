---
title: Events
published: 2016-09-01 12:00:00+10:00
---

There are two main data types in FRP - `Event`s and `Behavior`s - and we combine them together to build an FRP event network which describes how our program is going to respond to various events over time.

# Events

An `Event` is something that happens at a particular point of time - like a button press, or a key press, or the receipt of a HTTP request - and a `Behavior` is something that has values at all points of time.

For our purposes, this is a _logical_ view of time, where are logical clock ticks every time an event is fired from _outside_ of our FRP event network.

Let's take a look at `Event` from `reactive-banana`:
```haskell
data Event a
```
which has a `Functor` instance:
```haskell
instance Functor Event where ...
```

## Simultaneous `Event`s

The `Functor` instance provides a good example of how we can end up with multiple events happening at the same time originating from the _inside_ of our event network.

The simplest demonstration of this is:
```haskell
doubler :: Event Int -> Event Int
doubler eNumber =
  let
    eOtherNumber = (* 2) <$> eNumber
  in
    eOtherNumber
```
where `eNumber` and `eOtherNumber` are active at the same logical points in time.

(I'm using the `let` above to give things a name that I can refer to easily).

We could also use 
```haskell
filterE :: (a -> Bool) -> Event a -> Event a
```
to do
```haskell
evener :: Event Int -> Event Int
evener eNumber =
  let
    eEven = filterE (\x -> x `mod` 2 == 0) eNumber
  in
    eEven
```
and `eEven` will be active at the logical points in time when `eNumber` is both active and has an even value.

If you're dealing with a function in `reactive-banana` that has a _pure_ `Event` as an output, then the output `Event` will only ever be active at the same logical points of time as the input `Event`s.
The output `Event` maybe be active less often - as was the case with `filterE` - but it will always be active at the same time as one or more of the input events.

This becomes relevant when we start combining `Event`s.

If we never had simultaneous events, we could use something like:
```haskell
union :: Event a -> Event a -> Event a
```
which would activate the output whenever either of the inputs were active.

We have to deal with the possibility of events occurring at the same time, and so `reactive-banana` provides:
```haskell
unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
```

If the one of the input events is active and the other event is not, then the output is activated with the value of the active input.
If both events are active at the same time, then we combine the output is activated with the value that results from running the combining function on the two active values.

Now we can use that to get some important work done:
```haskell
importantWork :: Event Int -> Event String
importantWork eCount =
  let
    eFizz = 
      fmap (\_ -> "Fizz") . 
      filterE (\x -> x `mod` 3 == 0) $
      eCount
    eBuzz = 
      fmap (\_ -> "Buzz") . 
      filterE (\x -> x `mod` 5 == 0) $
      eCount
    eFizzBuzz = 
      unionWith (\_ _ -> "FizzBuzz") 
      eFizz 
      eBuzz
  in
    eFizzBuzz
```
although we'd probably refactor that a little.

We'll remove some duplication, and we'll switch to using `<$` since it's pretty usual to use `<$>` and `<$` in FRP code:
```haskell
multiple :: Int -> Event Int -> Event Int
multiple m = 
  filterE (\x -> x `mod` m == 0)

importantWork :: Event Int -> Event String
importantWork eCount =
  let
    eFizz = 
      "Fizz" <$ multiple 3 eCount
    eBuzz = 
      "Buzz" <$ multiple 5 eCount
    eFizzBuzz = 
      unionWith (\_ _ -> "FizzBuzz") 
      eFizz 
      eBuzz
  in
    eFizzBuzz
```

That's right.
I totally went there.

## Connecting the event network

Now that we've looking a little at the inside of the event network, it is time to take a look at how these things look from the outside.

We're going to be doing this by breaking ground on our first FRP application, which will be a command line application that echoes input.

It'll get more impressive soon, I promise.

When building an FRP app there are usually two pieces - or at least two kinds of pieces - that are involved.
There is the event network, which is where the FRP happens, and the event loop, which feeds events into the network from the outside.

There are several functions we can use to bridge that gap, but the one we'll be most interested in is:
```haskell
newAddHandler :: IO (AddHandler a, a -> IO ()) 
```

We can unpack that into a data type:
```haskell
data EventSource a = EventSource {
    addHandler :: AddHandler a
  , fire       :: a -> IO ()
  }

mkEventSource :: IO (EventSource a)
mkEventSource =
  uncurry EventSource <$> newAddHandler
```
where `addHandler` is used to subscribe to event updates from inside the event network, and `fire` is used to initiate events from the event loop.

For the event loop of our command line, input echoing application, we just need to repeatedly read lines and fire the event:
```haskell
eventLoop :: EventSource String -> IO ()
eventLoop i =
  forever $ do
    x <- getLine
    fire i x
```

Every event that we fire from the outside occurs at a distinct moment in logical time.
In the documentation for `sodium` these are referred to as transactions - which can be a helpful way of thinking of them - and the transactions have their own context.

In `reactive-banana` this context is provided by the `Moment` and `MomentIO` monads.
If you see these in the signature of a function, it means that you're dealing with something that is outside of the current moment in logical time.

For the event network, we use two functions from `reactive-banana` that operate in the `MomentIO` monad.

We register our event sources using:
```haskell
fromAddHandler :: AddHandler a -> MomentIO (Event a) 
```
and we cause `IO` to happen in response to events using:
```haskell
reactimate :: Event (IO ()) -> MomentIO () 
```

The use of `MomentIO` in here makes some kind of sense.
We will get the events after we register for them, and the `IO` output will occur after the event that triggers it.
I found that my intuition for the `Moment` monads really took a step up once I started using them inside of the event networks.
We'll get to that soon.

With these two functions in our toolbox, we can describe our first event network:
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

Finally we need to glue everything together:
```haskell
go :: IO ()
go = do
  input <- mkEventSource
  network <- compile $ networkDescription input
  actuate network
  eventLoop input
```

We use `compile` to create a `NetworkDescription` and `actuate` to get it kicking over.
Most of our programs are going to do pretty similar things with respect to gluing things together, so it's unlikely that we'll delve too deeply into that last piece of code.

So far it's not a very impressive event network, but it makes for a nice starting point.

## Improvements to the echo program

We can improve this program quite a bit.

### Adding the ability to quit

Let us start by adding the ability to quit the program when the user types "/quit":
```haskell
import System.Exit (exitSuccess) 

networkDescription :: EventSource String -> MomentIO ()
networkDescription i = do
  eRead <- fromAddHandler . addHandler $ i

  let
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead

  reactimate $ putStrLn <$> eMessage
  reactimate $ exitSuccess <$ eQuit
```

So far, so good.

### Printing a message on exit

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
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead

  reactimate $ fmap putStrLn . leftmost $ [
      eMessage
    , "Bye" <$ eQuit
    ]
  reactimate $ exitSuccess <$ eQuit
```

### Refactoring for testing

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
data InputIO =
  Read String
  deriving (Eq, Ord, Show)

data OutputIO =
    Write String
  | Close
  deriving (Eq, Ord, Show)
```

We want to be able to focus on the `String` in our `Read` inputs:
```haskell
inputToRead :: Event InputIO -> Event String
inputToRead eIn =
    filterJust $ maybeRead <$> eIn
  where
    maybeRead (Read x) = Just x
    maybeRead _        = Nothing
```

This also introduces `filterJust`, which unwraps `Just` values in its input `Event` and ignores the `Nothing` values.

We can now define our event network.

Since we're not doing any `IO` in the network, we can express it in the `Moment` monad rather than in the `MomentIO` monad.
In fact even that is overkill, since what we currently want can be expressed as a pure function.

We'll leave it in the `Moment` monad for now, since it means it is trivially compatible with `interpret`.

```haskell
myTestableNetwork :: Event InputIO -> Moment (Event [OutputIO])
myTestableNetwork eIn =
  let
    eRead    = inputToRead eIn
    eMessage =       filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead

    eWrite   = leftmost [
        eMessage
      , "Bye" <$ eQuit
      ]

    eOut = fmap ($ []) . unions $ [
        (\x xs -> Write x : xs) <$> eWrite
      , (\xs -> Close : xs) <$ eQuit
      ]
  in
    return eOut
```

The biggest change here is with the description of `eOut`.
We can't use `leftmost` for this, since when the user quits we will have `eWrite` and `eQuit` happening at the same time.
That's why `eOut` has type `Event [OutputIO]`.

We build `eOut` using `unions`.
```haskell
unions :: [Event (a -> a)] -> Event (a -> a)
```
If none of the input events activates, the output doesn't activate.
If only one of the input events activates with a value, the output activates with that value.
If multiple inputs activate at the same time, the functions in the events are composed - with the functions being applied in the order they are listed - and the output activates with the composed function as the value.

This means that the listing of `eWrite` before `eQuit` is significant.
If it were the other way around, our program would exit before it got to print its farewell.

Now we have something that we can test with `interpret`.

With some formatting liberties, we get:
```haskell
> output <- interpret myTestableNetwork [
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
which looks pretty handy for use with `QuickCheck` or `HUnit`.

In order to tie all of this together for actual usage we need to be able to convert our raw input events into events of type `Input`:
```haskell
handleInput :: EventSource String -> MomentIO (Event InputIO)
handleInput i = do
  eRead <- fromAddHandler . addHandler $ i
  return $ Read <$> eRead
```
and we need to be able to convert values of `Output` into `IO` actions:
```haskell
handleOutput :: OutputIO -> IO ()
handleOutput (Write s) = putStrLn s
handleOutput Close = exitSuccess
```

Our overall network is now split into setting up our inputs, a pure and testable network to convert inputs into outputs, and a dealing with outputs: 
```haskell
networkDescription :: EventSource String -> MomentIO ()
networkDescription s = do
  i <- handleInput s
  o <- liftMoment $ myTestableNetwork i
  reactimate $ traverse_ handleOutput <$> o
```
We use `liftMoment` here to convert our network in the `Moment` monad into a network in the `MomentIO` monad.

Voila! We have something that is both testable and usable.

### Fanning in and out

Scratch that "Voila!" though, because the network is still a bit of a mess.

At the moment our event network has one event as input and one event as output.

![](../images/network1.png)

That is mostly a side effect of the fact that we're writing a command line application.

We can see a hint of that if we look at the way that we're cramming two different outputs into `Output`.
It is worth going further with this, in order to work out what the interesting events in our domain are.

If we untangle things it might help with reuse, it'll give us smaller pieces that we can test, and it'll force us to think about what our system is really doing.

In this case, I want to evolve this application into the back-end for a web-service, so when we get to there we'll have different input events coming from different endpoints.
This will be easier if we can tease out some more structure.

The first thing we can do is to identify our logical inputs and build a data structure to hold them:
```haskell
data Inputs = Inputs {
    ieMessage        :: Event String
  , ieQuit           :: Event ()
  }
```

We then build a function that will be the bridge between our actual inputs and our logical inputs:
```haskell
fanOut :: Event InputIO -> Inputs
fanOut eIn =
  let
    eRead = inputToRead eIn
    eMessage = filterE (/= "/quit") eRead
    eQuit = () <$ filterE (== "/quit") eCommand
  in
    Inputs eMessage eQuit
```

We'll build a similar data structure for our logical outputs:
```haskell
data Outputs = Outputs {
    oeWrite :: [Event String]
  , oeClose :: [Event ()]
  }
```
and a function to bridge the gap:
```haskell
fanIn :: Outputs -> Event [OutputIO]
fanIn (Outputs eWrites eCloses) =
  let
    eCombinedWrites = fmap (\x xs -> Write x : xs) <$> eWrites
    eCombinedCloses = [(Close :) <$ leftmost eCloses]
  in
    fmap ($ []) .
    unions $
    eCombinedWrites ++ eCombinedCloses
```

We're dealing with multiple write events already, and it seems likely we'll end up with multiple close events, so `Outputs` contains lists for both of these.

We also want to be doing the combining of these in `fanIn` rather than in our event network, since knowing how to combine them is something that involves information about both the logical and actual domains.
You can see that in action in the code - we combine all of our write events by append them together, because we want all of the writes to happen, but we just use the leftmost close event, since we only want to close the application at most once.

We end up with a network that looks like this:
```haskell
myLogicalNetwork :: Inputs -> Outputs
myLogicalNetwork (Inputs eMessage eQuit) =
  let
    eWrites = [
        eMessage
      , "Bye" <$ eQuit
      ]
    eQuits = [
        eQuit
      ]
  in
    Outputs eWrites eQuits

myTestableNetwork :: Event InputIO -> Moment (Event [OutputIO])
myTestableNetwork =
  return . fanIn . myLogicalNetwork . fanOut
```
which is a bit underwhelming.

If we draw a block diagram for the system though, we've exposed a bit more of what is going on:

![](../images/network2.png)

It is worth pointing out that as an aside that we can test our `fanOut` component with `interpret` by introducing a new data type to collect the results:
```haskell
data FanOutResults =
    FrMessage String
  | FrQuit

collectFanOutResults :: Inputs -> Event FanOutResults
collectFanOutResults (Inputs eMessage eQuit _) =
  leftmost [
      FrMessage <$> eMessage
    , FrQuit <$ eQuit
    ]
```

### Handling unknown commands

On the topic of the `fanOut` function, we should make some changes to help prepare for the future.

We're going to treat all input strings that start with "/" as commands, and we want to reject any commands that we don't recognize or are otherwise ill-formed.
We'll also ignore empty strings that are input.

We'll need a new event in `Inputs`:
```haskell
data Inputs = Inputs {
    ieMessage        :: Event String
  , ieQuit           :: Event ()
  , ieUnknownCommand :: Event String
  }
```

Our new version of `fanOut` looks like this:
```haskell
fanOut :: Event InputIO -> Inputs
fanOut eIn =
  let
    eRead =
      filterE (not . null) $ inputToRead eIn

    isMessage =
      (/= "/") . take 1
    eMessage =
      filterE isMessage eRead

    isCommand =
      (== "/") . take 1
    eCommand =
      fmap (drop 1) . filterE isCommand $ eRead

    eQuit =
      () <$ filterE (== "quit") eCommand
    eUnknownCommand =
      filterE (/= "quit") eCommand
  in
    Inputs eMessage eQuit eUnknownCommand
```
and is starting to exhibit some of the wall-of-text characteristics that you come across in FRP code.

If you wanted to enhance that effect, you could inline `isMessage` and `isCommand` into the definitions of `eMessage` and `eCommand` respectively. 

To go in the other direciton we could do change to a two stage approach, with the first stage filtering out empty inputs and splitting messages from other commands, and the second stage handling the commands themselves.

The change that the new event has on our network is fairly small:
```haskell
myLogicalNetwork :: Inputs -> Outputs
myLogicalNetwork (Inputs eMessage eQuit eUnknownCommand) =
  let
    eWrites = [
        eMessage
      , "Bye" <$ eQuit
      , ("Unknown command: " ++) <$> eUnknownCommand
      ]
    eQuits = [
        eQuit
      ]
  in
    Outputs eWrites eQuits
```
which is usually a good sign.

### Breaking things down even more

The last change I want to play with is adding a component for each of the commands we want to handle.

Some of the FRP examples end up with pretty big event network all defined in the one place.
That can be fine once you're up to speed and now what you're doing.
One thing I took away from the Manning Functional Reactive Programming book was that you can clarify things a great deal by creating data types for the inputs and outputs of your logical components and pulling out the description of how the inputs relate to the outputs out of the main event network.

It might look like overkill, but you can always use fewer components with more going on inside each one.

We have a pair of data types for handling messages, and a function to get from one to the other:
```haskell
data MessageInput = MessageInput {
    mieRead :: Event String
  }

data MessageOutput = MessageOutput {
    moeWrite :: Event String
  }

handleMessage :: MessageInput -> MessageOutput
handleMessage (MessageInput eMessage) =
  MessageOutput eMessage
```

We have the same arrangement for handling the command to quit:
```haskell
data QuitInput = QuitInput {
    qieQuit :: Event ()
  }

data QuitOutput = QuitOutput {
    qoeWrite :: Event String
  , qoeQuit  :: Event ()
  }

handleQuit :: QuitInput -> QuitOutput
handleQuit (QuitInput eQuit) =
  QuitOutput ("Bye" <$ eQuit) eQuit
```
which encapsulates our farewell message rather than leaving it hanging out in the middle of our event network.

Unknown commands get a similar treatment:
```haskell
data UnknownCommandInput = UnknownCommandInput {
    ucieUnknownCommand :: Event String
  }

data UnknownCommandOutput = UnknownCommandOutput {
    ucoeUnknownCommand :: Event String
  }

handleUnknownCommand :: UnknownCommandInput -> UnknownCommandOutput
handleUnknownCommand (UnknownCommandInput eUnknownCommand) =
  UnknownCommandOutput (("Unknown command: " ++) <$> eUnknownCommand )
```

Our network that uses these pieces looks like this:
```haskell
myLogicalNetwork :: Inputs -> Outputs
myLogicalNetwork (Inputs eMessage eQuit eUnknownCommand) =
  let
    MessageOutput emWrite = handleMessage $ MessageInput eMessage
    QuitOutput eqWrite eqQuit = handleQuit $ QuitInput eQuit
    UnknownCommandOutput eucWrite = handleUnknownCommand $ UnknownCommandInput eUnknownCommand
  in
    Outputs [emWrite, eqWrite, eucWrite] [eqQuit]
```
with a block diagram that looks like this:

![](../images/network3.png)

## Next time...

