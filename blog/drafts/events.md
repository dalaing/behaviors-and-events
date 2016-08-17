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
    eMessage = filterE (/= "/quit") eRead
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
    eMessage = filterE (/= "/quit") eRead
    eQuit    = () <$ filterE (== "/quit") eRead

  reactimate $ fmap putStrLn . leftmost $ [
      eMessage
    , "Bye" <$ eQuit
    ]
  reactimate $ exitSuccess <$ eQuit
```

### Printing a greeting

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

### Adding a help command

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

### Detecting the use of unknown commands

To top it all off, we're going to detect the use of unknown commands.

```haskell
networkDescription :: InputSources -> MomentIO ()
networkDescription (InputSources o r) = do
  eOpen <- fromAddHandler . addHandler $ o
  eRead <- fromAddHandler . addHandler $ r

  let
    eMessage = filterE (/= "/" . take 1) eRead
    eCommand = fmap (drop 1) . filterE (== "/" . take 1) eRead
    eHelp    = () <$ filterE (== "help") eCommand
    eQuit    = () <$ filterE (== "quit") eCommand

    commands        = ["help", "quit"]
    eUnknownCommand = () <$ filterE (`notElem` commands) eCommand

  reactimate $ fmap putStrLn . leftmost $ [
      "Hi (type /help for instructions)" <$ eOpen
    , eMessage
    , "/help displays this message\n/quit exits the program" <$ eHelp
    , "Bye" <$ eQuit
    , (\x -> "Unknown command: " ++ x ++ " (type /help for instructions)") <$> eUnknownCommand
    ]
  reactimate $ exitSuccess <$ eQuit
```

## Next up

We've got our first little FRP application together, so we should be feeling pretty good.

Soon we'll look at behaviors, which will let us write some more interesting programs.

Before that, we're going to refactor what we have to make it more testable and to make the pieces more reusable.

[Onwards!](./refactoring.html)
