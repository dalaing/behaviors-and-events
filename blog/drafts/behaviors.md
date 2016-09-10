---
title: Behaviors
published: 2016-09-01 12:00:00+10:00
---

[Previously](./echo.html) we developed a simple command line program and refactored it to make parts of it more reusable and testable.

# Behaviors

The next piece of the `FRP` puzzle is behaviors.
Where events are only defined at particular points in time, behaviors are defined for all points of time.

The type has a single parameter:
```haskell
data Behavior a
```
and it has a `Functor` instance and an `Applicative` instance.
```haskell
instance Functor Behavior
instance Applicative Behavior
```

The documentation mentions that the semantics of an `Behavior a` allows you to view it as being similar to `Time -> a`.

The `Applicative` instance means that we can create a behavior that has a constant value across all times using `pure`.

A more interesting way to create a behavior is with `stepper`:
```haskell
stepper :: MonadMoment m => a -> Event a -> m (Behavior a)
stepper x eX = ...
```

This will create a `Behavior` that starts with the value `x`, and then changes its to the value the event `eX` every time that event occurs.
This change will not be observable until the next logical clock tick _after_ the event `eX`.

We always sample the values of behaviors using events, which give us the logical clock tick to use to sample the behaviour.

This is commonly done with some infix operators:
```haskell
(<@>) :: Behavior (a -> b) -> Event a -> Event b
(<@)  :: Behavior b        -> Event a -> Event b 
```

These are analogues of `<*>` and `<*` from `Applicative`.
Since behaviour has an `Applicative` instance, it means we can do things like:
```haskell
f <$> bBehavior1 <*> bBehavior2 <@> eEventWithStuffTheFunctionNeeds
```
or
```haskell
f <$> bBehavior1 <*> bBehavior2 <@ eEventThatIsJustActingAsATrigger
```

What has this given us?
It has given us a way to manage state inside an FRP system.

Say we want something like `Reader`.
We can create a `Behavior` using `pure` to get something similar.
The equivalent of using `ask` is to sample the `Behavior` using `<@` and an event that indicates when you would like to get the value out.

If you want something like `State`, you can create your `Behavior` using `stepper`.
In that case sampling the value with `<@` is like `get` and the input event is acting like `put`.

The biggest change happening here comes from the fact that events can fire multiple times, and we can combine multiple events into the one event.
This means that we have the option to specify all of the usages of `put` for the our `State` over its entire lifetime, all in the one call.
We can take care of all of our usages of `get` with one function as well.

An example of putting all of our `put`-equivalents in on basket s describing whether a user is logged in or not:
```haskell
logInHandler :: MonadMoment m => Event () -> Event () -> m (Behavior LogInState)
logInHandler eLogIn eLogOut =
  stepper LoggedOut . leftmost $ [
      -- change the state to LoggedIn when the user logs in
      LoggedIn  <$ eLogIn
      -- change the state to LoggedOut when the user logs out
    , LoggedOut <$ eLogOut
    ]
```

While we have that option, we don't have to take it.
If it makes it easier to define the event graph, we can have one use of `<@` that specifies all of our usages of `get` for non-authenticated users and another use of `<@` for the authenticated users.

We can add some `get`-equivalents to the above example to check for errors:
```haskell
logIn :: LogInState -> Either LogInError LogInState
logIn LoggedIn  = Left AlreadyLoggedIn
logIn LoggedOut = Right LoggedIn

logOut :: LogInState -> Either LogInError LogInState
logOut LoggedOut = Left NotLoggedIn
logOut LoggedIn  = Right LoggedOut

-- This makes use of the RecursiveDo language extension
-- Trust me for now - I'll explain how that works later
logInHandler :: MonadMoment m => Event () -> Event () -> m (Behavior LogInState, Event LogInError)
logInHandler eLogIn eLogOut = mdo
  bLogInState <- stepper LoggedOut eLogInState
  (eLogInError, eLogInState) = split . leftmost $ [
      logIn  <$> bLogInState <@ eLogIn
      logOut <$> bLogInState <@ eLogOut
    ]
  return (bLogInState, eLogInError)
```
while still being able to query the logged-in state elsewhere in the system.

Since `Behavior`s are defined at all times, it means that we can sample them outside of the points in time that were used to define them.
Perhaps that is obvious, but it means that you can define and/or compose your `Behavior`s in one library, and then sample it with `Event`s that are defined and/or composed in another library.
Composition is a huge win.

There's a lot more that we can do with these pieces, and there are other ways that you can view them.
I just thought it might be good to mention that you can view `Behavior`s as modelling state.
Hopefully it helps people with their intuitions, before I mess with them later on.

## Message history
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part3/)

Let's use `stepper`, `<@>` and `<@` to demonstrate what we can do with behaviors.

As a reminder, our previous take on message handling was pretty boring:
```haskell
data MessageInput  = MessageInput  { mieRead  :: Event String }
data MessageOutput = MessageOutput { moeWrite :: Event String }

handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) = do
  return $ MessageOutput eMessage
```

### The last message

We'll spice that up by printing the last message that was echoed:
```haskell
-- see Part3.Example1
handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- stepper "" eMessage
  let
    f l m = m ++ " (last message: " ++ l ++ ")"
    eOut = f <$> bMessages <@> eMessage
  return $ MessageOutput eOut
```

We're using `stepper` to keep the most recently seen message around so that we can sample it later on.
The updates to `stepper` are being driven by `eMessage`, and won't be seen until the next transaction.
By using `eMessage` to sample `bMessages` we're grabbing the value of `bMessages` at the moment before it gets updated by `eMessage`.

This is how we manage to use the behavior to get hold of the last message that we saw.

### The length of the last message

We've already got the last message on the screen, so maybe displaying the length of the last message would be more interesting:
```haskell
-- see Part3.Example2
handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- stepper "" eMessage
  let
    bLastLength = length <$> bMessages
    f l m = m ++ " (last message length: " ++ show l ++ ")"
    eOut = f <$> bLastLength <@> eMessage
  return $ MessageOutput eOut
```
It also shows that I'm not lying about the `Functor` instance.

### All of the previous messages

We use another function to do more interesting things with behaviors:
```haskell
accumB :: MonadMoment m => a -> Event (a -> a) -> m (Behavior a) 
accumB x eF = ...
```

On one hand this is like `accumE`, in that it accumulates the composition of the functions inside the various occurrences of the event.
On the other hand this is a bit like `stepper`, in that the change isn't observable until the next logical moment in time.

Both `accumE` and `accumB` work nicely with `unions`:
```haskell
unions :: [Event (a -> a)] -> Event (a -> a)
```
if we want to work with several events.

We can use this to accumulate all of the messages that we've seen:
```haskell
-- see Part3.Example3
handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] . fmap (:) $ eMessage
  let
    f ms m = m ++ " (previous messages: " ++ show ms ++ ")"
    eOut = f <$> bMessages <@> eMessage
  return $ MessageOutput eOut
```

### Conditional events based on behaviors

We can filter events using the value of a behavior at the time of the event using `whenE`:
```haskell
whenE :: Behavior Bool -> Event a -> Event a 
```

If we want to use the value of the event in the filtering decision, we can use `filterApply`:
```haskell
filterApply :: Behavior (a -> Bool) -> Event a -> Event a 
```

This gives us one (slightly contrived) option for not printing anything special when the history is empty:
```haskell
-- see Part3.Example4
handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] . fmap (:) $ eMessage
  let
    f ms m = m ++ " (previous messages: " ++ show ms ++ ")"
    bHasMessages = (not . null) <$> bMessages
    eOut = leftmost [
        f <$> bMessages <@> whenE bHasMessages eMessage
      , eMessage
      ]
  return $ MessageOutput eOut
```

If there are messages, `bHasMessages` will be `True`. If a message arrives then both of the events leading into `eOut` will be active, and the first event in the list will get used because that's how `leftmost` works.
If there are no messages, `bHasMessages` will be `False` and `eMessage` will flow through to `eOut` on its own.

### Trimming the history

If we just want to print the last 3 messages that we've seen we can do that:
```haskell
-- see Part3.Example5
handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] . fmap (:) $ eMessage
  let
    bLimitedMessages = take 3 <$> bMessages
    f ms m = m ++ " (previous 3 messages: " ++ show ms ++ ")"
    eOut = f <$> bLimitedMessages <@> eMessage
  return $ MessageOutput eOut
```

Maybe we want the number of messages to be configurable, or we want to tie it to a modifiable setting.
We can get some future proofing for that by passing in the number of messages that we want as a behavior.

Doing this is a common FRP idiom.

This is how it looks:
```haskell
-- see Part3.Example6
data MessageInput = MessageInput {
    mieRead  :: Event String
  , mibLimit :: Behavior Int
  }

handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage bLimit) = do
  bMessages <- accumB [] . fmap (:) $ eMessage
  let
    f n ms m = m ++ " (previous " ++ show n ++ " messages: " ++ show (take n ms) ++ ")"
    eOut = f <$> bLimit <*> bMessages <@> eMessage
  return $ MessageOutput eOut
```

If we want to hard-code a limit of 3 for the time being, we can set it up with `pure 3`.
Later on, we might add some code for admins to alter the limit - if that contributes to a behavior, and if our various components take their parameters as behaviors, then all we need to do is wire it up to our components.

There's a problem though: `bMessages` accumulates all of the messages and never frees up any memory.

For a fixed limit, we can just bring the `take 3` code into the `accumB`:
```haskell
-- see Part3.Example7
handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) = do
  let
    limitCons n x xs = take n (x : xs)
  bMessages <- accumB [] . fmap (limitCons 3) $ eMessage
  let
    f ms m = m ++ " (last 3 messages: " ++ show ms ++ ")"
    eOut = f <$> bMessages <@> eMessage
  return $ MessageOutput eOut
```

### A component for the history limit

In order to modify the limit as we go, we need to introduce a few more pieces.

Let's assume that the limit on the history starts at 1, and is modified by two events: one increments the limit, the other decrements the limit unless it is already at 0.

We can build a behavior for the limit using `accumB`:
```haskell
data LimitInput = LimitInput {
    lieLimitUp   :: Event ()
  , lieLimitDown :: Event ()
  }

data LimitOutput = LimitOutput {
    lobLimit :: Behavior Int
  }

handleLimit :: MonadMoment m => LimitInput -> m LimitOutput
handleLimit (LimitInput eUp eDown) = do
  bLimit <- accumB 1 . unions $ [
      succ <$ eUp
    , (max 0 . pred) <$ eDown
    ]
  return $ LimitOutput bLimit
```

If we wanted to be a little fancier for no real good reason, we could use recursive values via the `RecursiveDo` language extension:
```haskell
{-# LANGUAGE RecursiveDo #-}
handleLimit :: MonadMoment m => LimitInput -> m LimitOutput
handleLimit (LimitInput eUp eDown) = mdo
  let
    eDownNonNegative = whenE ((> 0) <$> bLimit) eDown
  bLimit <- accumB 1 . unions $ [
      succ <$ eUp
    , pred <$ eDownNonNegative
    ]
  return $ LimitOutput bLimit
```

As long as your behaviors recurse through intermediate events and your events through intermediate behaviors, `reactive-banana` will take care of you.

The behavior gives us a step function, but what we're going to end up wanting is an event that triggers when the behavior changes.

We can do that with `accumE`:
```haskell
eLimit <- accumE 1 . unions $ [
    (+ 1) <$ eUp
  , (max 0 . subtract 1) <$ eDown
  ]
```

We are going to end up needing both, and we can efficiently combine the two of these by using `mapAccum`:
```haskell
mapAccum :: MonadMoment m => acc -> Event (acc -> (x, acc)) -> m (Event x, Behavior acc) 
```

We can use this to create a new component that will give as both a behavior tracking the number of messages we should be displaying and an event that lets us known when that value has changed:
```haskell
data LimitInput = LimitInput {
    lieLimitUp :: Event ()
  , lieLimitDown :: Event ()
  }

data LimitOutput = LimitOutput {
    loeLimit :: Event Int
  , lobLimit :: Behavior Int
  }

handleLimit :: MonadMoment m => LimitInput -> m LimitOutput
handleLimit (LimitInput eUp eDown) = do
  (eLimit, bLimit) <- mapAccum 1 . fmap (\f x -> (f x, f x)) . unions $ [
      succ <$ eUp
    , (max 0 . pred) <$ eDown
    ]
  return $ LimitOutput eLimit bLimit
```

Having both a behavior and an event that triggers when the behavior changes can be handy.

### Trimming the history

With the limit component in hand we can prepare a component to manage and print the message history.

We set up our inputs and outputs:
```haskell
data MessageInput = MessageInput {
    mieMessage :: Event String
  , mieLimit :: Event Int
  , mibLimit :: Behavior Int
  }

data MessageOutput = MessageOutput {
    moeWrite :: Event String
  }
```
and then we connect them up:
```haskell
-- see Part3.Example8
addMessage :: Int -> String -> [String] -> [String]
addMessage n m ms =
  take n (m : ms)

handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage eLimit bLimit) = do
  bMessage <- accumB [] . unions $ [
      take <$> eLimit
    , addMessage <$> bLimit <@> eMessage
    ]
  let
    f l h m = m ++ " (last " ++ show l ++ "message: " ++ show h ++ ")"
    eWrite = f <$> bLimit <*> bMessage <@> eMessage
  return $ MessageOutput eWrite
```

When the limit changes, we trim the list.
When a message comes in, we add the message to the front of the list and then trim it to the current limit.

This would be useful if we needed to immediately reclaim the memory when the limit changes, but we could probably just make do with a version of `handleLimit` which just returns a `Behavior` and let `addMessage` handle the trimming of the history.

## Testing with behaviors

This will be filled in once I have my talk finished...
<!--

This next section is a bit gory, but is here to demonstrate that you can test event networks which have behaviors in their inputs and outputs if you're willing to get your hands dirty.

If that doesn't sound like your cup of tea, feel free to [skip ahead](./components.html).

In order to test code that has behaviors as inputs or outputs, we need to modify our testing code.

I'm not advocating actually using this, since it's much messier than it needs to be.
I just wanted something to give people a starting point, and to show that it can be done.

We add a data type to carry information about both the behaviors and the events that are inputs to our event network: 
```haskell
data Command b e = Command {
    _cmdB :: b
  , _cmdE :: e
  } deriving (Eq, Ord, Show)

mkLenses ''Command
```
and change the `Fannable` class to use it:
```haskell
class Fannable b e where
  type Fanned b e
  fanInput :: Testable m => Event (Command b e) -> m (Fanned b e)
```

The idea here is that we'll use the input events and the values in `cmdB` to reconstitute the behaviors using `stepper`.

For the outputs we make a similar change.

We carry the behaviors and possibly multiple simultaneous events in our data type:
```haskell
data Result b e = Result {
    _resB :: b
  , _resE :: [e]
  } deriving (Eq, Ord, Show)

mkLenses ''Result
```
and the the `Mergable` class uses that and picks up an extra argument:
```haskell
class Mergable b e where
  type ToMerge b e
  mergeOutput :: Event () -> ToMerge b e -> Event (Result b e)
```

The extra argument is used to sample the output behaviors, so we will know what their values are at each point in time that is observable by the event network.

We see this at work in the version of `testNetwork`:
```haskell
testNetwork :: ( 
            Testable m
          , Fannable ib ie
          , Mergable ob oe
          ) => (Fanned ib ie -> m (ToMerge ob oe)) 
            -> [Maybe (Command ib ie)] 
            -> IO [Maybe (Result ob oe)]
testNetwork fn =
  interpretEvents $ \i -> do
    fi <- fanInput i
    o <- fn fi
    return $ mergeOutput (() <$ i) o
```


We'll also add some helper functions for `Fannable`:
```haskell
filterPrism :: Prism' s a -> Event s -> Event a
filterPrism p e = 
  filterJust $ preview p <$> e

fanE :: (Testable m, Fannable b e) => Prism' e a -> Event (Command b e) -> m (Event a)
fanE p = 
  return . filterPrism p . fmap _cmdE

stepperLens :: MonadMoment m => a -> Lens' s a -> Event s -> m (Behavior a)
stepperLens x l e = 
  stepper x (view l <$> e)

fanB :: (Testable m, Fannable b e) => a -> Lens' b a -> Event (Command b e) -> m (Behavior a)
fanB x l = 
  stepperLens x (cmdB . l)
```
and for `Mergable`:
```haskell
combineResult :: Result b e -> Result b e -> Result b e
combineResult (Result b1 e1) (Result b2 e2) =
  Result b1 (e1 ++ e2)

class Mergable b e where
  type ToMerge b e
  mergeOutput :: Event () -> ToMerge b e -> Event (Result b e)

mergeE :: Mergable b e => Behavior b -> Prism' e a -> Event a -> Event (Result b e)
mergeE b p e =
  (\bv ev -> Result bv [review p ev]) <$> b <@> e

mergeB :: Mergable b e => Behavior b -> Event () -> Event (Result b e)
mergeB b e =
  (\bv -> Result bv []) <$> b <@ e
```

Let's take a look at them in action.

### Testing `handleLimit`

```haskell
data LimitInputCmd =
    LLimitUp
  | LLimitDown
  deriving (Eq, Ord, Show)

makePrisms ''LimitInputCmd

instance Fannable () LimitInputCmd where
  type Fanned () LimitInputCmd = LimitInput
  fanInput eIn =
    LimitInput <$>
      fanE _LLimitUp eIn <*>
      fanE _LLimitDown eIn
```

```haskell
data LimitOutputEvent =
    LLimit Int
  deriving (Eq, Ord, Show)

makePrisms ''LimitOutputEvent

data LimitOutputCmd = LimitOutputCmd {
    locbLimit :: Int
  , loceLimit :: [LimitOutputEvent]
  } deriving (Eq, Ord, Show)

makeLenses ''LimitOutputCmd

instance Mergable Int LimitOutputEvent where
  type ToMerge Int LimitOutputEvent = LimitOutput
  mergeOutput eSample (LimitOutput eLimit bLimit) =
    unionWith combineResult
      (mergeE bLimit _LLimit eLimit)
      (mergeB bLimit eSample)
```

### Testing `handleMessage`

TODO talk about how we test handleMessage, se we can see what happens with behaviors in the inputs

```haskell
data MessageInputEvent =
    MMessage String
  | MLimit Int
  deriving (Eq, Ord, Show)

makePrisms ''MessageInputEvent

data MessageInputCmd = MessageInputCmd {
    _micbLimit :: Int
  , _micEvent :: MessageInputEvent
  } deriving (Eq, Ord, Show)

makeLenses ''MessageInputCmd

instance Fannable Int MessageInputEvent where
  type Fanned Int MessageInputEvent = MessageInput
  fanInput eIn =
    MessageInput <$>
      fanE _MMessage eIn <*>
      fanE _MLimit eIn <*>
      fanB 0 id eIn

data MessageOutputCmd =
  MWrite String
  deriving (Eq, Ord, Show)

makePrisms ''MessageOutputCmd

instance Mergable () MessageOutputCmd where
  type ToMerge () MessageOutputCmd = MessageOutput
  mergeOutput _ (MessageOutput eWrite) =
    mergeE (pure ()) _MWrite eWrite
```
-->
## Next up

Now that we have some idea of what we can do with behaviors, we're going to start putting together some of the pieces we'll end up using in our chat server.

We'll begin by putting together all of the pieces that we can inside of a command line application, in order to introduce one or two more tricks.

[Onwards!](./components.html)
