---
title: Behaviors
published: 2016-10-01 12:00:00+10:00
---

[Previously](./echo.html) we developed a simple command line program and refactored it to make parts of it more reusable and testable.

# Behaviors

The next piece of the `FRP` puzzle is behaviors.
Where events are only defined at particular points in time, behaviors are defined for all points of time.

The documentation mentions that the semantics of an `Behavior a` allows you to view it as being similar to `Time -> a`.
In contrast to events, behaviors have values for every point in time.

This lets us use behaviors to model program state, since the program state is a value that changes over time.
We can also pass them as arguments as something like a 'live variable' or a 'first-class observer'.
All in good time.

The type in `reactive-banana` is:
```haskell
data Behavior a
```
and it has a `Functor` instance and an `Applicative` instance.
```haskell
instance Functor Behavior
instance Applicative Behavior
```

The `Applicative` instance means that we can create a behavior that has a constant value across all times using `pure`, and we can stitch them together using `<*>`.

```haskell
-- bInt and bString defined elsewhere

bBool :: Behavior Bool
bBool = pure True

bTriple :: Behavior (Int, String, Bool)
bTriple = (,,) <$> bInt <*> bString <*> bBool
```

The more common (and more interesting) way to create a behavior is with `stepper`:
```haskell
stepper :: MonadMoment m => a -> Event a -> m (Behavior a)
stepper x eNext = ...
```

This will create a `Behavior` that starts with the value `x`, and then changes its to the value the event `eNext` every time that event occurs.
This change will not be observable until the next logical clock tick _after_ the event `eNext`.

The time delay for the change stands out in the example diagrams:
<table><tr><td width=50%>
```haskell
holder :: MonadMoment m 
       => Event Colour 
       -> m (Behavior Colour)
holder eInput = do
  eOutput <-
    stepper Blue eInput
  return eOutput
```
</td><td width=50%>
![](../images/stepper.png)\
</td></tr></table>

If we're viewing this as state manipulation, then we should be able to use a `State` monad simile.
Imagine that we've identified a handful of conditions that would cause us to use `put` at various times and with various values.
We already have the tools to combine all of those calls to `put` into the `eInput` event - at which point we could use `stepper` to create a `Behavior` that models all of the changes to this particular piece of state over time.

We commonly build `Behavior`s using `Event`s, and it turns out that we also sample `Behavior`s using `Event`s.
The `Event` is what give us the logical clock tick to use to sample the `Behavior`.

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

We can demonstrate these pictorially.

Sometimes we want to involve the value of the `Event`:
<table><tr><td width=50%>
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
</td><td width=50%>
![](../images/sample-mix.png)\
</td></tr></table>
and sometimes we don't:
<table><tr><td width=50%>
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
</td><td width=50%>
![](../images/sample-const.png)\
</td></tr></table>

If we were squinting that the `Event` we used with `stepper` and imagining that it was the aggregation of all of our uses of `put` in the `State` monad, then we can view the `Event` we use with `<@` as the aggregation of some uses of `get`.

The last major use of `Behavior`s is to filter `Event`s, where the filter condition that changes over time:
```haskell
whenE       :: Behavior Bool -> Event a -> Event a
filterApply :: Behavior (a -> Bool) -> Event a -> Event a
```

Assuming `True` is dark gray and `False` is light gray, we would have:
<table><tr><td width=50%>
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
</td><td width=50%>
![](../images/whenE.png)\
</td></tr></table>

What has this given us?
It has given us a way to manage state inside an FRP system.

We have a lot of freedom here.

We can develop several `Behavior`s and combine them together with the `Applicative` typeclass.
The `Behavior`s don't need to know anything about each other - maybe they're all change at the same points in time, maybe they're changing at complete distinct points in time.

We can sample these `Behavior`s with any `Event`s that we have, and again we don't know and don't care if the points in time the `Event`s fire at correspond to the points in time where the `Behavior` is changing.

The independence and composability of the pieces are what makes FRP fun to work with.

## An example

We're going to model a user logging in and logging out of a website.
The goal is track whether they are currently logged in or logged out.

We'll start with a data type for the login state:
```haskell
data LoginState = 
    LoggedIn
  | LoggedOut
  deriving (Eq, Ord, Show)
```
and a pair of data types for the inputs and outputs of our component:
```haskell
data LoginInputs = LoginInputs {
    lieLogIn  :: Event ()
  , lieLogOut :: Event ()
  }
  
data LoginOutputs :: LoginOutputs {
    lobState :: Behavior LoginState
  }
```

From there we just need to use `leftmost` to combine the events and `stepper` to turn them into a behavior:
```haskell
logInHandler :: MonadMoment m => LogInInputs -> m LoginOutputs
logInHandler (LogInEvents eLogIn eLogOut) = do
  bState <- stepper LoggedOut . leftmost $ [
      LoggedIn  <$ eLogIn                                      -- (1)
    , LoggedOut <$ eLogOut                                     -- (2)
    ]
  return $ LoginOutputs bState
```
Notes:

1. Change the state to `LoggedIn` when the user logs in.
2. Change the state to `LoggedOut` when the user logs out.

All is well at this point.

If we want to add more meat to the problem, we can disallow logging in while already logged in or logging out while already logged out.

We add a data type for the kinds of errors we might see:
```haskell
data LoginError =
    AlreadyLoggedIn
  | NotLoggedIn
  deriving (Eq, Ord, Show)
```

We also write some helper functions to work out whether we should change state or signal that an error has occurred:
```haskell
logIn :: LogInState -> Either LoginError LoginState
logIn LoggedIn  = Left AlreadyLoggedIn
logIn LoggedOut = Right LoggedIn

logOut :: LogInState -> Either LoginError LoginState
logOut LoggedOut = Left NotLoggedIn
logOut LoggedIn  = Right LoggedOut
```

We add an event signaling that an error has occurred to our output:
```haskell
data LoginOutputs :: LoginOutputs {
    lobState :: Behavior LoginState
  , loeError :: Event LoginError
  }
```
and we're ready to begin.

I'm going to go slowly through this part.

We're going to make use of an extension that gets used a bit in the FRP world:
```haskell
{-# RecursiveDo #-}
```

This allows us to use `mdo` instead of `do` to indicate that we want to use value recursion:
```haskell
logInHandler :: LoginInputs -> Moment LoginOutputs
logInHandler (LoginInputs eLogIn eLogOut) = mdo
  ...
  return $ LoginOutputs ??? ???
```

We want to start the user as `LoggedOut`.
It should be possible to use `stepper` to build our `Behavior LoginState` from that and some `Event`, which we'll deal with later:
```haskell
logInHandler :: LoginInputs -> Moment LoginOutputs
logInHandler (LoginInputs eLogIn eLogOut) = mdo
  bState <- stepper LoggedOut ???
  ...
  return $ LoginOutputs bState ???
```
At this point we have one of our outputs.

Now we can sample `bLoginState` in order to bring our helper functions into play:
```haskell
logInHandler :: LoginInputs -> Moment LoginOutputs
logInHandler (LoginInputs eLogIn eLogOut) = mdo
  bState <- stepper LoggedOut ???
  ??? leftmost $ [
      logIn  <$> bState <@ eLogIn
    , logOut <$> bState <@ eLogOut
    ]
  return $ LoginOutputs bState ???
```
which will give us an `Event (Either LoginError LoginState)`.

That can be split into an event for the case where there was an error and an event for the case where we should change the state:
```haskell
logInHandler :: LoginInputs -> Moment LoginOutputs
logInHandler (LoginInputs eLogIn eLogOut) = mdo
  bState <- stepper LoggedOut ???
  let (eLoginError, eLoginState) = split . leftmost $ [
        logIn  <$> bState <@ eLogIn
      , logOut <$> bState <@ eLogOut
      ]
  return $ LoginOutputs bState eLoginError
```
which gives us our second output.

Now we make use of the value recursion, and use `eLoginState` as the event that drives the call to `stepper`:
```haskell
logInHandler :: LoginInputs -> Moment LoginOutputs
logInHandler (LoginInputs eLogIn eLogOut) = mdo
  bState <- stepper LoggedOut eLoginState
  let
    (eLoginError, eLoginState) = split . leftmost $ [
        logIn  <$> bState <@ eLogIn
      , logOut <$> bState <@ eLogOut
      ]
  return $ LoginOutputs bState eLoginError
```

This creates a loop in our event network, but everything will be well behaved if we only recursive refer to `Behavior`s via `Event`s or to `Event`s via `Behavior`s.
The delays in the updates to `Behavior`s appear to be part of the reason that this works out so well.

Value recursion can take a little while to wrap your head around, but it's very helpful for expressing interesting problems elegantly and concisely.

There are two variations on the above that are worth pointing out.

If we want to use `MonadMoment`, we also need to use a `MonadFix` constraint:
```haskell
logInHandler :: (MonadMoment m, MonadFix m) => LoginInputs -> m LoginOutputs
```

If we want to limit the scope of the value recursion, we can use the `rec` keyword instead of `mdo`:
```haskell
logInHandler :: LoginInputs -> Moment LoginOutputs
logInHandler (LoginInputs eLogIn eLogOut) = do
  rec
    bLogInState <- stepper LoggedOut eLogInState
    let
      (eLogInError, eLogInState) = split . leftmost $ [
          logIn  <$> bLogInState <@ eLogIn
        , logOut <$> bLogInState <@ eLogOut
        ]
  let 
    notInScopeOfTheRecursion = True
  return $ LoginOutputs bLogInState eLogInError
```

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
