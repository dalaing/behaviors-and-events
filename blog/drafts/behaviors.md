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
    format l m = m ++ " (last message: " ++ l ++ ")"
    eOut = format <$> bMessages <@> eMessage
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
    format l m = m ++ " (last message length: " ++ show l ++ ")"
    eOut = format <$> bLastLength <@> eMessage
  return $ MessageOutput eOut
```
It also shows that I'm not lying about the `Functor` instance.

### All of the previous messages

We use another function to do more interesting things with behaviors:
```haskell
accumB :: MonadMoment m => a -> Event (a -> a) -> m (Behavior a) 
accumB x eFunction = ...
```

This is like a combination of `stepper` and `accumE`:
<table><tr><td width=50%>
```haskell
toggler :: MonadMoment m 
        => Event ()
        -> m (Behavior Colour)
toggler eInput = do
  eOutput <- 
    accumB Red (flip <$ eInput)
  return eOutput
```
</td><td width=50%>
![](../images/accumB-flip.png)\
</td></tr></table>

It is like `accumE` because it accumulates the composition of the functions inside the various occurrences of the event, and it is like `stepper` because the change isn't observable until the next logical moment in time.

Looking at `accumE` and `accumB` side by side might help:
<table><tr><td width=50%>
![](../images/accumE-flip.png)\
</td><td width=50%>
![](../images/accumB-flip.png)\
</td></tr></table>

As a reminder, both `accumE` and `accumB` work nicely with `unions`:
```haskell
unions :: [Event (a -> a)] -> Event (a -> a)
```
if we want to work with several events.

We can use this to accumulate all of the messages that we've seen:
```haskell
-- see Part3.Example3
handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] $ 
    (\x xs -> x : xs) <$> eMessage

  let
    format ls m = m ++ " (previous messages: " ++ show ls ++ ")"
    eOut = format <$> bMessages <@> eMessage
        
  return $ MessageOutput eOut
```

### Conditional events based on behaviors

We can filter events using the value of a behavior at the time of the event using `whenE`:
```haskell
whenE :: Behavior Bool -> Event a -> Event a 
```

This gives us one (slightly contrived) option for not printing anything special when the history is empty:
```haskell
-- see Part3.Example4
handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] $
    (\x xs -> x : xs) <$> eMessage

  let
    format ls m = 
      m ++ 
      " (previous messages: " ++ show ls ++ ")"
    bHasMessages = (not . null) <$> bMessages
    eMessageWithHistory = whenE bHasMessages eMessage
    eOut = leftmost [
        format <$> bMessages <@> eMessageWithHistory
      , eMessage
      ]

  return $ MessageOutput eOut
```

This is a nice example of using `leftmost` as like a `switch-case-default` statement.
All of the inputs to `leftmost` are based on `eMessages`, but we have made one of them conditional.
If the condition is met then we get the conditional event, otherwise we get the default event that is the last element in the list.

### Trimming the history

If we just want to print the last 3 messages that we've seen we can do that:
```haskell
-- see Part3.Example5
handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] $
    (\x xs -> take 3 (x : xs)) <$> eMessage

  let
    format ls m = m ++ " (previous messages: " ++ show ls ++ ")"
    bHasMessages = (not . null) <$> bMessages
    eMessageWithHistory = whenE bHasMessages eMessage
    eOut = leftmost [
        format <$> bMessages <@> eMessageWithHistory
      , eMessage
      ]

  return $ MessageOutput eOut
```

We do the trimming inside of the accumulation so that we don't have a `Behavior` with collecting a list with unbounded length floating around in our system.

Hard-coding the `3` in there should be making us feel a little ill.

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
  bMessages <- accumB [] $
    (\n x xs -> take n (x : xs)) <$> bLimit <@> eMessage

  let
    format ls m = m ++ " (previous messages: " ++ show ls ++ ")"
    bHasMessages = (not . null) <$> bMessages
    eMessageWithHistory = whenE bHasMessages eMessage
    eOut = leftmost [
        format <$> bMessages <@> eMessageWithHistory
      , eMessage
      ]

  return $ MessageOutput eOut
```

If we want to hard-code a limit of 3 for the time being, we can set it up with `pure 3`.
Later on, we might add some code for admins to alter the limit - if that contributes to a behavior, and if our various components take their parameters as behaviors, then all we need to do is wire it up to our components.

### A component for the history limit

In order to modify the limit as we go, we need to introduce a few more pieces.

Let's assume that the limit on the history starts at 1, and is modified by two events: one increments the limit, the other decrements the limit unless it is already at 0.

We can build a behavior for the limit using `accumB`:
```haskell
-- see Part3.Example7
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

The behavior gives us something like a step function, but we might end up wanting an event that triggers when the behavior changes.

We can do that with `accumE`:
```haskell
eLimit <- accumE 1 . unions $ [
    succ <$ eUp
  , (max 0 . pred) <$ eDown
  ]
```

If we wind up needing both, we can efficiently combine the two of these by using `mapAccum`:
```haskell
mapAccum :: MonadMoment m => acc -> Event (acc -> (x, acc)) -> m (Event x, Behavior acc) 
```

This version uses `mapAccum` to do this:
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

Having both a behavior and an event that triggers when the behavior changes can be handy, so you'll see this idiom appear in a few of the `reactive-banana` examples floating around on the internet.

It's useful in this example as well.

Let's have a look at a sample interaction with our program:
```haskell
> a
a
> b
b (previous messages: ["a"])
> c
c (previous messages: ["b"])
> /limitup
> d
d (previous messages: ["c"])         -- (1)
> e
e (previous messages: ["d","c"])
> f
f (previous messages: ["e","d"])
> /limitup
> g
g (previous messages: ["f","e"])
> h
h (previous messages: ["g","f","e"])
> i
i (previous messages: ["h","g","f"])
> /limitdown
> j
j (previous messages: ["i","h","g"]) -- (2)
> k
k (previous messages: ["j","i"])
> l
l (previous messages: ["k","j"])
```
Notes:

1. This is fine - we have raised the limit, but have fewer messages in history than the limit.
2. This is less good - we have lowered the limit, another message has come through, and we're still storing more than the current limit.

It's not the end of the world, but we'd like to trim that history a little sooner.

In order to do that we'll use the version of the 'limit' component that produces both an event and a behavior, and we'll update our 'message' component to make use of it:
```haskell
data MessageInput = MessageInput {
    mieMessage :: Event String
  , mieLimit   :: Event Int
  , mibLimit   :: Behavior Int
  }

data MessageOutput = MessageOutput {
    moeWrite :: Event String
  }

handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage eLimit bLimit) = do
  bMessages <- accumB [] $ unions [
      (\n x xs -> take n (x : xs)) <$> bLimit <@> eMessage
    , take <$> eLimit                                              -- (1)
    ]

  let
    format ls m = m ++ " (previous messages: " ++ show ls ++ ")"
    bHasMessages = (not . null) <$> bMessages
    eMessageWithHistory = whenE bHasMessages eMessage
    eOut = leftmost [
        format <$> bMessages <@> eMessageWithHistory
      , eMessage
      ]

  return $ MessageOutput eOut
```
Notes:

1. We trim the messages when we get an event indicating that the limit has changed.

Now that same interaction with the new program looks like this:
```haskell
> a
a
> b
b (previous messages: ["a"])
> c
c (previous messages: ["b"])
> /limitup
> d
d (previous messages: ["c"])
> e
e (previous messages: ["d","c"])
> f
f (previous messages: ["e","d"])
> /limitup
> g
g (previous messages: ["f","e"])
> h
h (previous messages: ["g","f","e"])
> i
i (previous messages: ["h","g","f"])
> /limitdown
> j
j (previous messages: ["i","h"])     -- (1)
> k
k (previous messages: ["j","i"])
> l
l (previous messages: ["k","j"])
```
Notes

1. The message history has been trimmed in a timely fashion.

## Testing with behaviors

This will be filled in once I have dealt with some rough edges.

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

We will use the input event to our event network for this extra argument, which will provide an event for every observable point of time in our event network.
We use this to sample the output behaviors.

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
fanE :: (Testable m, Fannable b e) 
     => Prism' e a                -- ^ a prism used to extract this particular Event
     -> Event (Command b e)       -- ^ the combined Event we're extracting this Event from
     -> m (Event a)               -- ^ the particular Event we are after

fanB :: (Testable m, Fannable b e) 
     => a                         -- ^ the initial value for the Behavior we're extracting
     -> Lens' b a                 -- ^ a lens to used to extract this particular Behavior 
                                  --   from our combined Behavior type b
     -> Event (Command b e)       -- ^ the combined Event we're extracting this Behavior from
     -> m (Behavior a)            -- ^ the reconstituted Behavior
```
and for `Mergable`:
```haskell
-- We're grabbing the values of multiple Events, without knowing if they're happening simultaneously.
-- We want to track this - which is why we record a list of event values per time slice.
-- On top of all of that, we're also sampling the Behaviors at every point in time, so there's collisions 
-- happening there as well.
-- 
-- We use this function with `unionWith` to sort that out.
combineResult :: Result b e -> Result b e -> Result b e
combineResult (Result b1 e1) (Result b2 e2) =
  Result 
    -- When this collision happens, it'll happen at the same point in time, so the two sampled values for the 
    -- Behaviors should be the same, so we can just chose either one.
    b1 
    -- When we sample a Behavior, we use an empty list for the Event portion of the Result
    (e1 ++ e2)

mergeE :: Mergable b e 
       => Behavior b 
       -> Prism' e a 
       -> Event a 
       -> Event (Result b e)

mergeB :: Mergable b e 
       => Behavior b 
       -> Event () 
       -> Event (Result b e)
```

Let's take a look at them in action.

### Testing `handleLimit`

TODO mention that we're working with the version that outputs both events and behaviors

```haskell
data LimitInputE =
    LLimitUp
  | LLimitDown
  deriving (Eq, Ord, Show)

makePrisms ''LimitInputE

instance Fannable () LimitInputE where
  type Fanned () LimitInputE = LimitInput
  fanInput eIn =
    LimitInput <$>
      fanE _LLimitUp eIn <*>
      fanE _LLimitDown eIn
```

```haskell
instance Mergable Int Int where
  type ToMerge Int Int = LimitOutput
  mergeOutput eSample (LimitOutput eLimit bLimit) =
    unionWith combineResult
      (mergeE bLimit id eLimit)
      (mergeB bLimit eSample)
```

```haskell
testLimit ::    [Maybe (Command () LimitInputE)] 
          -> IO [Maybe (Result Int Int)]
testLimit = 
  testNetwork handleLimit
```

```haskell
> xs <- testLimit [
    Just (Command () LLimitUp)
  , Just (Command () LLimitUp)
  , Just (Command () LLimitDown)
  ]
> xs
[ Just (Result {_resB = 1, _resE = [2]})
, Just (Result {_resB = 2, _resE = [3]})
, Just (Result {_resB = 3, _resE = [2]})
]
```

### Testing `handleMessage`

TODO talk about how we test handleMessage, se we can see what happens with behaviors in the inputs

```haskell
data MessageInputE =
    MMessage String
  | MLimit Int
  deriving (Eq, Ord, Show)

makePrisms ''MessageInputE

instance Fannable Int String where
  type Fanned Int MessageInputE = MessageInput
  fanInput eIn =
    MessageInput <$>
      fanE _MMessage eIn <*>
      fanB 0 id eIn
```

```haskell
instance Mergable () String where
  type ToMerge () String = MessageOutput
  mergeOutput _ (MessageOutput eWrite) =
    mergeE (pure ()) _MWrite eWrite
```

```haskell
testMessage ::    [Maybe (Command Int String)] 
            -> IO [Maybe (Result () String)]
testMessage = 
  testNetwork handleMessage
```
-->

## Next up

Now that we have some idea of what we can do with behaviors, we're going to start putting together some of the pieces we'll end up using in our chat server.

We'll begin by putting together all of the pieces that we can inside of a command line application, in order to introduce one or two more tricks.

[Onwards!](./components.html)
