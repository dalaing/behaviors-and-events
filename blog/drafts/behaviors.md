---
title: Behaviors
published: 2016-09-01 12:00:00+10:00
---

[Previously](./events.html) we learned about how events work in FRP, and used what we learned to develop a simple command line program.
[We then](./refactoring.html) went on to refactor it in various ways, to explore different avenues for making our work reusable and testable.

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

This means that we can create a behavior that has a constant value across all times using `pure`.

A more interesting way to create a behavior is with `stepper`:
```haskell
stepper :: MonadMoment m => a -> Event a -> m (Behavior a)
stepper x eX = ...
```

This will create a `Behavior` that starts with the value `x`, and then changes its to the value the event `eX` every time that event occurs.
This change will not be observable until the next logical clock tick _after_ the event `eX`.

We always sample the values of behaviors using events, which let us know which logical clock tick to use to sample the behaviour.

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

## Message history

Let's use these along with `stepper` to demonstrate some of the above.

As a reminder, our previous take on message handling was pretty boring:
```haskell
data MessageInput  = MessageInput  { mieRead  :: Event String }
data MessageOutput = MessageOutput { moeWrite :: Event String }

handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) = do
  return $ MessageOutput eMessage
```

We'll spice that up by printing the last message that was echoed:
```haskell
handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- stepper "" eMessage
  let
    f l m = m ++ " (last message: " ++ l ++ ")"
    eOut = f <$> bMessages <@> eMessage
  return $ MessageOutput eOut
```

We're using `stepper` to keep the most recently seen message around so that we can sample it any time.
The updates to `stepper` are being driven by `eMessage`, and won't be seen until the next transaction, and so by sampling `bMessages` with `eMessage` we're able to grab the last message that we saw.

We've already got the last message on the screen, so maybe displaying the length of the last message would be more interesting:
```haskell
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

We use another function to do more interesting things with behaviors:
```haskell
accumB :: MonadMoment m => a -> Event (a -> a) -> m (Behavior a) 
accumB x eF = ...
```

This behaves like a scan.
The behavior starts with the value `x`, and every time the event `eF` occurs the function gets applied to the current value of the behavior.

This works nicely with `unions`:
```haskell
unions :: [Event (a -> a)] -> Event (a -> a)
```
if we want to build a behavior from a number of events.

We can use this to accumulate all of the messages that we've seen:
```haskell
handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- accumB [] . fmap (:) $ eMessage
  let
    f ms m = m ++ " (previous messages: " ++ show ms ++ ")"
    eOut = f <$> bMessages <@> eMessage
  return $ MessageOutput eOut
```

If we just want to print the last 3 messages that we've seen we can do that:
```haskell
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

We can get some future proofing for that by passing in the number of messages that we want as a behavior:
```haskell
data MessageInput = MessageInput {
    mieRead  :: Event String
    mibLimit :: Behavior Int
  }

handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage bLimit) = do
  bMessages <- accumB [] . fmap (:) $ eMessage
  let
    f n ms m = m ++ " (previous " ++ show n ++ " messages: " ++ show (take n ms) ++ ")"
    eOut = f <$> bLimit <*> bMessages <@> eMessage
  return $ MessageOutput eOut
```

There's a problem though: we have a behavior that accumulates all of the messages, and so uses unbounded memory.

For a fixed limit, we can just bring the `take 3` code into the `accumB`:
```haskell
handleMessage :: MonadMoment m => MessageInput -> m MessageOutput
handleMessage (MessageInput eMessage) = do
  let
    limitCons n x xs = take n (x : xs)
  bMessages <- accumB [] . fmap (limitCons 3) $ eMessage
  let
    f ms m = m ++ " (last 3 messages: " ++ show ms ++ ")"
    eOut = f <$> bMessages <@> eMessage
  return $ MessageOutput eOut (pure 0)
```

In order to modify the limit as we go, we need to introduce a few more pieces.

Let's assume that the limit on the history starts at 1, and is modified by two events: one increments the limit, the other decrements the limit unless it is already at 0.

We can build a behavior for the limit using `accumB`:
```haskell
bLimit <- accumB 1 . unions $ [
    (+ 1) <$ eUp
  , (max 0 . subtract 1) <$ eDown
  ]
```

The behavior gives us a step function, but what we're going to end up wanting is an event that triggers when the behavior changes.

We can get this by replacing `accumB` with `accumE`:
```haskell
accumE :: MonadMoment m => a -> Event (a -> a) -> m (Event a) 
accumE x eF = ...
```
like so:
```haskell
eLimit <- accumE 1 . unions $ [
    (+ 1) <$ eUp
  , (max 0 . subtract 1) <$ eDown
  ]
```

Unlike with `accumB`, we can observe these changes in the same transaction as the event that caused the change.

If we end up needing both, we can efficiently combine the two of these by using `mapAccum`:
```haskell
mapAccum :: MonadMoment m => acc -> Event (acc -> (x, acc)) -> m (Event x, Behavior acc) 
```
to get:
```haskell
(eLimit, bLimit) <- mapAccum 1 . fmap (\f x -> (f x, f x)) . unions $ [
    (+ 1) <$ eUp
  , (max 0 . subtract 1) <$ eDown
  ]
```

Having both a behavior and an event that triggers when the behavior changes can be handy.

Assuming we have both, we can prepare a component to manage and print the message history.

We set up our inputs and outputs:
```haskell
data HistoryInput = HistoryInput {
    hieMessage :: Event String
  , hieLimit :: Event Int
  , hibLimit :: Behavior Int
  }

data HistoryOutput = HistoryOutput {
    hoeWrite :: Event String
  }
```
and then we connect them up:
```haskell
addMessage :: Int -> String -> [String] -> [String]
addMessage n m ms =
  take n (m : ms)

handleHistory :: MonadMoment m => HistoryInput -> m HistoryOutput
handleHistory (HistoryInput eMessage eLimit bLimit) = do
  bHistory <- accumB [] . unions $ [
      take <$> eLimit
    , addMessage <$> bLimit <@> eMessage
    ]
  let
    f l h = "(last " ++ show l ++ "message: " ++ show h ++ ")"
    eWrite = f <$> bLimit <*> bHistory <@ eMessage
  return $ HistoryOutput eWrite
```

When the limit changes, we trim the list.
When a message comes in, we add the message to the front of the list and then trim it.
We output the history when a message comes in, but we don't output the message since another component is handling that - and so we use `<@` instead of `<@>` to synchronize `eWrite` and `eMessage`.

If we set up some data structures to make things neat and tidy:
```haskell
data History = History {
    hLimit :: Int
  , hMessages :: [String]
  }

changeLimit :: Int -> History -> History
changeLimit n (History _ ms) =
  History n (take n ms)

addMessage :: String -> History -> History
addMessage m (History n ms) =
  History n (take n $ m : ms)
```
we can get away with just the event for limit changes: 
```haskell
  bHistory <- accumB (History 1 []) . unions $ [
      changeLimit <$> eLimit
    , addMessage <$> eMessage
    ]
```

The problem with that is that it's recreating the work that we would have done to create a behavior out of the limit.

It seems that in cases like this it is better to set up both the event and behavior and to go with the design that flows from there.
If something else needs to know about the history limit later on, it can be handed the behavior and doesn't need to know anything about where the changes are coming from - and if we're doing that, then recreating the behavior inside our `History` data structure is code duplication that might bite us if the requirements change later on.

## A premium echo application

The next post is going to be pushing us towards the chat server functionality that we're after.

Before I start that earnest, I'm going to develop a whimsical feature in this section, to demonstrate some of the options we have for filtering events with behaviors.

As a prequel to our tale, assume that we've been tracking how many messages a user sends during their interaction with our application, and that we let them know about it when they quit.

The data structures are simple:
```haskell
data CounterInput = CounterInput {
    cieMessage :: Event String
  , cieQuit    :: Event ()
  }

data CounterOutput = CounterOutput {
    coeMessage :: Event String
  }
```
and the component is use `accumB` and `<@` to achieve the desired functionality.
```haskell
handleCounter :: MonadMoment m => CounterInput -> m CounterOutput
handleCounter (CounterInput eMessage eQuit) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  let
    f l = show l ++ " messages sent"
    eOut = f <$> bLines <@ eQuit
  return $ CounterOutput eOut
```

This integrates well with our existing network:
```haskell
domainNetworkDescription :: Inputs -> Moment Outputs
domainNetworkDescription (Inputs eOpen eMessage eHelp eQuit eUnknown) = do
  OpenOutput eoWrite        <- handleOpen $ OpenInput eOpen
  CounterOutput ecWrite     <- handleCounter $ CounterInput eMessage eQuit 
  MessageOutput emWrite     <- handleMessage $ MessageInput eMessage
  HelpOutput ehWrite        <- handleHelp $ HelpInput eHelp
  QuitOutput eqWrite eqQuit <- handleQuit $ QuitInput eQuit
  UnknownOutput euWrite     <- handleUnknown $ UnknownInput eUnknown
  return $ 
    Outputs [eoWrite, ecWrite ,emWrite, ehWrite, eqWrite, euWrite] [eqQuit]
```

Our story begins in earnest, as many stories often do, when someone decided to try to monetize something without first trying to understand if there was a market for it.

This immediately led to two different account types for our users:
```haskell
data AccountType =
    Plebian
  | Premium
  deriving (Eq, Ord, Show)
```
and the ability to upgrade an account:
```haskell
data UpgradeInput  = UpgradeInput  { uieUpgrade :: Event () }
data UpgradeOutput = UpgradeOutput { uobAccount :: Behavior AccountType }

upgrade :: UpgradeInput -> Moment UpgradeOutput
upgrade (UpgradeInput eUpgrade) = do
  bFns <- stepper Plebian (Premium <$ eUpgrade)
  return $ UpgradeOutput bFns
```

The cunning plan is to limit `Plebian` users to a certain number of messages per interaction with our application, while `Premium` users can send an unlimited number of messages.

If a `Plebian` user reaches the hard limit, the application will print something and exit.
There will also be a soft limit, which will remind the user that they can upgrade.
Marketing wanted to bug the user every second message, but we told them there were technical reasons why that wasn't feasible.

We're going to start with a soft limit of 5 messages and a hard limit of 10 messages.

We know what we want to do once we reach the various limits, and can encapsulate that:
```haskell
data LimitInput = LimitInput {
    lieSoftLimit :: Event ()
  , lieHardLimit :: Event ()
  }

data LimitOutput = LimitOutput {
    loeWrite :: Event String
  , loeQuit  :: Event ()
  }

handleLimit :: LimitInput -> LimitOutput
handleLimit (LimitInput eSoftLimit eHardLimit) =
  let
    eSoftLimitMessage =
      "You are using a Plebian account.  Consider upgrading to a Premium account for unlimited messages" <$ eSoftLimit
    eHardLimitMessage =
      "You have reached your message limit for a Plebian account, please upgrade" <$ eHardLimit
    eMessage =
      leftmost [
          eHardLimitMessage
        , eSoftLimitMessage
        ]
    eQuit =
      eHardLimit
  in
    LimitOutput eMessage eQuit
```

Now we just need to work out when the limits are reached.

There are few different ways that we can achieve this.

### Using `whenE`

```haskell
whenE :: Behavior Bool -> Event a -> Event a 
```

```haskell
data CounterInput = CounterInput {
    cieMessage :: Event String
  , cibAccount :: Behavior AccountType
  }
```

```haskell
softLimitCheck :: AccountType -> Int -> Bool
softLimitCheck Plebian x = x `mod` 5 == 4
softLimitCheck Premium x = False
```

```haskell
hardLimitCheck :: AccountType -> Int -> Bool
hardLimitCheck Plebian x = x >= 9
hardLimitCheck Premium x = False
```

```haskell
handleCounter :: MonadMoment m => CounterInput -> m LimitInput
handleCounter (CounterInput eMessage bAccount) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  let
    bSoftLimitReached = softLimitCheck <$> bAccount <*> bLines
    bHardLimitReached = hardLimitCheck <$> bAccount <*> bLines

    eSoftLimitReached = () <$ whenE bSoftLimitReached eMessage
    eHardLimitReached = () <$ whenE bHardLimitReached eMessage
  return $ LimitInput eSoftLimitReached eHardLimitReached
```

```haskell
data CounterInput = CounterInput {
    cieMessage   :: Event String
  , cibAccount   :: Behavior AccountType
  , cibSoftLimit :: Behavior Int
  , cibHardLimit :: Behavior Int
  }
```

```haskell
softLimitCheck :: Int -> AccountType -> Int -> Bool
softLimitCheck n Plebian x = x `mod` n == (n - 1)
softLimitCheck _ Premium _ = False
```

```haskell
hardLimitCheck :: Int -> AccountType -> Int -> Bool
hardLimitCheck n Plebian x = x >= (n - 1)
hardLimitCheck _ Premium _ = False
```

```haskell
handleCounter :: MonadMoment m => CounterInput -> m LimitInput
handleCounter (CounterInput eMessage bAccount bSoftLimit bHardLimit) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  let
    bSoftLimitReached = softLimitCheck <$> bSoftLimit <*> bAccount <*> bLines
    bHardLimitReached = hardLimitCheck <$> bHardLimit <*> bAccount <*> bLines

    eSoftLimitReached = () <$ whenE bSoftLimitReached eMessage
    eHardLimitReached = () <$ whenE bHardLimitReached eMessage
  return $ LimitInput eSoftLimitReached eHardLimitReached
````

### Using `filerApply`

If we need the event being filtered to take part in the decision along with some behaviours, we can use `filterApply`:
```haskell
filterApply :: Behavior (a -> Bool) -> Event a -> Event a 
```

We can see this in use if we change `handleCounter` to accumulate an event rather than a behavior to track the number of messages sent so far:
```haskell
handleCounter :: MonadMoment m => CounterInput -> m LimitInput
handleCounter (CounterInput eMessage bAccount bSoftLimit bHardLimit) = do
  eLines <- accumE (-1) ((+ 1) <$ eMessage)
  let
    bSoftLimitFn = softLimitCheck <$> bSoftLimit <*> bAccount
    bHardLimitFn = hardLimitCheck <$> bHardLimit <*> bAccount

    eSoftLimitReached = () <$ filterApply bSoftLimitFn eLines
    eHardLimitReached = () <$ filterApply bHardLimitFn eLines
  return $ LimitInput eSoftLimitReached eHardLimitReached
```

This is a bit contrived, but it's enough to demonstrate the function.

### Using `switchB`

```haskell
switchB :: MonadMoment m => Behavior a -> Event (Behavior a) -> m (Behavior a) 
```

### Using `switchE`

```haskell
switchE :: MonadMoment m => Event (Event a) -> m (Event a) 
```

### Other variations

There are a few variations on this that might be interesting to play with.

The first would be change the system from quitting when the hard limit is reached to just not accepting any more messages.
This would allow the users to either upgrade and continue or quit on their own terms once the limit was reached.

The second variation would be to start users off with a number of credits which are used when messages are sent, with a warning when credits are low and with messages not being processed once they have run out.
Just don't tell the people from marketing.

## Next up

Now that we have some idea of what we can do with behaviors, we're going to start putting together some of the pieces we'll end up using in our chat server.

<!-- [Onwards!](./chat-solo.html) -->
Coming soon...
