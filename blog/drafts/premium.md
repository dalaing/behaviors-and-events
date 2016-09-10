
## A premium echo application
##### [(The code for this section is here)](https://github.com/dalaing/behaviors-and-events/tree/master/code/talk/src/Part3/Account/)

The next post is going to be pushing us towards the chat server functionality that we're after.

Before I start that earnest, I'm going to develop a whimsical feature in this section to demonstrate some of the options we have for filtering events with behaviors.

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

handleUpgrade :: MonadMoment m => UpgradeInput -> m UpgradeOutput
handleUpgrade (UpgradeInput eUpgrade) = do
  bAccount <- stepper Plebian (Premium <$ eUpgrade)
  return $ UpgradeOutput bAccount
```

The cunning plan is to limit `Plebian` users to a certain number of messages per interaction with our application, while `Premium` users can send an unlimited number of messages.

If a `Plebian` user reaches the hard limit, the application will print something and exit.
There will also be a soft limit, which will remind the user that they can upgrade.
Marketing wanted to bug the user every second message, but we told them there were technical reasons why that wasn't feasible.

We're going to start with a soft limit of 5 messages and a hard limit of 10 messages.

We know what we want to do once we reach the various limits, and can encapsulate that:
```haskell
data LimitEvents = LimitEvents {
    lieSoftLimit :: Event ()
  , lieHardLimit :: Event ()
  }

data LimitOutput = LimitOutput {
    loeWrite :: Event String
  , loeQuit  :: Event ()
  }

translateLimitEvents :: LimitEvents -> LimitOutput
translateLimitEvents (LimitEvents eSoftLimit eHardLimit) =
  let
    eSoftLimitMessage =
      "You are using a Plebian account.  Consider upgrading to a Premium account for unlimited messages." <$ eSoftLimit
    eHardLimitMessage =
      "You have reached your message limit for a Plebian account, please upgrade." <$ eHardLimit
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

Once we work out how to create `LimitEvents` values, we'll have something like this for our event network:
```haskell
domainNetworkDescription :: Inputs -> Moment Outputs
domainNetworkDescription (Inputs eOpen eMessage eUpgrade eHelp eQuit eUnknown) = do
  OpenOutput eoWrite         <- handleOpen $ OpenInput eOpen
  UpgradeOutput bAccount     <- handleUpgrade $ UpgradeInput eUpgrade
  LimitOutput elWrite elQuit <- fmap translateLimitEvents ???
  MessageOutput emWrite      <- handleMessage $ MessageInput eMessage
  HelpOutput ehWrite         <- handleHelp $ HelpInput eHelp
  QuitOutput eqWrite eqQuit  <- handleQuit $ QuitInput eQuit
  UnknownOutput euWrite      <- handleUnknown $ UnknownInput eUnknown
  return $ 
    Outputs 
      [eoWrite, elWrite ,emWrite, ehWrite, eqWrite, euWrite]
      [elQuit, eqQuit]
```

Now we just need to work out when the limits are reached.

There are few different ways that we can achieve this.

### Using `whenE` with fixed limits

Given and account type and the number of messages that have been sent so far, we can write functions that indicate when we consider that a limit has been reached:
```haskell
softLimitCheck :: AccountType -> Int -> Bool
softLimitCheck Plebian x = x `mod` 5 == 4
softLimitCheck Premium x = False

hardLimitCheck :: AccountType -> Int -> Bool
hardLimitCheck Plebian x = x >= 9
hardLimitCheck Premium x = False
```

The soft limit function is written so that we'll bug the user to upgrade at a particular frequency, regardless of hte hard limit.

If we have behaviors carrying the current account type and the number of lines we've seen so far, then we can use the `Applicative` instance and the above functions to create a `Behavior Bool`.

For the soft limit, this looks like:
```haskell
bSoftLimitReached = softLimitCheck <$> bAccount <*> bLines
```

We can use a `Behavior Bool` to selectively filter events, using `whenE`:
```haskell
whenE :: Behavior Bool -> Event a -> Event a 
```
which only allows activations of the input event to pass through it when the behavior has value `True`.

This can be used to create an event that will activate whenever we hit our soft limit:
```haskell
eSoftLimitReached = () <$ whenE bSoftLimitReached eMessage
```

That is all the pieces we need to create the `LimitEvents` value we were after:
```haskell
-- see Part3.Account.Example1
data LimitInput = LimitInput {
    lieMessage :: Event String
  , libAccount :: Behavior AccountType
  }

handleLimit :: MonadMoment m => LimitInput -> m LimitEvents
handleLimit (LimitInput eMessage bAccount) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  let
    bSoftLimitReached = softLimitCheck <$> bAccount <*> bLines
    bHardLimitReached = hardLimitCheck <$> bAccount <*> bLines

    eSoftLimitReached = () <$ whenE bSoftLimitReached eMessage
    eHardLimitReached = () <$ whenE bHardLimitReached eMessage
  return $ LimitEvents eSoftLimitReached eHardLimitReached
```

This slots into our event network nicely:
```haskell
domainNetworkDescription :: MonadMoment m => Inputs -> m Outputs
domainNetworkDescription (Inputs eOpen eMessage eUpgrade eHelp eQuit eUnknown) = do
  OpenOutput eoWrite         <- handleOpen $ OpenInput eOpen
  UpgradeOutput bAccount     <- handleUpgrade $ UpgradeInput eUpgrade
  LimitOutput elWrite elQuit <- fmap translateLimitEvents .
                                handleLimit $ LimitInput eMessage bAccount
  MessageOutput emWrite      <- handleMessage $ MessageInput eMessage
  HelpOutput ehWrite         <- handleHelp $ HelpInput eHelp
  QuitOutput eqWrite eqQuit  <- handleQuit $ QuitInput eQuit
  UnknownOutput euWrite      <- handleUnknown $ UnknownInput eUnknown
  return $ 
    Outputs 
      [eoWrite, elWrite, emWrite, ehWrite, eqWrite, euWrite]
      [elQuit, eqQuit]
```

### Using `whenE` with variable limits

It is probably good practice to set the actual limits via behaviors rather than just hard coding them.

We need to pass in those limits as behaviors:
```haskell
data LimitInput = LimitInput {
    lieMessage   :: Event String
  , libAccount   :: Behavior AccountType
  , libSoftLimit :: Behavior Int
  , libHardLimit :: Behavior Int
  }
```
and we need to update our limit-checking functions:
```haskell
softLimitCheck :: Int -> AccountType -> Int -> Bool
softLimitCheck n Plebian x = x `mod` n == (n - 1)
softLimitCheck _ Premium _ = False

hardLimitCheck :: Int -> AccountType -> Int -> Bool
hardLimitCheck n Plebian x = x >= (n - 1)
hardLimitCheck _ Premium _ = False
```

After that, we just need to connect the new behaviors to those functions:
```haskell
-- see Part3.Account.Example2
handleLimit :: MonadMoment m => LimitInput -> m LimitEvents
handleLimit (LimitInput eMessage bAccount bSoftLimit bHardLimit) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  let
    bSoftLimitReached = softLimitCheck <$> bSoftLimit <*> bAccount <*> bLines
    bHardLimitReached = hardLimitCheck <$> bHardLimit <*> bAccount <*> bLines

    eSoftLimitReached = () <$ whenE bSoftLimitReached eMessage
    eHardLimitReached = () <$ whenE bHardLimitReached eMessage
  return $ LimitEvents eSoftLimitReached eHardLimitReached
````

### Using `filterApply`

If we need the event being filtered to take part in the decision along with some behaviours, we can use `filterApply`:
```haskell
filterApply :: Behavior (a -> Bool) -> Event a -> Event a 
```

We can see this in use if we change `handleLimit` to accumulate an event rather than a behavior to track the number of messages sent so far:
```haskell
-- see Part3.Account.Example3
handleLimit :: MonadMoment m => LimitInput -> m LimitEvents
handleLimit (LimitInput eMessage bAccount bSoftLimit bHardLimit) = do
  eLines <- accumE (-1) ((+ 1) <$ eMessage)
  let
    bSoftLimitFn = softLimitCheck <$> bSoftLimit <*> bAccount
    bHardLimitFn = hardLimitCheck <$> bHardLimit <*> bAccount

    eSoftLimitReached = () <$ filterApply bSoftLimitFn eLines
    eHardLimitReached = () <$ filterApply bHardLimitFn eLines
  return $ LimitEvents eSoftLimitReached eHardLimitReached
```

This is a bit contrived, but it's enough to demonstrate the function.

### Using `switchB`

The solutions so far get the job done, but we can do better.

Once we've upgraded our account we don't need to count the number of messages coming through anymore - but out various solutions so far will keep counting in the background.

We can change our behaviors based on various events by using `switchB`:
```haskell
switchB :: MonadMoment m => Behavior a -> Event (Behavior a) -> m (Behavior a) 
```

The output begins as the first behavior.
Whenever the event fires, the output becomes the behavior contained in the event.

With `switchB` in play, we only need the limit functions for the plebian accounts:
```haskell
softLimitCheck :: Int -> Int -> Bool
softLimitCheck n x = x `mod` n == (n - 1)

hardLimitCheck :: Int -> Int -> Bool
hardLimitCheck n x = x >= (n - 1)
```
and we use `pure True` for the premium accounts.

We build our `Behavior Bool` out of these pieces like this:
```haskell
  let
    bPlebianSoft = softLimitCheck <$> bSoftLimit <*> bLines
    bPremiumSoft = pure False

  bSoft <- switchB bPlebianSoft (bPremiumSoft <$ eUpgrade)
```

The overall component looks like this:
```haskell
-- see Part3.Account.Example4
data LimitInput = LimitInput {
    lieUpgrade   :: Event ()
  , lieMessage   :: Event String
  , libSoftLimit :: Behavior Int
  , libHardLimit :: Behavior Int
  }

handleLimit :: MonadMoment m => LimitInput -> m LimitEvents
handleLimit (LimitInput eUpgrade eMessage bSoftLimit bHardLimit) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)

  let
    bPlebianSoft = softLimitCheck <$> bSoftLimit <*> bLines
    bPremiumSoft = pure False
    bPlebianHard = hardLimitCheck <$> bHardLimit <*> bLines
    bPremiumHard = pure False

  bSoft <- switchB bPlebianSoft (bPremiumSoft <$ eUpgrade)
  bHard <- switchB bPlebianHard (bPremiumHard <$ eUpgrade)

  let
    eSoftLimitReached = () <$ whenE bSoft eMessage
    eHardLimitReached = () <$ whenE bHard eMessage
  return $ LimitEvents eSoftLimitReached eHardLimitReached
```

It's a small win now, but it'll become more significant as we work on more complex networks.

We can also use multiple events to switch to multiple behaviors, and events which fire multiple times to jump back and forth between different behaviors - there's a lot we can do with this stuff.

### Using `switchE`

We can go a little further in this case using `switchE`:
```haskell
switchE :: MonadMoment m => Event (Event a) -> m (Event a) 
```
This uses the inner event between firings of the outer event.

An example should help.
In our case we'll be using it like this:
```haskell
eSoftLimitReached <- switchE . leftmost $ [
    eSoftPlebianLimitReached <$ eOpen
  , eSoftPremiumLimitReached <$ eUpgrade
  ]
```
and the output will be `eSoftPlebianLimitReached` from the time `eOpen` is activated until `eUpgrade` is activated, at which point it will switch to `eSoftPremiumLimited`.

It would be nice if `switchE` took an initial event to use until the first outer even fired - both because it would be useful and because it would by symmetric with `switchB` - but we do what we can with what we have.

To use this, we'll pass a few more events into our input data structure:
```haskell
data LimitInput = LimitInput {
    lieOpen      :: Event ()
  , lieUpgrade   :: Event ()
  , lieMessage   :: Event String
  , libSoftLimit :: Behavior Int
  , libHardLimit :: Behavior Int
  }
```

Then we'll write three components.

One of them is for use with the plebian accounts:
```haskell
handleLimitPlebian :: MonadMoment m => LimitInput -> m LimitEvents
handleLimitPlebian (LimitInput _ _ eMessage bSoftLimit bHardLimit) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)

  let
    bSoft = softLimitCheck <$> bSoftLimit <*> bLines
    bHard = hardLimitCheck <$> bHardLimit <*> bLines
    eSoftLimitReached = () <$ whenE bSoft eMessage
    eHardLimitReached = () <$ whenE bHard eMessage

  return $ LimitEvents eSoftLimitReached eHardLimitReached
```

One of them is for use with the premium accounts:
```haskell
handleLimitPremium :: MonadMoment m => LimitInput -> m LimitEvents
handleLimitPremium _ =
  return $ LimitEvents never never
```

The last of them is used to switch between the two:
```haskell
handleLimit :: MonadMoment m => LimitInput -> m LimitEvents
handleLimit li@(LimitInput eOpen eUpgrade _ _ _) = do
  LimitEvents eSoftPlebian eHardPlebian <- handleLimitPlebian li
  LimitEvents eSoftPremium eHardPremium <- handleLimitPremium li

  eSoftLimitReached <- switchE . leftmost $ [
      eSoftPlebian <$ eOpen
    , eSoftPremium <$ eUpgrade
    ]

  eHardLimitReached <- switchE . leftmost $ [
      eHardPlebian <$ eOpen
    , eHardPremium <$ eUpgrade
    ]

  return $ LimitEvents eSoftLimitReached eHardLimitReached
```

### Using a typeclass for switching

We can make this a bit more convenient by introducing a typeclass to help us with switching:
```haskell
class Switch a where
  switch :: MonadMoment m => a -> Event a -> m a
```

We provide instances for behaviors:
```haskell
instance Switch (Behavior a) where
  switch = switchB
```
and for events:
```haskell
instance Switch (Event a) where
  switch _ = switchE
```

We also provide a convenience function:
```haskell
switchAp :: (Switch b, MonadMoment m) 
         => (a -> b) -> a -> Event a -> m b
switchAp f a e = 
  switch (f a) (f <$> e)
```

We make use of these to provide an instance for `LimitEvents`
```haskell
instance Switch LimitEvents where
  switch e ee = 
    LimitEvents <$> 
      switchAp lieSoftLimit e ee <*> 
      switchAp lieHardLimit e ee
```

This makes our component easier to write and easier to read:
```haskell
-- see Part3.Account.Example6
emptyLimitEvents :: LimitEvents
emptyLimitEvents =
  LimitEvents never never

handleLimit :: MonadMoment m => LimitInput -> m LimitEvents
handleLimit li@(LimitInput eOpen eUpgrade _ _ _) = do
  plebianEvents <- handleLimitPlebian li
  premiumEvents <- handleLimitPremium li

  switch emptyLimitEvents . leftmost $ [
      plebianEvents <$ eOpen
    , premiumEvents <$ eUpgrade
    ]
```

We still need to provide an initial object, but that is fine.
Sometimes we'll be working with objects that contain behaviors as well as events, and in those cases we'll need that initial object in order to give the behaviors their initial values.

This would change if `switchE` took an initial event to use before the first firing.

The `Switch` instance for `Event` would change to match:
```haskell
instance Switch (Event a) where
  switch = switchE
```
and we'd no longer need to use `eOpen` to get things started:
```haskell
  switch plebianEvents (premiumEvents <$ eUpgrade)
```

### Using `observeE` and `execute`

```haskell
handleLimit :: MonadMoment m => LimitInput -> m LimitEvents
handleLimit li@(LimitInput eOpen eUpgrade _ _ _) = do
  plebianEvents <- handleLimitPlebian li
  premiumEvents <- handleLimitPremium li

  switch emptyLimitEvents . leftmost $ [
      plebianEvents <$ eOpen
    , premiumEvents <$ eUpgrade
    ]
```

```haskell
handleLimit :: LimitInput -> Moment LimitEvents
handleLimit li@(LimitInput eOpen eUpgrade _ _ _) = do
  let
    plebianEvents = handleLimitPlebian li
    premiumEvents = handleLimitPremium li

  switch emptyLimitEvents . observeE . leftmost $ [
      plebianEvents <$ eOpen
    , premiumEvents <$ eUpgrade
    ]
```

TODO update refactoring and behaviors to reflect the new typeclasses and code structure
TODO once interpret is introduced, start using testNetwork in doctests for the various networks
TODO - although this requires that we have the ability to fan in and fan out our events
TODO - also means we have to be able to deal with behaviors going in and out of these things

TODO clean up earlier mentions of pruning the network and GC - maybe recast in terms of blocks you can dynamically chose to be involved with the network to various degrees
TODO mention timing issues with an open event, how that comes from the missing argument in switchE
TODO mention onceE in other systems - is onceE e = switchE e (never <$ e)?

TODO mention the momentio version of interpret back where interpret first came up
TODO possibly a typeclass to include things in the graph, abstract over observeE and execute
TODO possibly something similar for testing

TODO change Next up to link to a post on GC in reactive-banana

```haskell
handleLimit :: LimitInput -> MomentIO LimitEvents
handleLimit li@(LimitInput eOpen eUpgrade _ _ _) = do
  let
    plebianEvents = handleLimitPlebian li
    premiumEvents = handleLimitPremium li

  switchedLimitEvents <- execute . leftmost $ [
      plebianEvents <$ eOpen
    , premiumEvents <$ eUpgrade
    ]

  switch emptyLimitEvents switchedLimitEvents
```

### Other variations

There are a few variations on this that might be interesting to play with.

The first would be change the system from quitting when the hard limit is reached to just not accepting any more messages.
This would allow the users to either upgrade and continue or quit on their own terms once the limit was reached.

The second variation would be to start users off with a number of credits which are used when messages are sent, with a warning when credits are low and with messages not being processed once they have run out.
Just don't tell the people from marketing.
