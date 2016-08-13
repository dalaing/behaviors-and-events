---
title: Behaviors
published: 2016-09-01 12:00:00+10:00
---

[Previously](./events.html) we learned about how events work in FRP, and used what we learned to develop a simple command line program.

# Behaviors

The next piece of the `FRP` puzzle is behaviors.
Where events are only defined at particular points in time, behaviors are defined for all points of time.

```haskell
Behavior a
```

```haskell
instance Functor Behavior
instance Applicative Behavior
```

Start a `Behavior` with the value `x`, then change it the value of the event `eX` every time that it fires.

```haskell
stepper :: MonadMoment m => a -> Event a -> m (Behavior a)
stepper x eX = ...
```
  
```haskell
data MessageInput = MessageInput {
    mieRead :: Event String
  }
```

```haskell
data MessageOutput = MessageOutput {
    moeWrite :: Event String
  }
```

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- stepper "" eMessage
  let
    f l m = m ++ " (last message: " ++ l ++ ")"
    eOut = f <$> bMessages <@> eMessage
  return $ MessageOutput eOut
```

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  bMessages <- stepper "" eMessage
  let
    bLastLength = length <$> bMessages
    f l m = m ++ " (last message length: " ++ show l ++ ")"
    eOut = f <$> bLastLength <@> eMessage
  return $ MessageOutput eOut
```

Start a `Behavior` with the value `x`, then apply the function in the event `eF` every time that it fires.

```haskell
accumB :: MonadMoment m => a -> Event (a -> a) -> m (Behavior a) 
accumB x eF = ...
```

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  bMessages <- stepper "" eMessage
  let
    bLinesNow = (+ 1) <$> bLines
    bLastLength = length <$> bMessages
    f c l m = m ++ " (line count: " ++ show c ++ ", last message length: " ++ show l ++ ")"
    eOut = f <$> bLinesNow <*> bLastLength <@> eMessage
  return $ MessageOutput eOut
```

```haskell
data MessageOutput = MessageOutput {
    moeWrite :: Event String
  , mobLines :: Behavior Int
  }
```

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  return $ MessageOutput eMessage bLines
```

```haskell
data QuitInput = QuitInput {
    qieQuit :: Event ()
  , qibLines :: Behavior Int
  }
```

```haskell
data QuitOutput = QuitOutput {
    qoeWrite :: Event String
  , qoeQuit  :: Event ()
  }
```

```haskell
handleQuit :: QuitInput -> QuitOutput
handleQuit (QuitInput eQuit bLines) =
  let
    f x = "Bye (" ++ show x ++ " messages sent)"
    eWrite = f <$> bLines <@ eQuit
  in
    QuitOutput eWrite eQuit
```

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  bLines <- accumB 0 ((+ 1) <$ eMessage)
  let
    f m (c, _) = (c + 1, m)
  ePair <- accumE (0, "") (f <$> eMessage)
  let
    g (c, m) = "(" ++ show c ++ ") " ++ m
    eOut = g <$> ePair
  return $ MessageOutput eOut bLines
```

```haskell
handleMessage :: MessageInput -> Moment MessageOutput
handleMessage (MessageInput eMessage) = do
  let
    f m (c, _) = (c + 1, m)
  (ePair, bPair) <- mapAccum (0, "") . fmap (\f x -> (f x, f x)) $ (f <$> eMessage)
  let
    g (c, m) = "(" ++ show c ++ ") " ++ m
    eOut = g <$> ePair
    bLines = fst <$> bPair
  return $ MessageOutput eOut bLines
```

Both of these describe how they're going to change things later on

Explain <@>, <@, whenE etc..

Talk about sample, briefly

## Counting lines

Most of the examples in this post are going to be pushing us towards the chat server functionality that we're after, although they'll be in a lot of small steps.

We can use this to count the number of messages sent by our users:
```haskell
bLines <- accumB 0 ((+ 1) <$ eMessage)
```

If we sample `bLines` with `eMessage`, like this:
```haskell
let 
  eSample = bLines <@ eMessage
```
we will get `0` when the first message event occurs, `1` when the second message event occurs and so on.

The behaviors that we get from `sample` and `accumB` are changed by events, but the change is only observable in the next clock tick / moment / transaction.
If the changes where observable at the same time as the event, we'd get `1` when the first message event occurred.

TODO extend box idea, events come in and go out, behaviors go in and out, change at the boundaries but not inside
- maybe a white connector at external junctions for pure values, black for moment values?
- maybe just treat all box edges as transaction edges?

TODO Print the number of messages when we quit - accumB
TODO Print the number of messages with each Message - accumE will do it
TODO Show how to use mapAccum to get both

## A premium echo application

hassle every x messages, quit after y messages
- values for limits, then behaviour int, then behavior (int -> bool)
- add an upgrade command, use it to switch behaviours of type (int -> bool)

- use switch to change between two different networks
  - take in bLines, output eMessage and eQuit
  - the premium one ignore the output and returns never for both outputs
  
- use switch to change fanout, so that we don't have to check for upgrade once we've done an upgrade
  - although it should probably just change to an "already upgraded" error
  - we'd really like to not count lines anymore after the upgrade

## Prompting for a name

- use phase to filter commands
- want a phase change event as well, so we can use it for "logged in"
  - would be nice to get something like that for entering the name phase as well, so we can use it to trigger the initial prompt
- add ruling out of certain reserved names
- use switch to really cut across to the next phase

- eventually would be nice to have the name behaviour lose the Maybe

## Collecting notifications

- separate notifications and error messages etc...
- add an open event, so we can have log in and log out events

- collect until asked for, then clear
- bounded queue version to limit the amount of memory we might need
  - mention the ability to use files for overflow, or a database, here

- extra phase to grab preference? or command line option, passed in as a behavior?
  - talk about where we're going with this
