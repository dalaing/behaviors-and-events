---
title: A command line chat server
published: 2016-09-01 12:00:00+10:00
---

TODO previously

# Chat server functionality

TODO intro

## Prompting for a name

### The name prompting phase

```haskell
data NameInput = NameInput {
    nieStart :: Event ()
  , nieRead  :: Event String
  , nibNames :: Behavior (S.Set String)
  }
```

```haskell
data NameOutput = NameOutput {
    noeWrite :: Event String
  , noeStop  :: Event ()
  , nobName  :: Behavior String
  }
```

```haskell
data NameError =
    EmptyName
  | MultiWordName String
  | IllegalCharInName String
  | NameInUse String
  deriving (Eq, Ord, Show)
```

```haskell
nameErrorMessage :: NameError -> String
nameErrorMessage EmptyName =
  "Your name cannot be an empty string"
nameErrorMessage (MultiWordName _) =
  "Your name can only be a single word"
nameErrorMessage (IllegalCharInName _) =
  "Your name cannot contain the character '/'"
nameErrorMessage (NameInUse s) =
  "The name " ++ s ++ " is already in use"
```

```haskell
processName :: S.Set String -> String -> Either NameError String
processName names line =
  case words line of
    [] ->
      Left EmptyName
    [n]
      | n `S.member` names ->
        Left $ NameInUse line
      | '/' `elem` n ->
        Left $ IllegalCharInName line
      | otherwise ->
        Right n
    _ ->
      Left $ MultiWordName line
```

TODO introduce split

```haskell
handleName :: MonadMoment m => NameInput -> m NameOutput
handleName (NameInput eStart eRead bNames) = do
  let
    (eNameInvalid, eNameValid) = split $ processName <$> bNames <@> eRead
    eStop = () <$ eNameValid
    eWrite = leftmost [
        "Welcome to the chat server.\nPlease enter your name:" <$ eStart
      , (\e -> nameErrorMessage e ++ "\nPlease enter your name:") <$> eNameInvalid
      ]

  bName <- stepper "" eNameValid

  return $ NameOutput eWrite eStop bName
```
  
### The command processing phase 

- once the name arrives, print <name> has joined
- prefix message output with <name>: <message>
- when quitting, print <name> has quit

- add private and kick components, which checks if the user is in bName
  - error message if not
  - confirmation if it happens
    - print <name> has kicked <other>
    - print <name> to <other>: message
    
- also need to handle receiving a private message or a kick

### Going recursive

```haskell
data PhaseInput = PhaseInput {
    pieStartName :: Event ()
  , pieStartCmd  :: Event ()
  }
```

```haskell
data Phase =
    PreOpen
  | NamePrompting
  | CommandProcessing
  deriving (Eq, Ord, Show)

data PhaseOutput = PhaseOutput {
    pobPhase :: Behavior Phase
  }
```

```haskell
handlePhase :: MonadMoment m => PhaseInput -> m PhaseOutput
handlePhase (PhaseInput eName eCmd) = do
  bPhase <- stepper PreOpen . leftmost $ [
      NamePrompting <$ eName
    , CommandProcessing <$ eCmd
    ]
  return $ PhaseOutput bPhase
```

```haskell
{-# LANGUAGE RecursiveDo #-}
pureNetworkDescription :: InputIO -> Moment OutputIO
pureNetworkDescription (InputIO eOpen eRead) = do

  let
    bNames = pure (S.fromList ["root"])

  rec
    PhaseOutput bPhase <- handlePhase $ PhaseInput eOpen enStop

    let
      eReadName = whenE ((== NamePrompting) <$> bPhase) eRead
    NameOutput enWrite enStop bName <- handleName $ NameInput eOpen eReadName bNames

  let
    eReadCmd  = whenE ((== CommandProcessing) <$> bPhase) eRead
  CmdOutput ecWrite ecQuit <- handleCmd $ CmdInput enStop eReadCmd bNames bName

  let
    eWrite = leftmost [enWrite, ecWrite]

  return $ OutputIO eWrite ecQuit
```

### Switching things up

```haskell
duplicateLater :: Event a -> MomentIO (Event a)
duplicateLater e = do
  (eLater, f) <- newEvent
  reactimate $ f <$> e
  return eLater
```

```haskell
pureNetworkDescription2 :: InputIO -> MomentIO OutputIO
pureNetworkDescription2 (InputIO eOpen eRead) = do

  let
    bNames = pure (S.fromList ["root"])

  -- misses the initial open events if we
  -- - use the open events for the component and the switch
  --   don't use a delay

  eOpenName <- duplicateLater eOpen
  NameOutput enWrite enStop bName <- handleName $ NameInput eOpenName eRead bNames

  eOpenCmd <- duplicateLater enStop
  CmdOutput ecWrite ecQuit <- handleCmd $ CmdInput eOpenCmd eRead bNames bName

  let
    emptyOut = OutputIO never never
    nameOut = OutputIO enWrite never
    cmdOut = OutputIO ecWrite ecQuit

  switch emptyOut . leftmost $ [
      nameOut <$ eOpen
    , cmdOut <$ enStop
    ]
```

## Collecting notifications

When we finally make the jump to a proper chat server, we're going to need to take a more fine grained view of our outputs.

Some of the output should only be shown to the user: the helper output, error messages for various commands, and things like that.

Other parts of the output should be broadcast to everyone.

We should also be responding to the data broadcast from others.

### The shape of things

### Online notifications

### Batch mode notifications

### Chosing the mode at login

- separate notifications and error messages etc...
- add an open event, so we can have log in and log out events

- collect until asked for, then clear
- bounded queue version to limit the amount of memory we might need
  - mention the ability to use files for overflow, or a database, here

- extra phase to grab preference? or command line option, passed in as a behavior?
  - talk about where we're going with this

## Next up

TODO
