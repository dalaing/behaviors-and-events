---
title: Filtering and switching
published: 2016-09-01 12:00:00+10:00
---

[Previously](./commands.html) we went through the two main components that will take care of interacting with individual users of our chat server.

# Filtering and switching 

We have two components at the moment.

One for gather the users name, via this interface:
```haskell
data NameInput = NameInput {
    nieOpen     :: Event ()
  , nieRead     :: Event String
  , nibGreeting :: Behavior String
  , nibNames    :: Behavior (S.Set User)
  }

data NameOutput = NameOutput {
    noeWrite :: Event String
  , noeName  :: Event User
  }
```
and one for processing commands, using this interface:
```haskell
data CommandInput = CommandInput {
    cieOpen   :: Event ()
  , cieRead   :: Event String
  , cieNotify :: Event Notification
  , cibNames  :: Behavior (S.Set User)
  , cibName   :: Behavior User
  }

data CommandOutput = CommandOutput {
    coeWrite  :: Event String
  , coeClose  :: Event ()
  , coeNotify :: Event Notification
  , coeKick   :: Event User
  }
```

What we need to do is to build an event network such that once a valid name has been entered, we stop processing potential names and start processing commands.
We also need to be able to use the `Event User` output of the name processing component to set up a `Behavior User` as input to the command processing component.

## Filtering the inputs

## Switching the outputs

## Switching the components

## Switching back and forth 

TODO talk about a hypothetical /nick command that would bring you back to the name phase

## Next up

TODO GC interlude

