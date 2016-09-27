---
title: Components for a chat server
published: 2016-10-01 12:00:00+10:00
---

[Previously](./behaviors.html) we got used to the basics of behaviors.

# Components for a chat server

We're going to start writing bits and pieces that we'll be able to reuse with out chat server.
Since we're only interacting with one user from the command line, we'll be focusing on the pieces that we'll be using for each of the client connections, with the console standing in for actual sockets.

There are three components that we'll be creating here:

- a component to handle delivering notifications to the user
- a component to prompt a user for a valid and unique nickname
- a component to handle command processing, which will be an extended form of what we already have

## Notifications

We are making a distinction between two types of output to the user.

A notification is any chat server event that needs to be communicated to let people know what is going on - these include notifications for when people join or quit, when they send messages, and so on.

The other kind of output is for communication directly with the client.
This includes displaying help and error messages, and will also include the display of any notifications we want to present to the user.

We are making this distinction because we have two ways of dealing with notifications.

The first - and simplest - is to display them to the user as they come in.
The second is to collect the notifications (up to some bounds) until the user requests them, at which time we display the contents of the notification queue and then clear it.

Why would we want both of these options?
We'll see in a little while.

### Data type for notifications

```haskell
data Notification =
    NJoin User
  | NMessage User Message
  | NTell User User Message
  | NKick User User
  | NQuit User
  deriving (Eq, Ord, Show)
```

We need to be able to convert these into `String`s:
```haskell
notificationMessage :: Notification -> String
notificationMessage (NJoin user) =
  user ++ " has joined"
notificationMessage (NMessage user message) =
  "<" ++ user ++ ">: " ++ message
notificationMessage (NTell userFrom userTo message) =
  "*" ++ userFrom ++ "*: " ++ message
notificationMessage (NKick kicker kickee) =
  kicker ++ " has kicked " ++ kickee
notificationMessage (NQuit user) =
  user ++ " has quit"
```

TODO data types for the components

### Streaming notifications

### Batching notifications

## Prompting for a name

The first thing we want to do when a user connects is to display a friendly greeting and ask what nickname they would like to use.
Bonus points if we can change that greeting without having to restart the server.

### Dealing with names

We'd like the following restrictions on the nicknames:

- they can't be empty
- they should be one word
- they should not contain a '/' character, since we're using those for commands
- they should not be the same as the nickname of any of the other users

In the absence of other users, we're going to reserve the usernames "root" and "admin", to prevent (entirely hypothetical) mischief.

If the user enters a name that doesn't meet our restrictions, we should print some feedback and ask them to have another try.

We'll create a data type to describe name-related errors:
```haskell
data NameError =
    EmptyName
  | MultiWordName String
  | IllegalCharInName String
  | NameInUse String
  | NameNotInUse String
  deriving (Eq, Ord, Show)
```

Note that we have a `NameNotInUse` error here, which we'll use later on if someone tries to send a private message to a non-existent user.

We add a function to turn them into strings:
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
nameErrorMessage (NameNotInUse s) =
  "The name " ++ s ++ " is not in use"
```

Then we'll make some functions to check candidate names under various conditions:
```haskell
import qualified Data.Set as S

checkValidName :: String -> Either NameError String
checkValidName name =
  case words name of
    [] ->
      Left EmptyName
    [n]
      | '/' `elem` n ->
        Left $ IllegalCharInName name
      | otherwise ->
        Right n
    _ ->
      Left $ MultiWordName name

checkValidNameNotInUse :: S.Set String -> String -> Either NameError String
checkValidNameNotInUse names line = do
  name <- checkValidName line
  if name `S.member` names
  then Left $ NameInUse line
  else Right name

checkValidNameInUse :: S.Set String -> String -> Either NameError String
checkValidNameInUse names line = do
  name <- checkValidName line
  if name `S.notMember` names
  then Left $ NameNotInUse line
  else Right name
```

Having pure functions for these is nice.

### An event network for name prompting

At this point we're actually nearly done.

We need some data structures for our inputs and outputs.

We have a start event to trigger the initial greeting and a read event for the strings from the user, and we pass in the greeting and set of user names that are currently in use as behaviors.
```haskell
data NameInput = NameInput {
    nieStart    :: Event ()
  , nieRead     :: Event String
  , nibGreeting :: Behavior String
  , nibNames    :: Behavior (S.Set String)
  }
```

For the outputs, we have an event for the text we want to write to the display and an event which is fired when the user selects a valid user name.
```haskell
data NameOutput = NameOutput {
    noeWrite :: Event String
  , noeName  :: Event String
  }
```

We'll write some helper functions to stitch together our output strings:
```haskell
mkGreeting :: String -> String -> String
mkGreeting greeting prompt =
  unlines [greeting, prompt]

mkNameErrorMessage :: String -> NameError -> String
mkNameErrorMessage prompt e =
  unlines [nameErrorMessage e, prompt]
```

Then we pull out the duct tape and build ourselves an event network:
```haskell
handleName :: MonadMoment m => NameInput -> m NameOutput
handleName (NameInput eStart eRead bGreeting bNames) = do
  let
    bPrompt = pure "Please enter your name:"

    eGreeting = mkGreeting <$> bGreeting <*> bPrompt <@ eStart

    (eNameInvalid, eNameValid) = split $ checkValidNameNotInUse <$> bNames <@> eRead

    eNameErrorMessage = mkNameErrorMessage <$> bPrompt <@> eNameInvalid

    eWrite = leftmost [
        eGreeting
      , eNameErrorMessage
      ]

  return $ NameOutput eWrite eNameValid
```

This makes use of `split`, which is handy when you want to fan out an `Either` into two different events:
```haskell
split :: Event (Either a b) -> (Event a, Event b) 
```

This _could_ also be split out into two smaller components - one for handling the initial greeting and prompt, and another for checking the name.

## Processing commands

The commands we are going to implement are modeled after the server in Parallel and Concurrent Programming in Haskell:

- `message...`            will send a message to all users
- `/tell user message...` will send a private message to `user`
- `/kick user`            will kick a user off the server
- `/quit`                 will exit the server

We also have added:

- `/fetch`                will fetch any pending notifications (if relevant)
- `/help`                 will print out a help message

### Notifications and other output

This subtly introduces two types of outputs.

Notifications are visible to some or all users, and include the messages that are zipping around, notices of who is logging in and who is quitting, and other activity like that.

We'll create a data type for these:
```haskell
type User = String
type Message = String

data Notification =
    NJoin User
  | NMessage User Message
  | NTell User User Message
  | NKick User User
  | NQuit User
  deriving (Eq, Ord, Show)
```

We'll be working with a component that has an `Event Notification` as both an input and as an output.
The input carries the notifications coming in from all of the users on the chat server, and the output is for the notifications generated by the user issuing the commands that we are processing.
Since we're only handling a single user at the moment, we're going to end up piping our output back into our input.

The other type of messages are the ones which only the current user should see, which include the help output and the various error message.

### Using names in notifications

Three of our commands need to be tagged with the user name to create a notification.
This can be taken care of using `<@>` and `<@`, which we've covered previously.

These would normally be inlined, but I've presented them in functions here so that you can see what types are in play.

```haskell
notifyOpen :: Behavior User -> Event () -> Event Notification
notifyOpen bName eOpen = 
  NJoin <$> bName <@ eOpen

notifyMessage :: Behavior User -> Event Message -> Event Notification
notifyMessage bName eMessage = 
  NMessage <$> bName <@> eMessage

notifyQuit :: Behavior User -> Event () -> Event Notification
notifyQuit bName eQuit = 
  NQuit <$> bName <@ eQuit
```

So far, so good.

Two of our commands refer to other users.
This means we need to check that those users are valid and currently active, and the commands need to be tagged with both the source and target users in order to create a notification.

We have already introduced the code to check if a name is valid and either in use or not in use, so we just need to package that up:
```haskell
data KickInput = KickInput {
    kibNames :: Behavior (S.Set User)
  , kieName  :: Event User
  }

data KickOutput = KickOutput {
    koeKickValid :: Event User
  , koeError     :: Event String
  }

handleKick :: MonadMoment m => KickInput -> m KickOutput
handleKick (KickInput bNames eName) = do
  let
    (eNameInvalid, eNameValid) = split $ checkValidNameInUse <$> bNames <@> eName
    eError = nameErrorMessage <$> eNameInvalid
  return $ KickOutput eNameValid eError
```

The notification then gets created in the same way as the previous notifications:
```haskell
notifyKick :: Behavior User -> Event User -> Event Notification
notifyKick bName eKick = 
  NKick <$> bName <@> eKick
```
but is only created in the case where the kickee was a valid, logged in user:
```haskell
  KickOutput eKickValid ekError <- handleKick $ KickInput eKick bNames
  let 
    enKick = notifyKick bName eKickValid
```

We treat private messages in the same way.

### Two ways of handling notifications

For various nefarious reasons, we're going to introduce a two ways of consuming the incoming notifications.

The first method will print the notifications as they come in.
The second method will gather the notifications up until a `/fetch` command is used, causing the pending notifications to be displayed and the queue to be reset.

We model the choice as:
```haskell
data NotificationType =
    Stream
  | Batch (Behavior Int)
```
where the `Behavior Int` provides the upper bound on our notification queue length. 

We would like a common interface to our notification components, and so we write one:
```haskell
data NotificationInput = NotificationInput {
    nieFetch        :: Event ()
  , nieNotification :: Event Notification
  }

data NotificationOutput = NotificationOutput {
    noeNotifications :: Event [Notification]
  }
```

The streaming version of handling notifications is very simple:
```haskell
handleNotificationStream :: MonadMoment m => NotificationInput -> m NotificationOutput
handleNotificationStream (NotificationInput _ eNotify) =
  return . NotificationOutput $ pure <$> eNotify
```

The batching method is similar to the example we used when introducing `Behavior`:
```haskell
addToBoundedList :: Int -> a -> [a] -> [a]
addToBoundedList limit x xs =
  take limit (x : xs)

handleNotificationBatch :: MonadMoment m => Behavior Int -> NotificationInput -> m NotificationOutput
handleNotificationBatch bLimit (NotificationInput eFetch eNotify) = do
  bNotifications <- accumB [] . unions $ [
      addToBoundedList <$> bLimit <@> eNotify
    , const [] <$ eFetch
    ]
  return . NotificationOutput $ reverse <$> bNotifications <@ eFetch
```

Once we have both options, we provide a wrapper to help us choose between them.
```haskell
handleNotification :: MonadMoment m => NotificationType -> NotificationInput -> m NotificationOutput
handleNotification Stream =
  handleNotificationStream
handleNotification (Batch limit) =
  handleNotificationBatch limit
```

### An event network for commands

All of the pieces are in place.
Let's start stitching them together.

TODO

## Next up

We've seen two components of the chat server - one for prompting a user for their name, and another for processing the commands that they issue.

Next we'll look at how to link them together so that they are used one after the other.

[Onwards!](./filtering-and-switching.html)
