---
title: Preparing for a chat server 
published: 2016-09-01 12:00:00+10:00
---

TODO previously

# Chat server functionality

TODO intro

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

## Next up

TODO
