---
title: Home
---

I've been playing around with Functional Reactive Programming (FRP) recently.

Specifically, I've been looking at the style of FRP with events and behaviours.
It started with Conal Elliot and Paul Hudak, there have been many papers since, and it is present in the `reactive-banana`, `FRPNow` and `reflex` packages for Haskell and in the `sodium` library for several other languages.

# The plan

I'm planning to port the chat server from [Parallel and Concurrent Programming in Haskell](http://amzn.to/2aP5Caa) to `reactive-banana` and then to `reflex`.

I'm going to start with a command-line application for a single user, then extend that to a socket based server, and from there go on to both the front and back ends of a web service.

Along the way, I'll be exploring different corners of FRP API design, playing with some ideas around testing, and generally poking at things that I find interesting.
Hopefully you find it interesting as well.

<!-- 
# The posts

- [Events](./posts/events.html)
- [Behaviors](./posts/behaviors.html)

-->

# Links

## Packages

- [`reactive-banana`](https://hackage.haskell.org/package/reactive-banana)
- [`reflex`](https://hackage.haskell.org/package/reflex)
    - [`reflex-dom`](https://hackage.haskell.org/package/reflex-dom)

## Papers

As far as I can tell, Conal Elliot and Paul Hudak kicked all of this off:

- [Functional Reactive Animation by Conal Elliot and Paul Hudak](http://conal.net/papers/icfp97/)
and Conal kept going with it:
- [Push-pull functional reactive programming by Conal Elliot](http://conal.net/papers/push-pull-frp/)

There has also been some pretty great work on avoiding space leaks:

- [Practical Principled FRP by Atze van der Ploeg and Koen Claessen](http://www.cse.chalmers.se/~atze/papers/prprfrp.pdf) (and the associated [FRPNow!](https://hackage.haskell.org/package/frpnow) package)
- [Higher-Order Functional Reactive Programming without Spacetime Leaks by Neelakantan Krishnaswami](http://www.cs.bham.ac.uk/~krishnan/simple-frp.pdf)

I've seen most of these folks - along with the authors of the packages mentioned above - either dropping by Reddit, StackOverflow and several other places to answer questions or comment on FRP matters.
I'd recommend listening to them, since they all seem knowledgeable, friendly and keen to help.

## Books

Stephen Blackheath and Anthony Jones have a terrific book called "Functional Reactive Programming"

[<img border="0" src="//ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1633430103&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=dlaingorg-20" />](https://www.amazon.com/Functional-Reactive-Programming-Blackheath/dp/1633430103/ref=as_li_ss_il?ie=UTF8&qid=1471080550&sr=8-1&keywords=functional+reactive+programming&linkCode=li3&tag=dlaingorg-20&linkId=9daa51241d44e00fa6f08b4ddf428860)
<img src="https://ir-na.amazon-adsystem.com/e/ir?t=dlaingorg-20&l=li3&o=1&a=1633430103" width="1" height="1" border="0" alt="" style="border:none !important; margin:0px !important;" />

It mostly uses Java to explore the `sodium` library, which uses events-and-behaviors FRP. 
It's similar enough to `reactive-banana` that the Haskell version of `sodium` was deprecated in its stead.

It's a really good book.

I read the final early-access of the version before the final version landed, at which point I'd spent several dozen hours playing around with `reactive-banana` to get a feel for FRP.

The progression and pacing of the book is great, and I think it covers everything you need to quickly ramp up with events-and-behaviors FRP.

There are versions of `sodium` for Java, C#, Kotlin and Typescript, with C++ and Scala versions existing but apparently in need of some work.
If you're not working with Haskell, I'd recommend that you stop reading this series and read the book first instead.

Personally, the book gave me a much better understanding for how the `Moment` monad works in FRP - transactions in `sodium` - and the examples really opened my eyes to a different way of thinking about how to structure FRP programs.

I also foud the diagrams are really useful for understanding.
I wanted to use similar diagrams for these posts, but at this point in time they're just far too labour intensive for me to put together.

The exposition, the examples, and the sense of humour are fantastic.
Highly recommended.
