---
title: Redoing My Website With Haskell and Hakyll
author: Logan McGrath
date: 2021-12-03T05:58:58-08:00
published: 2021-12-03T05:58:58-08:00
comments: false
tags: Hakyll, Haskell
layout: post
bodyClass: redoing-my-website
changefreq: daily
---

I found a COVID project: _redoing my website!_

My website prior to 2021 was authored using Octopress or Jekyll. Truthfully I don't remember which as I hadn't touched my website in quite a few years. I needed to update my homepage and I was in a bind because I didn't know how to produce a build after making changes.

<!--more-->

Sitting down, I tried to size up the amount of work I was in for. It looked like Jekyll was no longer maintained, and I had already known for some time that Octopress wasn't maintained either. Additionally, I had hopped a few computers since the last time I actively worked on my site so I did not have the old software handy. This work was starting to look like quite the yak shave and a fair amount of work to get some software running that I would likely not want to have running for very long. I don't mind a yak shave, however, so I decided to choose a new static site generator.

## Wants In a Static Site Generator

* Support for arbitrary static pages written in either Markdown or HTML
* Sass support
* Blog posts in Markdown
* Listing most recent posts
* Listing all posts
* Listing posts by category or tag
* Some mechanism for authoring drafts and viewing them online

Really any static site generator could meet these, but I do like a yak shave. [Hakyll](https://jaspervdj.be/hakyll/) in this regard seemed like a good fit.

## Why Hakyll, Specifically?

Briefly, let me introduce Hakyll as I understand it: Hakyll is a Jekyll-like or -inspired static site generator that provides just enough tools to put together a website. Not to knock it, of course, in reality Hakyll is very lean but provides plenty of building blocks to deliver a no-frills blog.

Naturally the primary value proposition of Hakyll for me was the opportunity to use Haskell for a project that wasn't entirely trivial. A good yak shave, if you will!

## How Hakyll Accomplishes What I Need

### Inputs and Outputs

Hakyll roughly models a website as a series of transformations between inputs and outputs using the `Rules` monad. Rules are initialized by matching file paths by name or patterns as inputs, `Route`ing is specified for the output, and a transformation is applied by means of the `Compiler` monad.

### Turning One Thing Into Another

Compilers are cool, they're where all the magic happens! Through compilers Hakyll is able to take any input format, usually Markdown or HTML, and output usually proper HTML for viewing in a browser. Hakyll out-of-the-box supports multiple input and output formats, and I suspect that in addition to a plain-old technical blog I could also write a really large textbook source in Latex and output a PDF if I really wanted.

My sense is that compilers are Hakyll's primary extension point, as I leaned on these pretty heavily myself and developed opinions as I spent time with them.

Hakyll very smartly tracks dependencies between each compiled artifact and, like a good modern software, only compiles artifacts when it detects changes. For example, this allows for precompiling templates early in the build pipeline and allowing multiple dependants downstream use the same built artifact.

## So What Happened?

So I did in fact redo my website with Haskell and Hakyll!

I kept a small scratch list of things I "knew" would happen but didn't "expect":

* Haskell compiles really slow, like _really_ slow
* Compiled languages are supposed to be more productive but they don't _feel_ that way
* Just using HTML is a tall order, aka. Transformation Telephone
* Templating is inexplicably difficult
* Type inferencing is a real treat until it doesn't work
* CSS is still like the Family Guy meme
* I did not fight functional programming like I expected, I rather found it invaluable

So there's some weird in that list. Let me expand on some more impactful subjects in detail.

### Haskell compiles really slow, like _really_ slow

Hakyll depends on the [pandoc](https://hackage.haskell.org/package/pandoc) library for its document processing. It's very large, and for my particular use case provides way more functionality than my website will ever need. I'm only leveraging code highlighting, HTML, and Markdown processing but pandoc (which can be thought of as a pan-document processing utility) can process pretty much everything under the sun and transform it all this-way and that-. It takes something like 40 minutes to compile clean on my 2019 MacBook Pro.

### Compiled languages are supposed to be more productive but they don't _feel_ that way

With compiled dependencies, my site takes about two minutes to compile clean just the executable. It's at about the upper limit of what I would consider acceptable. Nothing made me feel more slow however than changing a widely dependend-on source file and then waiting two minutes for the binary to rebuild.

I never had to worry about a type error at runtime, but I did miss the quick turnaround I had with my Ruby-based site generator. I can't say whether I was faster or slower in fact, as I wasn't dealing with a whole class of problems related to typos alone, but it sure didn't feel like I was more productive.

### Just using HTML is a tall order, aka. Transformation Telephone

Pandoc loses some data fidelity as it transforms markup between formats. I was trying to use HTML5 semantic tags (`<aside>`, `<figure>`, and friends) as I updated my existing posts and found out that pandoc would process the HTML in the Markdown source and remove the tags but pass the contents through. Digging through pandoc's source code I discovered that there is an intermediate representation for all documents that does not itself have the fidelity necessary to retain whether the contents of those tags _are those tags_ so I didn't feel that I could affect a pull request without a very large amount of work whose value did not appear to serve only my own ends.

### Templating is inexplicably difficult

Hakyll does not have a good template system. I spent some months on and off writing my own system on top of Hakyll compilers to provide the following capabilities that weren't present by default or just weren't very good out of the box:

* Better contexts to retrieve data from compiled items in scope
* Dotted field accessors along contexts
* Custom function declaration
* Easy layouts driven through both metadata and special syntax
* Conditionals
* Loops
* Default values when absent
* Context literals
* List literals
* Syntax literals (i.e. macros and lazy evaluation)
* Falsy and Truthy values, even _undefined_ values!

All of these features gave me a rather roundabout way of satisfying some of the conveniences of a dynamic language within the very stiff context of a Haskell-powered static site generator. Strapping together my own template system was a surprisingly fun, though difficult itch to scratch and a very informative experience worth writing about in another post.

### Type inferencing is a real treat until it doesn't work

Sometimes Haskell couldn't figure out what I was trying to tell it to do. While I didn't read up on any formal documentation on how Haskell's type inferencing algorithm works specifically (it's some flavor of [Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system), interesting stuff) but I did develop some intuition on what was going on and Haskell would complain primarily in two cases, where I was:

1. Failing to return a specifically typed argument from a function
2. Failing to return a specifically typed context from a function

Both of these came down to Haskell not being able to figure out parametrically polymorphic type arguments and this seemed perfectly reasonable to me. I imagine any language would have complained similarly. That said, figuring out the syntaxes to force Haskell to figure out what I mean was like pulling teeth initially and I nearly table-flipped my computer out the living room window at least twice.

Type signatures of this variety usually come in the form of `f :: m a` where _something_ in the calling context has to heavily signal to the compiler what `m a` ought to be, but the calling context is _dependent_ on what `m a` _actually are_ and thus the compiler fails. The trick that I found worked for me was to indicate the type directly via this syntax, as here `(f :: Compiler String)` so that the compiler knows to resolve `f` as `Compiler String` because that's what I mean it to be, and it will fail if the calling location expects differently.

### CSS is still like the Family Guy meme

{{img id: "family-guy-css",
      title: "CSS: it's still like this, but it got prettier",
      src: "/images/redoing-my-website/family-guy-css.gif"}}

I hadn't touched CSS since probably 2013, which I think is about 8 years. It feels the same, with a few upgrades that are in very much the same vein as the old stuff. For as frustrated as I found myself still being with it, I am happy with how the site looks considering that I didn't particularly know what I was doing.

Despite feeling as though I was fumbling about, I was so proud to have figured out how to style my site in such a way that looked great on both desktop and mobile, scaling across multiple resolutions and feeling that I wasn't compromising on aesthetics or functionality. Try looking at this or other pages from desktop or phone, and try especially comparing landscape vs. portrait!

I was utterly thrilled to find that all major browsers' dev tools had greatly matured, had even become on-par with each other, and I was able to view my site from various resolutions and make changes at a dizzying pace. I'm incredibly impressed and very happy with how enabling the tools are.

### I did not fight functional programming like I expected, I rather found it invaluable

I don't get what the negative fuss about functional programming is. Everything I needed help with was googleable, like everything else in tech. I rather enjoyed using Haskell for what was a pretty boring problem solvable by any programming language, and on the whole I'm happy with my decision to use it. Learning functional programming benefits from a palate cleanser in order to fully grok, and I gave it an earnest shot and found some valuable tidbits that I imagine will find their ways back into how I write code elsewhere:

* Laziness is _wonderful_ because I don't have to perform acrobatics to suspend logic for running later. My code is simpler for being able to use it, and my code more closely followed intuition than an alien, prescriptive execution plan.
* Abstract data types are the best way of implementing closed polymorphism that I've found thus far, and enums in object oriented languages are close enough to simulate them effectively in those languages.
* Type classes are the best way of implementing open polymorphism that I've found thus far, and interfaces in object oriented languages are close enough to simulate them effectively in those languages.
* I did not for one moment miss inheritance or class-based polymorphism. They're a rather brittle way of implementing polymorphism. I wager I could even write decent Java without them, and I have actually been doing this for some time already in other languages.
* Immutability makes reasoning about data flow and change of state across an application so, so easy and I will leverage this everywhere I reasonably can.
* Pattern matching and branching conditionally based on the _shape_ of arguments is astoundingly empowering as it elevates data into a first-class position within API design. Until recently, I can't think of any C syntax-inspired language that took advantage of this concept and now that this is changing it most certainly will have an enormous economic impact as this will have an enabling effect in how we write our applications. I wish this was something that could be measured more concretely, but I'm happy even if it's just a convenience to have.
* This is perhaps my most unexpected takeaway: effect types make very clear that _here there be dragons_. Those dragons such as:
  * _Changing the world_ when you launch a rocket with `IO`
  * _Nondeterminism_ when sometimes you don't know how many you get back with `[a]`
  * _Absence_ when sometimes you don't get nothing back with `Maybe`
  * _Wrong_ when sometimes it's not right with `Either`
  * Or any other _"nope"_ modes of execution
