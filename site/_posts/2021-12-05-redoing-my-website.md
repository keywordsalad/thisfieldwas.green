---
title: Redoing my website with Haskell and Hakyll
author: Logan McGrath
date: 2021-12-05T08:08:32-08:00
published: 2021-12-05T08:08:32-08:00
comments: false
tags: hakyll, Haskell
layout: post
bodyClass: redoing-my-website
changefreq: daily
---

I'm picking up a COVID project: _Redoing my website!_

My website prior to 2021 was authored using [`octopress`](http://octopress.org/) or [`jekyll`](https://jekyllrb.com/). Truthfully I don't remember which as I haven't touched my website in quite a few years. I want to update my homepage and I'm in a bind because I don't know how to produce a build after I make changes.

Sitting down, I try to size up the amount of housekeeping present here. I can't quite tell what's going on with `jekyll`, and I know that `octopress` [hasn't been updated](http://octopress.org/2015/01/15/octopress-3.0-is-coming/) for a while. Additionally, I'm several computers removed in time since I last actively worked on my site, so I don't have the old software handy. This work is starting to look like quite the yak shave to get some software running that I would soon have to update. I don't mind a _good_ yak shave, however, so I'm going to find a new static site generator.

<!--more-->

## Wants in a static site generator

* Support for arbitrary static pages written in either `markdown` or `html`.
* `sass` support.
* Blog posts in `markdown`.
* Listing most recent posts.
* Listing all posts.
* Listing posts by category or tag.
* Some mechanism for authoring drafts and viewing them online.

Really any static site generator can meet these, but I do _enjoy_ a good yak shave. [hakyll](https://jaspervdj.be/hakyll/) in this regard seems like a good fit.

## Why `hakyll`, specifically?

Briefly, let me introduce `hakyll` as I understand it: `hakyll` is a `jekyll`-inspired static site generator that provides just enough tools to put together a website. Not to knock it, of course, in reality `hakyll` is very lean but provides plenty of building blocks to deliver a no-frills blog.

Naturally the primary value proposition of `hakyll` for me is the opportunity to use Haskell for a project that isn't entirely trivial. An _enjoyable_ and _good_ yak shave, if you will!

## How `hakyll` accomplishes what I need

### Inputs and outputs

`hakyll` roughly models a website as a series of transformations between inputs and outputs using the `Rules` monad.

1. `Rules` are initialized by matching file paths by name or pattern as inputs.
2. Per input, a `Route` indicates an output destination.
3. When routed, a transformation is applied by means of the `Compiler` monad.

**Rules for copying images into my site**

```{.haskell .numberLines}
images :: Rules ()
images =
  match "images/**" do        -- 1. match all files in the images folder
    route idRoute             -- 2. output image with the file name as-is
    compile copyFileCompiler  -- 3. copy image file from source to output as-is
```

### Turning one thing into another

Compilers are cool, they're where all the magic happens! Through compilers I use `hakyll`'s magic to transform `markdown` and `html` into `html` for viewing in a browser. `hakyll` out-of-the-box supports multiple input and output formats, and I suspect that in addition to a plain-old technical blog I can also write a really large textbook source in `LaTeX` and output a `pdf`.

I sense that compilers are `hakyll`'s primary extension point, as I lean on these heavily and develop opinions as I spend time with them.

`hakyll` very smartly tracks dependencies between each compiled artifact and, like a good modern software, only compiles artifacts when it detects changes. For example, this allows for precompiling templates early in the build pipeline and allowing multiple dependents downstream use the same built artifact.

## So what happened?

My website now uses for its static site generator: Haskell and `hakyll`!

I kept a small scratch list of things I _knew_ would happen but didn't _expect_:

* Haskell compiles really slow, like _really_ slow.
* Compiled languages are supposed to be more productive but they don't _feel_ that way.
* Just using plain-old `html` is a tall order, aka. _Transformation Telephone_.
* Templating is inexplicably difficult.
* Type inferencing is a real treat until it doesn't work.
* Using `css` is still like the Family Guy meme.
* I did not fight functional programming like I expected, I rather experienced it as an invaluable teacher.

Let me expand on these items.

### Haskell compiles really slow, like _really_ slow

`hakyll` depends on the [`pandoc`](https://hackage.haskell.org/package/pandoc) library for its document processing. It's very large, and for my particular use case provides much more functionality than my website will ever need. I'm leveraging code highlighting, `html`, and `markdown` processing but `pandoc` (which can be thought of as a _pan-document swiss army knife markup processing utility_ of sorts) can process pretty much everything under the sun and transform it all this-way and that-. From source it takes around 40 minutes to clean-compile on my 2019 MacBook Pro.

### Compiled languages are supposed to be more productive but they don't _feel_ that way

With compiled dependencies, my site takes about two minutes to clean-compile just the executable. That's at about the upper limit of what I would consider acceptable. Changing a widely depended-on source file and then waiting two minutes for the binary to rebuild is a real grind.

I never have to worry about a type error at runtime, but I do miss the quick turnaround I had with my Ruby-based site generator. I can't say whether I was faster or slower in fact, as I wasn't dealing with a whole class of problems related to typos alone, but it sure doesn't feel like I'm more productive.

_UPDATE:_ Since working more in content vs. code, `hakyll` has proven itself very snappy and resilient to errors. I hardly have to think about it as I make edits and experiment with cosmetic touches. I'm still on the fence about productivity in code, but I'm not unhappy.

### Just using plain-old `html` is a tall order, aka. _Transformation Telephone_

`pandoc` loses some data fidelity as it transforms markup between formats. I try to use `html5` semantic tags (`<aside>`, `<figure>`, and friends) as I update my existing posts and find out that `pandoc` processes the `html` in the `markdown` source and removes the tags but passes the contents through mostly unchanged. Digging through `pandoc`'s source code I discover that there is an [intermediate representation](https://hackage.haskell.org/package/pandoc-types-1.22.1/docs/Text-Pandoc-Definition.html#t:Block) for all documents that does not itself have the fidelity necessary to retain whether the contents of those tags represent their intended semantics. I don't feel that I can affect a pull request to add this capability without a very large amount of work whose value serves more than my own ends.

### Templating is inexplicably difficult

`hakyll` does not have a good template language. I spend a few months on and off as I can squeeze time out after work so that I can write my own template language. This language provides for me the following capabilities that aren't present by default or just aren't very good out of the box:

* Better contexts to retrieve data from compiled items in scope.
* Dotted field accessors along contexts.
* Custom template function declarations.
* Easy layouts driven through both metadata and special syntax.
* Conditionals.
* Loops.
* Default values when absent.
* Context literals.
* List literals.
* Syntax literals (i.e. macros and lazy evaluation).
* Falsy and Truthy values, even _undefined_ values!

All of these features give me a rather roundabout way of satisfying some of the conveniences of a dynamic language within the very stiff context of a Haskell-powered static site generator. Strapping together my own template language is a surprisingly fun, though difficult itch to scratch, and a very informative experience worth writing about in another post.

### Type inferencing is a real treat until it doesn't work

Sometimes Haskell can't figure out what I'm trying to tell it to do. I didn't read any formal documentation on how Haskell's type inferencing algorithm works specifically (the system derives from [Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system), interesting stuff) but I did develop some intuition over time. Unfortunately words escape me and I can't seem to explain how it works just yet.

Occasionally I have to tell Haskell what to do, and there is a syntax for this:

```{.haskell .numberLines}
-- Haskell doesn't know that I mean "page" is a `String` and not `[a]`
constField "layout" ("page" :: String)
```

That particular instance above is due to a [footgun](http://dev.stephendiehl.com/hask/#overlappinginstances) I use so that lists of type `[a]` are treated generally and `Strings`, which are just `[Char]`, are treated specially. Haskell can't tell the difference in this case unless I provide the extra type information.

### Using `css` is still like the Family Guy meme

{{img id: "family-guy-css",
      title: "`css`: it's still like this, but it got prettier",
      src: "/images/redoing-my-website/family-guy-css.gif"}}

I probably haven't touched `css` since 2013, which right now is about 8 years ago. It feels the same, with a few upgrades that are in very much the same vein as the old stuff. It's still weird and clunky, at least to me. For as frustrated as I find myself with it, I am happy with how the site looks considering that I don't particularly know what I'm doing.

Despite feeling as though I'm fumbling about, I'm so proud to have figured out how to style my site so that looks great on both desktop and mobile, that it scales across multiple resolutions, but that it especially feels like I did not compromise on aesthetics or functionality. Try looking at this or other pages from desktop or phone, and try especially comparing landscape vs. portrait!

I am thrilled that all major browsers' dev tools have greatly matured, have even become on-par with each other. I'm very happy with how enabling these tools are now.

### I did not fight functional programming like I expected, I rather experienced it as an invaluable teacher

I don't get what the negative fuss about functional programming is. Everything I've needed help with is googleable, like everything else in tech. I rather enjoy using Haskell for what is a pretty boring problem solvable by any programming language, and on the whole I'm happy so far with my decision to use it. Learning functional programming benefits from a palate cleanser in order to fully grok, and I'm giving it an earnest shot. I've also picked up some tidbits that I imagine will find their way back into how I write code elsewhere.

#### Abstract data types

Abstract data types are the best way of implementing closed polymorphism that I've found thus far, and enums in object oriented languages are close enough to simulate them effectively in those languages.

In my experience, polymorphism dealing with data only needs to extend over a small number of cases. By using abstract data types, I can keep my code variants local to each other so that differences can be viewed in context. Adding new variants requires that changes be made in-context, ensuring that no case for a polymorphic change in behavior is unchecked. I lean on the compiler to tell me if I left any location unhandled.

#### Type classes

Type classes are the best way of implementing open polymorphism that I've found thus far, and interfaces in object oriented languages are close enough to simulate them effectively in those languages.

Contrasting with abstract data types, interfaces provide a facility for extending polymorphism over an open set of cases. As interfaces speak only in terms of operations, they run a much lower risk of introducing variant code paths that could introduce defects related to data.

#### Object-oriented programming in the Kingdom of Nouns

I did not for one moment miss inheritance or class-based polymorphism. They're a brittle way of implementing polymorphism, generally. I wager I could write decent Java without them, and I have been doing this for some time already in other languages.

#### Changing the world

Immutability makes reasoning about data flow and change of state across an application really easy and I will leverage this everywhere I reasonably can.

#### Pattern matching

Pattern matching and branching conditionally based on the _shape_ of arguments is astoundingly empowering. It elevates data into a first-class position within API design. Until recently, I can't think of any C syntax-inspired language that takes advantage of this concept. Pattern matching may be so enabling, I think, that it might even have a measurable impact on the economy! I wish this was something that could be measured more concretely, but I'm happy even if it's just a convenience to have.

#### Dial "M" for Monad

This is perhaps my most unexpected takeaway: Effect types make very clear that _here there be dragons_. Those dragons such as:

* _Changing the world_ when I launch a rocket with `IO`
* _Nondeterminism_ when I don't know how many things I'm going to get back with `[a]`
* _Absence_ when sometimes I `Just` get `Nothing` with `Maybe`
* _Wrong_ when sometimes it's not `Right` and all I'm `Left` with is `Either`
* Or any other modes of execution where the outcome has an orthogonal quality in addition to the one I wanted.

## I rather like `hakyll` and Haskell!

I'm really happy with `hakyll`! I'm also really happy with Haskell. My website is taking some investment, but it's at a point where I'm really happy with it. I think I'm going to keep it this way for a while yet.
