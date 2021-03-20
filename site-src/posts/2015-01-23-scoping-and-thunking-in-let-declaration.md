---
layout: post
title: "Scoping And Thunking In Let Declarations"
date: 2015-01-23 08:30
comments: false
categories:
  - Functional Programming
  - Language Design
---

I haven't stopped fiddling with language development. Actually, I still do it
almost full-time in my free time. My latest prototype,
[Scotch](https://github.com/lmcgrath/scotch-lang), which uses Haskell-like syntax
and semantics, is coming close to a point where I can start developing the main
libraries. Close, but there are a few kinks. Here I'll be covering an issue I'm
having with `let` declarations.

<!--more-->

Specifically, what I'm struggling to get my head around is how to model scoping
of values declared within `let` declarations. (_Read about `let` declarations in
[Learn You A Haskell](http://learnyouahaskell.com/syntax-in-functions#let-it-be)._)

In this code, the child declaration `y` uses child declaration `z`, which is
used by the body of the `let`. Consider for a moment: What if `z` had side
effects?

```haskell: Scotch Let Declaration
f :: s -> (a, s)
f x = let y = something with z
          z = something else
          in do things with y and then z again
```

## How Are Side Effects A Problem In Child Scopes?

Scotch is compiled into JVM bytecode. Value and function declarations are
encoded as static methods returning Java 8 lambdas. The declarations internally
use [thunks](http://stackoverflow.com/questions/2641489/what-is-a-thunk) to
suspend evaluation but also to retain the evaluated result.

```java Scotch "add" Function Encoded As Static Java Method
// What x + y would look like compiled:
//
// This uses the runtime support functions applicable() and callable() to
// automatically generate the relevant thunk types.
public static Applicable<Integer, Applicable<Integer, Integer>> add() {
  return applicable(
    augend -> applicable(
      addend -> callable(
        () -> augend.call() + addend.call()
      )
    )
  );
}
```

```java What A Thunk Looks Like
public abstract class Thunk<A> implements Callable<A> {
  private A value;

  public A call() {
    if (value == null) {
      value = evaluate();
    }
    return value;
  }

  protected abstract A evaluate();
}
```

The reason for using thunks is to support lazy evaluation. Arguments passed into
functions are not evaluated until they are referenced. This also comes with the
benefit that arguments are only ever evaluated once. Because child declarations
form [closures](http://en.wikipedia.org/wiki/Closure_%28computer_programming%29)
over variables in their parent scope, this also ensures local variables are also
only evaluated once within these child declarations.

Top-level declarations, like values and functions, are encoded as static Java
methods. Each time they are referenced, a new thunk is returned. In `let` declarations,
this poses a problem because every time a declaration is referenced, it returns
a new thunk. Say for example we have a child declaration called "username" which
fetches a username from a database. Every time the associated thunk is referenced,
the database is hit:

```java What It Looks Like in Pseudo-Java
static String username() {
return new Thunk() { /* database query */ }
}

static String greet(String msg) {
  log("greeting " + username())
  return msg + " " + username() + "!"
}
```

This becomes a major problem in Scotch because the values with side-effects
look like variables!

```haskell What it looks like in Scotch
greet msg =
    let username = do
          something to get
          this from a database
    in do
      log "greeting " ++ username
      msg ++ " " ++ username ++ "!"
```

## Modeling Let As A Function

I've been mulling around a few crazy ways to model `let` declarations. My favorite
so far is wrapping the `let` body up in a function, passing in all declarations as
arguments:

```java Psuedo-Java `let` Modeled With Values As Arguments
static String username() {
  return new Thunk() { /* database query */ }
}

static String greet(String msg) {
  greet_(msg, username())
}

static String greet_(String msg, Thunk<String> username) {
  log("greeting" + username.get())        // here it evaluates for the first time
  return msg + " " + username.get() + "!" // here it gets the value from the initial evaluation above
}
```

This leverages the existing way of modeling evaluation, but it's kinda ugly. For
the near term, this solution is decent, however it adds the overhead of extra
arguments being passed around. I'm not entirely sure how this impacts runtime
performance in compiled code, though I would like another way of modeling `let`
declarations to compare side-by-side.

## Storing Nested Values As Variables

I really like this method of modeling `let` because it behaves more like how a
`let` looks in source code. Instead of wrapping a `let` within another function,
the declarations are assigned to local variables:

```java Values As Variables In Pseudo-Java
static String username() {
  return new Thunk() { /* database query */ }
}

static String greet(String msg) {
  var username = username()
  log("greeting " + username)
  return msg + " " + username + "!"
}
```

I'm not entirely sure what performance impact of variables in `let` would be
compared to using arguments. Either way, I would be calling the methods associated
with each declaration and storing them somewhere. The only thing saved is an
additional function call, so any gains in performance would be minuscule.

## Why The Indecision?

I'm not sure I'm following the best ways to model `let`. And I also don't have
any immediate way to test either solution short of branching the code and spiking
out the two solutions. The tradeoffs aren't necessarily obvious, and I won't see
which one is better in the long term compared to the one not chosen.

Compilation is hard. Scotch uses a [multi-pass compiler](http://en.wikipedia.org/wiki/Multi-pass_compiler)
with 12 steps currently, so adding a new language feature or changing an existing
one can be very arduous tasks. Ideally, I only want to implement `let` once.

I will be posting on progress and which solution I end up choosing, stay tuned.

## @TODO

* Write-up on lexical scoping in Scotch.
* Details on how Scotch compiles values and functions in Java 8 lambdas.
* How simple, non-recursive closures are implemented.
* Implementation of co-recursive closures.

--------------------------

### Links

* [Scotch Source](https://github.com/lmcgrath/scotch-lang)
* [Multi-pass Compiler](http://en.wikipedia.org/wiki/Multi-pass_compiler)
* [Thunks \(Wikipedia\)](http://en.wikipedia.org/wiki/Thunk)

### Credits

Thanks to my dear friend [Alexander Zagniotov](https://github.com/azagniotov)
who took time out of his busy day to review my multiple drafts.
