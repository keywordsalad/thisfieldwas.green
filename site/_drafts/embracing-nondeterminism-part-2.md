---
title: "Embracing Nondeterminism Part II: Products of Contexts"
description: Abstracting nondeterminism and complexity in contexts in order to consume products of two or more in parallel.
author: Logan McGrath
comments: false
date: 2022-02-23T20:58:26-0800
tags: functional programming, programming, scala, design patterns
layout: post
twitter:
  image: /images/tags/functional-programming/functional-grass-512x512.png
og:
  image:
    url: /images/tags/functional-programming/functional-grass-512x512.png
    alt: Abstracting nondeterminism and complexity in contexts in order to consume products of two or more in parallel.
---

In my last post {{ linkedTitle "_drafts/embracing-nondeterminism-part-1.md" }} we introduced **functors** as a design pattern used to abstract over the **effects** of nondeterminism and complexity inherent in programs. We modeled nondeterminism and complexity as **contexts** representing sets of effects. Using the functor's `map()` function, you are able to consume the terms produced by individual contexts. But what if you require two or more terms from as many contexts? 

<!--more-->

## Motivating applicatives as a design pattern

Recall the `Functor` typeclass:

:::{.numberLines .nowrap}
```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B] 
}
object Functor {
  def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
}
```
:::

For any context `F[A]`, the `map()` function accepts another function `f: A => B` and applies it to the term within the context, giving back `F[B]`. 

This abstraction is very elementary as it only permits function application against terms produced by a single context. Specifically, you can't use `map()` to apply the following function to the two contexts:

:::{.numberLines .nowrap}
```scala
val fa: F[A]
val fb: F[B]

def combine(a: A, b: B): C
```
:::

Let's try to apply the function and see why:

:::{.numberLines .nowrap}
```scala
// apply to the first context
val ff: F[B => C] = Functor[F].map(fa)(combine.curried)

// apply to the second context
val fc: F[F[C]] = Functor[F].map(ff)(f => Functor[F].map(f))
```
:::

_The context became nested within itself: `F[F[C]]`._ 

How do you apply a function to these two contexts? You use a _special case_ of the functor known as an **applicative** functor.

### What is an applicative?

Like the functor, an applicative is a simple structure. It provides a `map()` function, as it is a special case of functor, and defines two more:

1. `pure()` is a constructor which _lifts_ an instance into the context as `A => F[A]`. The name _pure_ may feel alien, but you can remember it because it gives you back a _valid_, _present_, or _**pure**_ context.
2. `ap()`, read as _apply_, which takes a lifted function and applies a lifted argument to it as `F[A => B] => F[A] => F[B]`.

Applicatives in Scala may be defined using the following typeclass:

:::{.numberLines .nowrap}
```
trait Applicative[F[_]] extends Functor[F] {
  def pure(a: A): F[A]
  def ap(ff: F[A => B])(fa: F[A]): F[B]
}
object Applicative {
  def apply[F[_]: Applicative]: Applicative[F] = implicit[Applicative[F]]
}
```
:::

These two functions, _pure()_ and _ap()_, don't appear to work on their own to apply two contexts to a function. This is because they are elementary abstractions from which a number of operations are derived. The function you want is called `map2()` and it is the applicative's two-argument analog of the functor's single-argument `map()` function.

:::{.numberLines .nowrap}
```scala
def map2[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] =
  ap(ap(pure(a => b => f(a, b)), fa), fb)

// and then from before:
val fa: F[A]
val fb: F[B]
def combine(a: A, b: B): C

map2(fa)(fb)(combine)
```
:::

Take a look at the body of the `map2()` function. Do you see some repitition? With some refactoring you can define a default implementation of `map()` by extracting the inner portion of the body:

:::{.numberLines .nowrap}
```scala
def map[A, B](fa: F[A])(f: A => B): F[B] =
  ap(pure(f), fa)

def map2[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] =
  ap(map(fa)(a => b => f(a, b)), fb)
```
:::

Just with the `pure()` and `ap()` functions, you get `map()` for free and an easy `map2()` function to boot!

> The `map2()` function can also be defined by _currying_ `f`:
  
  ```scala
  def map2[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] =
    ap(map(fa)(f.curried), fb)
  ```
 