---
title: "The Set Function"
description: Modeling a mathematical set as a function.
author: Logan McGrath
comments: true
date: 2022-06-16T21:29:05-0700
published: 2022-06-16T22:22:26-0700
tags: functional programming, programming, scala, design patterns, combinators
layout: post
---

What is a `Set`? A `Set` can tell you whether or not an `value` is a member of the `Set`. This means that a `Set` is merely a function, specifically of `A => Boolean`. In this post I will explore the usage of **combinators** to build a `Set` from elementary functions alone.

<!--more-->

## Modeling a `Set`

I'm going to use a trait to model a `Set`:

:::{.numberLines}
```scala
trait Set[A] extends (A => Boolean)
```
:::

And define the most elementary of `Set`s, the _empty `Set`_ and _singleton `Set`_:

:::{.numberLines}
```scala
def empty[A](): Set[A] = _ => false

def singleton[A](value: A): Set[A] = test => test == value

val emptyInts = empty[Int]()
emptyInts(1) // false
emptyInts(2) // false

val justOne = singleton(1)
justOne(1) // true
justOne(2) // false
```
:::

But what about more complex sets?

## The `Set` of natural numbers

The `Set` of natural numbers requires more work to model. Specifically, it represents a lower bound of `0`:

:::{.numberLines}
```scala
def nat(value: Int): Boolean = value >= 0
```
:::

But this is very concrete. A lower bound specifically is a type of _predicate_, and this can be parameterized:

:::{.numberLines}
```scala
def satisfy(predicate: A => Boolean): Set[A] = predicate

def nat: Set[Int] = satisfy(_ >= 0)

nat(-1) // false
nat(0) // true
nat(1) // true
```
:::

## Building a `Set`

How do I add values to the `Set`? I have to be able to combine `Set`s:

:::{.numberLines}
```scala
trait Set[A] extends (A => Boolean) {

  def |[B >: A](other: Set[B]): Set[B] =
    value => this(value) || other(value)
}
```
:::

By combining `Set`s, I can now specify that a `value` be one of two `value`s in a `Set`:

:::{.numberLines}
```scala
val oneOrTwo = singleton(1) | singleton(2)

oneOrTwo(1) // true
oneOrTwo(2) // true
oneOrTwo(3) // false
```
:::

But this `|` operator doesn't allow me to set boundaries using predicates, as it represents a _disjoint `Set`_. This means `value`s must be present at least in one `Set` or the other:

:::{.numberLines}
```scala
val singleDigits = satisfy(_ >= 0) | satisfy(_ <= 9)

singleDigits(10) // true
singleDigits(-1) // true
```
:::

I need another operator requiring membership in both `Set`s, so that I can create a _joint `Set`_:

:::{.numberLines}
```scala
trait Set[A] extends (A => Boolean) {

  def |[B >: A](other: Set[B]): Set[B] =
    value => this(value) || other(value)

  def &[B >: A](other: Set[B]): Set[B] =
    value => this(value) && other(value)
}
```
:::

The `&` operator now requires that the `value` exists in _both_ `Set`s:

:::{.numberLines}
```scala
val singleDigits = satisfy(_ >= 0) & satisfy(_ <= 9)

singleDigits(2) // true
singleDigits(9) // true
singleDigits(10) // false
singleDigits(-1) // false
```
:::

### Building a complex `Set`

I'm going to build a weird `Set` of numbers from `0-100`, `102`, `104`, `220-230`, and all numbers greater than `300` that are divisible by `17`:

:::{.numberLines}
```scala
val complexSet = (satisfy(_ >= 0) & satisfy(_ <= 100)) |
  singleton(102) |
  singleton(104) |
  (satisfy(_ >= 220) | satisfy(_ <= 230)) |
  (satisfy(_ >= 300) & satisfy(_ % 17 == 0))

complexSet(60) // true
complexSet(104) // true
complexSet(180) // false
complexSet(227) // true
complexSet(340) // true
complexSet(341) // false
```
:::

## Takeaway

Using functions alone, complex logic can be composed from atomic, testable building blocks. These functions that take other functions as arguments to produce new functions are referred to as **combinators** as they _combine_ their capabilities.

> Composing programs from small units ensures testability and ease in refactoring, as the single-responsibility principle is taken to its logical extreme with this technique.

### Why don't we use `Set`s as functions?

This seems like a cool way to model `Set`s, but that's about where the utility stops. As an exercise, it's fun to see what you can do to model `Set`s using functions alone, as this is an excellent vehicle for learning functional composition with combinators. In practice this would produce a `Set` with linear-time performance, and you don't want that.
