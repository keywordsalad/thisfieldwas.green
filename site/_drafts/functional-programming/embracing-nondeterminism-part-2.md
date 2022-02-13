---
title: "Embracing Nondeterminism Part II: Contexts in Parallel"
author: Logan McGrath
comments: false
date: 2022-01-24T17:14:03-0800
tags: functional programming, programming, scala, design patterns
layout: post
---

## Motivating Applicatives

Functors permit you to consume a term via `map()`:

:::{.numberLines .nowrap}
```scala
def map(fa: F[A])(f: A => B): F[B]
```
:::

Consider for a moment: with a Functor you are able to work within the scope of a term within a single context. But what happens if you have two contexts and you want to operate on the terms from each at the same time?

Take for example these two contexts and the function signature for `combine()`:

:::{.numberLines .nowrap}
```scala
val fa: F[A]
val fb: F[B]

def combine(x: A, y: B): C
```
:::

How do you apply `combine()` to the terms `A` and `B` produced by the two contexts? This is what motivates Applicatives.

**Applicatives** are an abstraction that allow you to consume terms `A` and `B` in parallel. They are also known by their more formal name _Applicative Functor_ as they are a _special case_ of Functor. Like Functors, they are also a simple structure and define two functions:

1. A constructor to _lift_ a term into the context called `pure()`:

    ```scala
    def pure(a: A): F[A]
    ```

    The name `pure()` might feel alien. You can remember the name by thinking of it like this: By taking an instance of term `A`, `pure()` gives you back a context with a present instance of the term `A`, which makes it a _non void_, _valid_, or _**pure**_ context.

2. A function that is able to apply a _lifted function_ to a _lifted term_ called `ap()`:

    ```scala
    def ap(ff: F[A => B])(fa: F[A]): F[B]
    ```

    The proper name of the function `ap()` is _apply_, but it is written _ap_.

Here is an example of the two functions defined for the `Option[_]` context:

:::{.numberLines .nowrap}
```scala
def pure(a: A): Option[A] = Some(a)

def ap(ff: Option[A => B])(fa: Option[A]): Option[B] = {
  (ff, fa) match {
    case (Some(f), Some(a)) => Some(f(a))
    case _ => None // void
  }
```
:::

Notice that Applicatives also respect a _void effect_ like a Functor does with some specialization: _both_ terms must be present as otherwise there would be no function to apply or no term to apply the function to. Applicative `ap()` thus is an all-or-nothing operation.

Applicatives permit the definition of a higher-order function called `map2()` which may be defined in terms of both `map()` and `ap()`. It is the parallel analog to Functor's serial `map()` function:

:::{.numberLines .nowrap}
```scala
def map2(fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] =
  ap(map(fa)(a => b => f(a, b)))(fb)
```
:::

The function argument to `map2()` is lifted into the context `F[_]` where it may be applied to the terms produced by the two contexts `A` and `B`. If either `A` or `B` are absent, then `f` is not applied and a _void_ `F[C]` is returned.

With this `map2()` function, you are able to apply `combine()` to the terms produced by both contexts:

:::{.numberLines .nowrap}
```scala
// pseudo code
val fa: F[A]
val fb: F[B]

def combine(x: A, y: B): C

map2(fa)(fb)(combine)
// => F[C]
```
:::

## Becoming an Applicative

Like Functor, each context must provide its own implementation of `pure()` and `ap()` in order for it to be used as an Applicative. Any type with the shape of `F[_]` may become an Applicative by implementing the following typeclass:

:::{.numberLines .nowrap}
```scala
trait Applicative[F[_]] extends Functor[F[_]] {
  def pure[A](a: A): F[A]
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  // Applicative instances get map() for free!
  def map(fa: F[A])(f: A => B): F[B] =
    ap(pure(f))(fa)

  // Applicative instances get map2() for free!
  def map2(fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] =
    ap(map(fa)(a => b => f(a, b)))(fb)
}
object Applicative {
  def apply[F[_]: Applicative[F]]: Applicative[F] = implicitly[Applicative[F]]
}
```
:::

Notice that Applicatives build on top of Functors. In the above typeclass, `map()` and `map2()` are provided default implementations in terms of `pure()` and `ap()`. Instances may override these functions if they choose. Our Functor instances from before may now be upgraded to Applicatives:

:::{.numberLines .nowrap}
```scala
object ApplicativeInstances {
  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    def pure[A](a: A): Option[A] = Some(a)
    def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] =
      (ff, fa) match {
        case (Some(f), Some(a)) => Some(f(a))
        case _ => None
      }
  }
  implicit def eitherApplicative[X]: Applicative[Either[X, *]] = new Applicative[Either[X, *]] {
    def pure[A](a: A): Either[X, A] = Right(a)
    def ap[A, B](ff: Either[X, A => B])(fa: Either[X, A]): Either[X, B] =
      (ff, fa) match {
        case (Right(f), Right(a)) => Right(f(a))
        case (x, _) => x
      }
  }
  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    def pure[A](a: A): List[A] = List(a)
    def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
      (ff, fa) match {
        case (f :: ffTail, a :: faTail) => f(a) :: ap(ffTail)(faTail)
        case _ => Nil
      }
  }
}
```
:::

Regarding `ap()` and `map2()`: have you noticed their resemblance to a `zip()` function? This is most apparent in `List[_]`'s implementation of `ap()` as the list stops "zipping" once one of the two lists has run out.

Having this "zipping" characteristic enables _parallel_ consumption of terms, whereas Functors enable _serial_ consumption. This difference is key particularly when working with `Future[_]`'s. Can you guess why?

By enabling parallel consumption of terms, an Applicative implementation of `Future[_]` _executes them in parallel_. This abstraction enables simple concurrency in your programs and you get this benefit with no effort beyond using functions defined against Applicative contexts.

Armed with both serial and parallel means of consuming terms you are able to write programs that more or less take an input and produce some output. These two abstractions allow you to compose effectful operations, but they don't allow you to dictate whether your program should continue executing. Take error handling, for example. Recall how Functors and Applicatives short-circuit? How do you short-circuit them?
