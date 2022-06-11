---
title: "Embracing Nondeterminism Part III: Imperative Computation"
description: Leveraging the case of the previous effect to indicate whether computation of the next effect should proceed.
author: Logan McGrath
comments: true
date: 2022-05-30T10:04:42-0700
tags: functional programming, programming, scala, design patterns
layout: post
twitter:
  image: /images/tags/functional-programming/functional-grass-512x512.png
og:
  image:
    url: /images/tags/functional-programming/functional-grass-512x512.png
    alt:  Leveraging the case of the previous effect to indicate whether computation of the next effect should proceed.
code_repo: https://bitsof.thisfieldwas.green/keywordsalad/embracing-nondeterminism-code/src/branch/part3
---

Remember **functors** and **applicatives**? In my last post {{linkedTitle "_posts/2022-06-05-permitting-or-halting-computation.md"}} we explored how functors and applicatives abstract over **desired** and **undesired cases** of **contexts** in order to express control flow and permit independent computation. In this post we will explore **monads** and how to leverage their specific abstraction to express **imperative** control flow.

<!--more-->

> **This post is part of a series:**
>
> 1. {{linkedTitle "_posts/2022-03-15-contexts-and-effects.md"}}
> 2. {{linkedTitle "_posts/2022-06-05-permitting-or-halting-computation.md"}}
> 3. {{title}}

_The code that accompanies this post may be found [here]({{code_repo}})._

## Motivating monads as a design pattern

Recall the `Functor` and `Applicative` typeclasses:

:::{.numberLines}
```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
object Functor {
  def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
}

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  def map[A, B](fa: F[A]): F[B] =
    ap(pure(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    ap(ap(pure(f.curried))(fa))(fb)

  def sequence[A](listFa: List[F[A]]): F[List[A]] =
    listFa.foldLeft(pure(List[A]()))((fListA, fa) => map2(fa, fListA)(_ :: _))
}
object Applicative {
  def apply[F: Applicative]: Applicative[F] = implicitly[Applicative[F]]
}
```
:::

> See [`Functor`]({{code_repo}}/src/main/scala/green/thisfieldwas/embracingnondeterminism/typeclasses/Functor.scala)
> and [`Applicative`'s]({{code_repo}}/src/main/scala/green/thisfieldwas/embracingnondeterminism/typeclasses/Applicative.scala)
> definitions in the sample repository.

`Functor` abstracts over contexts' unknown cases by lifting a function via `map()` and applying it to instances of the term produced by the context if they exist. Control flow can't be expressed using `map()` because it does not permit the case of the context to be modified.

`Applicative` can create contexts in the desired case via `pure()`. It can also apply lifted functions to lifted arguments via `ap()` if both contexts are in their desired case. `Applicative` is capable of expessing control flow through `ap()` because supplying either the lifted function or lifted argument in an undesired case will not permit further computation. The context in the undesired case is propagated instead.

The problem however is that `Applicative` requires that the lifted function and lifted argument supplied to `ap()` be computed before deciding whether to permit or halt further computation. This means that regardless of whether one of the function or argument successfully computes, if the other fails the succeeding computation will be discarded. Both of these contexts are independent, and `Applicative` will only use them if they both are in their **desired case**.

All functions derived from `ap()` represent _all-or-nothing_ operations accordingly, and there is no effective way of ordering the computation of the arguments or preventing computation of arguments if any dependent arguments fail to compute. Therefore, totally imperative programming is not possible using `Applicative`.

## Introducing imperative control flow

In order to restrict computation such that subsequent computation occurs only if previous computation succeeds, we must introduce a new abstraction over the cases of contexts.

What function might limit subsequent computations from running if a previous one has failed? There is one, and you've probably seen it out in the wild:

:::{.numberLines}
```scala
def flatMap[F[_], A, B](fa: F[A])(f: A => F[B]): F[B]
```
:::

What `flatMap()` does is allow for the injection of a context into a pipeline of computations to either permit computation to proceed or force it to halt. The function argument `f`, which you supply, has full control of what case the returned context `F[B]` should be in. If `F[B]` is in the **undesired case**, then all further computations are skipped.

Take for example this definition of a `filter()` function from the sample repository's `sbt console`:

:::{.numberLines}
```scala
scala> :load console/imports.scala

scala> def filter[A](fa: Option[A])(f: A => Boolean) = fa.flatMap {
     |   case a if f(a) => Some(a)
     |   case _ => None
     | }
def filter[A](fa: Option[A])(f: A => Boolean): Option[A]

scala> filter(Some(4))(_ % 2 == 0)
val res1: Option[Int] = Some(4)

scala> filter(Some(3))(_ % 2 == 0)
val res2: Option[Int] = None
```
:::

This example is only very simple. But it demonstrates the key enabling feature of `flatMap()`: we are able to choose whether the context remains in its desired case. If we continue with this example, we can try chaining `map()` to the context returned by it.

:::{.numberLines}
```scala
scala> filter(Some(4))(_ % 2 == 0).map(n => s"The square of even number $n is ${n * n}")
val res3: Option[String] = Some(The square of even number 4 is 16)

scala> filter(Some(3))(_ % 2 == 0).map(n => s"The square of even number $n is ${n * n}")
val res4: Option[String] = None
```
:::

Thus we are able to gate `map()` behind the case of the context we return from the `flatMap()` we used in `filter()`, enabling imperative control flow.

## Monads

The `flatMap()` function is implemented using a new structure, a specialization of an applicative functor called a **monad**.

Monads are a specialization that arise when the type of `A` contained within an applicative `F[_]`. If `A` is an opaque type, then `F[_]` is a functor and no more; if `A` is known to have type `A => B`, that is _`A` is a function_, then `F[_]` is an applicative functor. But if `A` is known to have type `F[A]`, then this means that `F[_]` is _nested within itself_ and thus a monad.

The `Monad` typeclass defines two functions, which by default are defined in terms of each other:

:::{.numberLines}
```scala
trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    flatten(map(fa)(f))

  def flatten[A](ffa: F[F[A]]): F[A] =
    flatMap(fa => fa)
}

object Monad {

  def apply[F[_]: Monad]: Monad[F] = implicitly[Monad[F]]
}
```
:::

This means that in order to implement an instance of the `Monad` typeclass, you must implement one of either `flatMap()` or `flatten()` or calling one will recurse until the stack overflows. Some contexts are best defined using one over the other, and you have the choice to pick between the two.

> [See here]({{code_repo}}/src/main/scala/green/thisfieldwas/embracingnondeterminism/typeclasses/Monad.scala) for the definition in the sample repository.

### Composing monads

`Monad` also defines a special composition operator. Recall that functions may be composed together, as in `h = g ∘ f` or _"h is g after f"_. This may be expressed a few ways in vanilla Scala. Try below using the sample repository's `sbt console`:

:::{.numberLines}
```scala
scala> val f: Int => String = n => (n * n).toString
val f: Int => String = $Lambda$5252/0x000000010195e040@7a76dc63

scala> val g: String => Int = n => n.length
val g: String => Int = $Lambda$5254/0x000000010195f840@3843085

scala> val h = g.compose(f)
val h: Int => Int = scala.Function1$$Lambda$5244/0x0000000101958840@388b1b42

scala> h(3)
val res1: Int = 1

scala> h(25)
val res2: Int = 3

scala> val hh = f.andThen(g)

scala> hh(3)
val res3: Int = 1

scala> hh(25)
val res4: Int = 3
```
:::

Scala out of the box affords the `compose()` and `andThen()` functions for composing functions of the form of `A => B` together. But these composition functions don't work for the signature of `flatMap()`, which returns contexts in its signature of the form of `A => F[B]`.

There is a form of composition for monads known as [_Kleisli composition_](https://blog.ploeh.dk/2022/04/04/kleisli-composition/). This composition will lift following functions into the context returned by previous functions and apply them to the term contained within the context if the context is in the desired case. Kleisli composition is expressed using the _fish_ or _arrow operator_ `>=>` which is defined in the sample repository as an extension operator on functions of the form `A => F[B]` where `F[_]` has an instance of `Monad`:

:::{.numberLines}
```scala
implicit class KleisliCompositionOps[F[_], A, B](val f: A => F[B]) extends AnyVal {

  def >=>[C](g: B => F[C])(implicit F: Monad[F]): A => F[C] = f(_).flatMap(g)
}
```
:::

> [See here]({{code_repo}}/src/main/scala/green/thisfieldwas/embracingnondeterminism/typeclasses/Monad.scala#L99) for the definition in the sample repository.

Kleisli composition is useful for monads in that from two or more `flatMap()`-compatible functions, a single function may be created that takes an unlifted argument and produces an output context that has been applied to each function in sequence.

:::{.numberLines}
```scala
scala> :load console/imports.scala

scala> val squareEvens: Int => Either[String, Int] = {
     |   case n if n % 2 == 0 => Either.right[String, Int](n * n)
     |   case n               => Either.left[String, Int](s"$n is an odd number")
     | }
val squareEvens: Int => Either[String,Int] = $Lambda$5278

scala> val piEvenLastDigit: Int => Either[String, Int] = { n =>
     |   val nPi = n * scala.math.Pi
     |   val lastDigit = nPi.toString.last.asDigit
     |   if (lastDigit % 2 == 0) {
     |     Either.right[String, Int](lastDigit)
     |   } else {
     |     Either.left[String, Int](s"$n * Pi's last digit is an odd number")
     |   }
     | }

scala> val evensSquaredWithEvenLastDigitsOfPi = squareEvens >=> piEvenLastDigit
val evensSquaredWithEvenLastDigitsOfPi: Int => Either[String,Int] = Monad$Syntax$KleisliCompositionOps$$$Lambda$5241

scala> evensSquaredWithEvenLastDigitsOfPi(2)
val res1: Either[String,Int] = Right(2)

scala> evensSquaredWithEvenLastDigitsOfPi(23)
val res2: Either[String,Int] = Left(23 is an odd number)

scala> evensSquaredWithEvenLastDigitsOfPi(48)
val res3: Either[String,Int] = Left(2304 * Pi's last digit is an odd number)
```
:::

Above we are using the composed function directly, but because it has the form of `A => F[B]` we are able to apply it using `flatMap()` as well:

:::{.numberLines}
```scala
scala> Either.right[String, Int](2).flatMap(evensSquaredWithEvenLastDigitsOfPi)
val res4: Either[String,Int] = Right(2)

scala> Either.right[String, Int](19).flatMap(evensSquaredWithEvenLastDigitsOfPi)
val res5: Either[String,Int] = Left(19 is an odd number)

scala> Either.right[String, Int](4).flatMap(evensSquaredWithEvenLastDigitsOfPi)
val res6: Either[String,Int] = Left(16 * Pi's last digit is an odd number)

scala> Either.left[String, Int]("Nothing's gonna happen").flatMap(evensSquaredWithEvenLastDigitsOfPi)
val res7: Either[String,Int] = Left(Nothing's gonna happen)
```
:::

Thus Kleisli composition is to `flatMap()` as function composition is to `map()`, with the key difference being that the case of the context may be changed by any one of the composed functions. Kleisli composition is powerful in that entire processes may be composed into step-by-step operations and applied as a single unit to a context via `flatMap()` or directly to an unlifted argument.

### The _for comprehension_ and imperative programming

Scala provides a syntax sugar over `flatMap()` in the form of the [_for comprehension_](https://docs.scala-lang.org/tour/for-comprehensions.html). Any type that provides both the `map()` and `flatMap()` functions are able to participate in this syntax sugar, and it allows for monadic operations to be expressed as if they were written as procedural code. This means that you don't have to rely on Kleisli composition to express complex flows of logic, and that you also aren't limited to single input/output pipelines of functions.

As a simple example, here are the above functions leveraged using a for comprehension:

:::{.numberLines}
```scala
scala> :load console/imports.scala

scala> val squareEvens: Int => Either[String, Int] = {
     |   case n if n % 2 == 0 => Either.right[String, Int](n * n)
     |   case n               => Either.left[String, Int](s"$n is an odd number")
     | }
val squareEvens: Int => Either[String,Int] = $Lambda$5278

scala> val piEvenLastDigit: Int => Either[String, Int] = { n =>
     |   val nPi = n * scala.math.Pi
     |   val lastDigit = nPi.toString.last.asDigit
     |   if (lastDigit % 2 == 0) {
     |     Either.right[String, Int](lastDigit)
     |   } else {
     |     Either.left[String, Int](s"$n * Pi's last digit is an odd number")
     |   }
     | }

scala> val evensSquaredWithEvenLastDigitsOfPi: Int => Either[String, Int] =
     | n => for {
     |   squaredEvenN <- squareEvens(n)
     |   piEvenLastDigitN <- piEvenLastDigit(squaredEvenN)
     | } yield piEvenLastDigitN

val evensSquaredWithEvenLastDigitsOfPi: Int => Either[String,Int] = $Lambda$5263

scala> evensSquaredWithEvenLastDigitsOfPi(2)
val res1: Either[String,Int] = Right(2)

scala> Either.right[String, Int](2).flatMap(evensSquaredWithEvenLastDigitsOfPi)
val res2: Either[String,Int] = Right(2)

scala> Either.left[String, Int]("Nothing's gonna happen").flatMap(evensSquaredWithEvenLastDigitsOfPi)
val res3: Either[String,Int] = Left(Nothing's gonna happen)
```
:::

The key thing that a for comprehension allows is for multiple arguments to be lowered from contexts and applied to a multi-argument function that returns another context. An example of such an operation might look like this:

:::{.numberLines}
```scala
def sendMarketingUpdate(updateId: Int, userId: Int): Future[MarketingUpdateStatus] =
  for {
    user <- loadUser(userId)
    marketingTemplate <- loadTemplate(updateId)
    composedEmail <- composeEmailForUser(user, marketingTemplate)
    result <- sendEmail(composedEmail)
  } yield result.toMarketingUpdateStatus
```
:::

Because `composeEmailForUser()` requires both a `user` and `marketingTemplate`, this operation function is more easily expressed using a for comprehension.

However you don't have to abandon applicative functions when using for comprehensions. The above function may be written to parallelize loading the user and template:

:::{.numberLines}
```scala
def sendMarketingUpdate(updateId: Int, userId: Int): Future[MarketingUpdateStatus] =
  for {
    (user, marketingTemplate) <- (loadUser(userId), loadTemplate(updateId)).tupled
    composedEmail <- composeEmailForUser(user, marketingTemplate)
    result <- sendEmail(composedEmail)
  } yield result.toMarketingUpdateStatus
```
:::

The code can be trimmed just a little bit more, as well:

:::{.numberLines}
```scala
def sendMarketingUpdate(updateId: Int, userId: Int): Future[MarketingUpdateStatus] =
  for {
    composedEmail <- (loadUser(userId), loadTemplate(updateId)).mapN(composeEmailForUser)
    result <- sendEmail(composedEmail)
  } yield result.toMarketingUpdateStatus
```
:::

This means that with applicative functions, you can express imperative code to some degree by gating functions dependent on a set of inputs, such as `composeEmailForUser()`, and lean on monad's `flatMap()` for when you need absolute ordering of computations. These two styles of computation thus complement each other.

## Becoming a Monad

In order to become a `Monad`, an effect type must implement the typeclass. Let's implement instances for the usual suspects, `Option`, `Either`, and `List`. As `Monad` is a specialization of `Applicative` and thus also `Functor`, we can simply upgrade our `Applicative` instances to become `Monad`s:

:::{.numberLines}
```scala
implicit val optionMonad: Monad[Option] = new Monad[Option] {

  // keeping existing definitions

  override def flatten[A](ffa: Option[Option[A]]): Option[A] =
    ffa match {
      case Some(fa) => fa
      case None     => None
    }
}

implicit def eitherMonad[X]: Monad[Either[X, *]] = new Monad[Either[X, *]] {

  // keeping existing definitions

  override def flatten[A](ffa: Either[X, Either[X, A]]): Either[X, A] =
    ffa match {
      case Right(fa) => fa
      case Left(x) => Left(x)
    }
}

implicit val listMonad: Monad[List] = new Monad[List] {

  // keeping existing definitions

  override def flatten[A](ffa: List[List[A]]): List[A] =
    ffa
      .foldLeft(List[A]()) { (outerResult, as) =>
        as.foldLeft(outerResult) { (innerResult, a) =>
          a :: innerResult
        }
      }
      .reverse
}
```
:::

> See the instances of `Monad` for
> [`Option`]({{code_repo}}/src/main/scala/green/thisfieldwas/embracingnondeterminism/effects/Option.scala#L127-L161),
> [`Either`]({{code_repo}}/src/main/scala/green/thisfieldwas/embracingnondeterminism/effects/Either.scala#L122-L156),
> and [`List`]({{code_repo}}/src/main/scala/green/thisfieldwas/embracingnondeterminism/effects/List.scala#L206-L244) in the sample repository.

In the above example, I implemented the `Monad` instances using `flatten()`. These could alternatively been implemented using `flatMap()`. You might try to do this if you wish as an exercise.
