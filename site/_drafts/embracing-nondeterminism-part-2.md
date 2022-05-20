---
title: "Embracing Nondeterminism Part II: Permitting or Halting Computation"
description: Leveraging the effects of two or more contexts to allow computations to proceed or halt.
author: Logan McGrath
comments: true
date: 2022-05-07T14:21:23-0700
tags: functional programming, programming, scala, design patterns
layout: post
twitter:
  image: /images/tags/functional-programming/functional-grass-512x512.png
og:
  image:
    url: /images/tags/functional-programming/functional-grass-512x512.png
    alt: Leveraging the effects of two or more contexts to allow computations to proceed or halt.
code_repo: https://bitsof.thisfieldwas.green/keywordsalad/embracing-nondeterminism-code/src/branch/part2
---

Remember functors? Recall from my last post, {{linkedTitle "_posts/2022-03-15-embracing-nondeterminism-part-1.md"}}, they are structures that define a single function: `map()`. What `map()` allows you to do is lift a function into a context and so that it may be applied to the context's term if the context is in the desired case, but performing no action if the context is not. In this post we will explore how to exploit contexts in their undesired cases to halt computations in order to express control flow.

<!--more-->

> The code that accompanies this post may be found [here]({{code_repo}}).

## Motivating applicative functors as a design pattern

Recall the `Functor` typeclass:

:::{.numberLines}
```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
object Functor {
  def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
}
```
:::

> [See here]({{code_repo}}/src/main/scala/green/thisfieldwas/embracingnondeterminism/typeclasses/Functor.scala) for the definition in the sample repository.

For any context `F[A]`, the `map()` function accepts another function `f: A => B` and applies it to the term within the context, giving back `F[B]`.

Contexts that are functors thus allow abstraction over unknown cases. For example, a function `f: A => B` may be lifted into an `Option[A]` and applied if an instance of `A` is present. If no instance of `A` is present, then nothing happens. Specifically, by using `map()` the function `f` is unconcerned with the unknown quantity of `A`'s presence. Many contexts encode dimensions of unknown quantities, and as functors they completely abstract the nondeterminism of these quantities, allowing the business logic expressed by the lifted functions to focus only on the terms the operate against.

What this means is that for any context in the desired case, such as a `Some` of `Option[A]` or a `Right` of `Either[X, A]`, the function `f` will be applied when lifted with `map()`. Any number of functions may be applied to the new contexts returned by subsequent applications of `map()`, and they will all apply as the initial context was in the desired case. Functors thus represent a form of data transformation, as they transform data if data exists, or they simply return a void context if data does not exist, i.e. they _propagate_ the undesired case.

You can try for yourself from the sample repository's `sbt console`:

:::{.numberLines}
```scala
scala> :load console/imports.scala

scala> val desiredOption: Option[Int] = Some(42)
val desiredOption: Option[Int] = Some(42)

scala> desiredOption.map(_.toString).map(numStr => s"backwards: ${numStr.reverse}")
res0: Option[String] = Some(backwards: 24)

scala> val undesiredOption: Option[Int] = None
val undesiredOption: Option[Int] = None

scala> undesiredOption.map(_.toString).map(numStr => s"backwards: ${numStr.reverse}")
res1: Option[String] = None
```
:::

Functors however only allow transformation of data from a sole instance of a context. This means that functions applicable to `map()` may only have the form `f: A => B`. Multiple inputs to a function lifted with `map()` are not possible with this abstraction. Consequently, as there is only one input, control flow is not possible as `map()` always applies `f` if the context is in its desired case, and no instance of the term `A` will cause `map()` to return the context in an undesired case to halt further computation against the context. There is no way to control the flow of execution against contexts by using functors.

## Introducing control flow

What function might allow for control flow against contexts? There is one, `map2()`:

:::{.numberLines}
```scala
def map2[F[_], A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
```
:::

This two-argument analog of `map()` unlocks a key capability: controlling whether to proceed or halt computation against the terms contained within the contexts `fa` and `fb`. If both `fa` and `fb` are in their desired case, then there are instances of `A` and `B` against which the function `f` may be applied. But if either one or both are in the undesired case, `f` does not apply, and the undesired cases are propagated through `F[C]`. This means that `fa` and `fb` become levers with which to halt computations against the produced context `F[C]`.

Take for example addition lifted into the context of `Option[Int]` within the example sample repository's `sbt console`:

:::{.numberLines}
```scala
scala> :load console/imports.scala

scala> val F = Applicative[Option]
val F: Applicative[Option] = Option$Instances$$anon$1

scala> F.map2(Option(2), Option(2))(_ + _)
val res0: Option[Int] = Some(4)

scala> F.map2(Option(2), Option[Int]())(_ + _)
val res1: Option[Int] = None

scala> F.map2(Option[Int](), Option(2))(_ + _)
val res2: Option[Int] = None

scala> F.map2(Option[Int](), Option[Int]())(_ + _)
val res3: Option[Int] = None
```
:::

Only when both arguments are in the desired case does the function of addition apply. If either or both arguments are in the undesired case, then the undesired case is propagated. This halts any further operations against the context. The functions that produce the two input contexts are thus capable of controlling whether computation via `f` proceeds and permits further computation against the context `F[C]`.

## Applicative functors

The `map2()` function is implemented using a new structure, a specialization of a functor called an **applicative functor**, or simply an _applicative_.

Applicative specialization arises in the type of `A` contained within functor `F[_]`. If `A` is merely an opaque type, then `F[A]` is a functor and no more. But if however `A` is specifically known to have some type `A => B`, that is to say _`A` is a function_, then `F[A => B]` is an _applicative_ functor.

Applicatives define two functions from which many more are derived, including `map2()`:

:::{.numberLines}
```scala
trait Applicative[F[_]] extends Functor[F] {

  def pure[A](a: A): F[A]

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  // other functions are introduced below
}

object Applicative {

  def apply[F: Applicative]: Applicative[F] = implicitly[Applicative[F]]

  object Syntax {

    implicit class ApplicativeIdOps[A](val a: A) extends AnyVal {

      def pure[F[_]](implicit F: Applicative[F]): F[A] = F.pure(a)
    }

    implicit class ApplicativeOps[F[_], A, B](val ff: F[A => B]) extends AnyVal {

      def ap(fa: F[A])(implicit F: Applicative[F]): F[B] = F.ap(ff)(fa)
    }
  }
}
```
:::

> [See here]({{code_repo}}/src/main/scala/green/thisfieldwas/embracingnondeterminism/typeclasses/Applicative.scala) for the definition in the sample repository.

Note that `Applicative` extends `Functor` as it is a specialization. All applicatives are also functors and therefore also provide the `map()` function.

The new functions afforded by applicatives offer two capabilities:

* `pure()` which lifts the result of a pure computation `A` into the context such that `pure: A => F[A]`. This is essentially a constructor producing a context in the desired case, such as `Some` for `Option[A]` or `Right` for `Either[X, A]`.
* `ap()`, read as _apply_, for applying a lifted function to a lifted argument.

You might be wondering why a function would ever be lifted into a context? I will demonstrate why this is desirable in how `ap()` works by defining `map2()` within `Applicative`:

:::{.numberLines}
```scala
def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
  ap(ap(pure(f.curried))(fa))(fb)
```
:::

Lifting `f` into the context works following these steps:

1. By currying the function `f: (A, B) => C`, it becomes `A => B => C`.
2. Lifting it with `pure()` gives `F[A => B => C]` in the desired case.
3. In this form, `ap()` may apply the function to context argument `fa: F[A]` which will give back `F[B => C]` in the desired case if `fa` is itself in the desired case.
4. Then `ap()` may apply the function to the context argument `fb: F[B]` which will give back `F[C]` in the desired case if `fb` is itself in the desired case.

Each step of lifted function application accounts for the case of the function and argument contexts and halts if either context is in the undesired case. The undesired case will propagate instead if and when it exists.

You might have noticed, `map2()` looks an awful lot like `map()`. In fact, `Applicative` provides a default implementation of `map()`:

:::{.numberLines}
```scala
def map[A, B](fa: F[A])(f: A => B): F[B] =
  ap(pure(f))(fa)
```
:::

If your context is an applicative, then it is also a functor with no extra work. But you can always provide your own implementation of `map()` if it makes sense to.

## Validation as a use case

Let's walk through a powerful capability of applicatives: _validation_.

### Validating a `User` from external data

When constructing a `User` from data that we receive from an external source, such as a form or API, we can use `ap()` to lift `User`'s curried constructor into a validation context and apply it to validated arguments. If all arguments are valid, then we should receive a validation context containing a valid `User`. If any arguments are invalid, then we should receive an invalid context with all reasons for validation failure.

Here is the definition for `User`:

:::{.numberLines}
```scala
case class User(username: String, email: String, password: String)
```
:::

Each of `username`, `email`, and `password` must to be valid in order for `User` itself to be valid. This requires the introduction of a validation context:

:::{.numberLines}
```scala
sealed trait Validated[+E, +A] {

  def invalid: E

  def valid: A
}

case class Valid[+A](valid: A) extends Validated[Nothing, A] {

  def invalid: Nothing = throw new NoSuchElementException("Valid.invalid")
}

case class Invalid[+E](invalid: E) extends Validated[E, Nothing] {

  def valid: Nothing = throw new NoSuchElementException("Invalid.valid")
}
```
:::

> [See here]({{code_repo}}/src/main/scala/green/thisfieldwas/embracingnondeterminism/effects/Validated.scala) for the definitions in the sample repository.

#### Why use a new `Validated` context instead of `Either`?

The `Applicative` instance for `Either` in the context of validation has one key flaw: it short-circuits on the first `Left`.

Refer to `Either`'s implementation of `ap()`:

:::{.numberLines}
```scala
override def ap[A, B](ff: Either[X, A => B])(fa: Either[X, A]): Either[X, B] =
  (ff, fa) match {
    case (Right(f), Right(a)) => Right(f(a))
    case (Left(x), _)         => Left(x)
    case (_, Left(x))         => Left(x)
  }
```
:::

> [See here]({{code_repo}}/src/main/scala/green/thisfieldwas/embracingnondeterminism/effects/Either.scala#L145-L150) for `Either`'s definition of `ap()`.

For both cases of `Left` they are immediately returned and there is no specific handling for situations where both `ff` and `fa` may be in the `Left` case. This means that the first `Left` propagates and all subsequent `Left`s are swallowed. In the context of validation, this means that for any number of validation errors that the context might produce, we would only receive the first error. We would have to resolve the error and re-run the operation, and repeat for each subsequent error until the operation succeeded. This makes `Either` a very poor choice for modeling validation. It is strictly one thing or the other, whereas validation is specialized to propagate all reasons for failure.

### Validation via `Applicative`

`Validated` contains the valid value you want or the reason for invalidation. We could fail to receive a `User` for three or more reasons related to `username`, `email`, and `password` all being invalid, which implies that term `E` represents some data containing zero or more of _something_. This has a specific implication on how we define an instance of `Applicative` for `Validated`:

:::{.numberLines}
```scala
implicit def validatedApplicative[E]: Applicative[Validated[E, *]] =
  new Applicative[Validated[E, *]] {

    def pure[A](a: A): Validated[E, A] = Valid(a)

    def ap[A, B](ff: Validated[E, A => B])(fa: Validated[E, A]): Validated[E, B] =
      (ff, fa) match {
        case (Valid(f), Valid(a)) => Valid(f(a))
        case (Invalid(x), Invalid(y)) => ??? /* what do we do with these two E's? */
        case (Invalid(x), _) => Invalid(x)
        case (_, Invalid(y)) => Invalid(y)
      }
  }
```
:::

When there are two instances of `E` we don't have a way to combine them as `E` is an opaque type. Without concretely defining `E`, such as with `List[String]` or another similar structure, we won't be able to combine their values, but this creates an inflexible API. Specifically, this inability to combine `E` leaves `Validated` in the same position that `Either` is in: the first undesired case propagates and subsequent cases are swallowed. How do we combine the undesired cases?

### Modeling combinable structures

Structures defining a `combine()` function form a typeclass known as a **semigroup** under a specific condition: that `combine()` is associative. Semigroups are very common, and constraining `E` to have an instance of `Semigroup` provides great API flexibility. First, let's see how the `Semigroup` typeclass is defined:

:::{.numberLines}
```scala
trait Semigroup[A] {
  def combine(left: A, right: A): A
}
object Semigroup {
  def apply[A: Semigroup]: Semigroup[A] = implicit[Semigroup[A]]
}
```
:::

> [See here]({{code_repo}}/src/main/scala/green/thisfieldwas/embracingnondeterminism/data/Semigroup.scala) for the definition in the sample repository.

A semigroup is thus an additive form of data. Here's a few familiar data types that you may have used as semigroups without realizing it:

* [Lists under concatenation]({{code_repo}}/src/test/scala/green/thisfieldwas/embracingnondeterminism/stdlib/ListSpec.scala#L28)
* [Strings under concatenation]({{code_repo}}/src/test/scala/green/thisfieldwas/embracingnondeterminism/stdlib/StringSpec.scala#L10)
* [Integers under addition]({{code_repo}}/src/test/scala/green/thisfieldwas/embracingnondeterminism/stdlib/IntegerSpec.scala#L13)
* [Booleans under `&&`]({{code_repo}}/src/test/scala/green/thisfieldwas/embracingnondeterminism/stdlib/BooleanSpec.scala#L10)
* [Sets under union]({{code_repo}}/src/test/scala/green/thisfieldwas/embracingnondeterminism/stdlib/SetSpec.scala#L10)

The `combine()` function must be associative. This can be tested with a `scalacheck` property:

:::{.numberLines}
```scala
def checkSemigroupLaws[S: Semigroup: Arbitrary](): Unit = {

  import Semigroup.Syntax._

  property("Semigroup preserves associativity") {
    forAll(for {
      a <- arbitrary[S]
      b <- arbitrary[S]
      c <- arbitrary[S]
    } yield (a, b, c)) { case (a, b, c) =>
      (a |+| b) |+| c mustBe a |+| (b |+| c)
    }
  }
}
```
:::

> [See here]({{code_repo}}/src/test/scala/green/thisfieldwas/embracingnondeterminism/data/SemigroupLaws.scala) for the definition in the sample repository.

Given an `E` with an instance of `Semigroup`, we can define an `Applicative` instance for `Validated`:

:::{.numberLines}
```scala
implicit def validatedApplicative[E: Semigroup]: Applicative[Validated[E, *]] =
  new Applicative[Validated[E, *]] {
    import Semigroup.Syntax._

    override def pure[A](a: A): Validated[E, A] = Valid(a)

    override def ap[A, B](ff: Validated[E, A => B])(fa: Validated[E, A]): Validated[E, B] =
      (ff, fa) match {
        case (Valid(f), Valid(a))     => Valid(f(a))
        case (Invalid(x), Invalid(y)) => Invalid(x |+| y) // propagate both undesired cases via Semigroup
        case (Invalid(x), _)          => Invalid(x)
        case (_, Invalid(y))          => Invalid(y)
      }
  }
```
:::

> [See here]({{code_repo}}/src/main/scala/green/thisfieldwas/embracingnondeterminism/effects/Validated.scala#L98-L104) for the definition in the sample repository.

This means that `Validated` is usable as an `Applicative` for any case where `E` is combinable. What implications does this have?

* Errors may exist in a non-zero amount when the context is `Invalid`.
* When there are errors, the amount is unbounded.
* Combining errors can be frequent, and should be computationally cheap.

Naively, a `List[String]` works for `E`. It forms a `Semigroup` under concatenation, but concatenation isn't cheap in Scala `List`s. It can also be empty per its type, which means that as `E` you have to code for invariants where it is actually empty.

There exists a better structure, and we can whip it together pretty quick. The `NonEmptyChain`:

:::{.numberLines}
```scala
trait NonEmptyChain[+A] {

  import NonEmptyChain._

  def head: A

  def tail: Option[NonEmptyChain[A]]

  def length: Int

  def cons[B >: A](newHead: B): NonEmptyChain[B] = prepend(Singleton(newHead))

  def prepend[B >: A](prefix: NonEmptyChain[B]): NonEmptyChain[B] = prefix.append(this)

  def append[B >: A](suffix: NonEmptyChain[B]): NonEmptyChain[B] = Append(this, suffix)
}

object NonEmptyChain {

  def apply[A](value: A, rest: A*): NonEmptyChain[A] =
    rest.map[NonEmptyChain[A]](Singleton(_)).foldLeft[NonEmptyChain[A]](Singleton(value))(_ append _)

  case class Singleton[+A](head: A) extends NonEmptyChain[A] {

    override def tail: Option[NonEmptyChain[A]] = None

    override def length: Int = 1
  }


  case class Append[+A](prefix: NonEmptyChain[A], suffix: NonEmptyChain[A]) extends NonEmptyChain[A] {

    override def head: A = prefix.head

    override def tail: Option[NonEmptyChain[A]] = prefix.tail.map(suffix.prepend).orElse(Some(suffix))

    override def length: Int = prefix.length + suffix.length
  }

  object Instances {

    /** `NonEmptyChain` forms a `Semigroup` under the `append()` function.
      */
    implicit def nonEmptyChainSemigroup[A]: Semigroup[NonEmptyChain[A]] = _ append _
  }
}

```
:::

> [See here]({{code_repo}}/src/main/scala/green/thisfieldwas/embracingnondeterminism/data/NonEmptyChain.scala) for the definition in the sample repository.

Using a `NonEmptyChain`, we can start writing validation functions for `User` to test that they work:

:::{.numberLines}
```scala
import Applicative.Syntax._
import NonEmptyChain.Instances._
import Validated.Instances._
import Validated.Syntax._

case class User(username: String, email: String, password: String)

def validateUsername(username: String): ValidatedNec[String, String] =
  if (username.isEmpty) {
    "Username can't be blank".invalidNec[String]
  } else {
    username.validNec[String]
  }

def validateEmail(email: String): ValidatedNec[String, String] =
  if (!email.contains('@')) {
    "Email does not appear to be valid".invalidNec[String]
  } else {
    email.validNec[String]
  }

def validatePassword(password: String): ValidatedNec[String, String] =
  List(
    Option.cond(password.length < 8)("Password must have at least 8 characters"),
    Option.cond(!password.exists(_.isLower))("Password must contain at least one lowercase character"),
    Option.cond(!password.exists(_.isUpper))("Password must contain at least one uppercase character"),
    Option.cond(!password.exists(_.isDigit))("Password must contain at least one digit"),
    Option.cond(!password.exists("`~!@#$%^&*()-_=+[]{}\\|;:'\",.<>/?".contains(_)))(
      "Password must contain at least one symbol"
    ),
  ).foldLeft(password.validNec[String]) { (validated, errorOption) =>
    errorOption.fold(validated) { message =>
      Invalid(validated.fold(_.cons(message))(_ => NonEmptyChain(message)))
    }
  }

def validateUser(username: String, email: String, password: String): Validated[NonEmptyChain[String], User] =
  User.curried.pure[Validated[NonEmptyChain[String], *]]
    .ap(validateUsername(username))
    .ap(validateEmail(email))
    .ap(validatePassword(password))
```
:::

Using this function, we can attempt to create a `User` with nothing but invalid data and get the complete collection of errors back:

:::{.numberLines}
```scala
val validatedUser = validateUser(
  username = "",
  email = "bananaphone",
  password = "\n\t\r\r\r",
)
inside(validatedUser) { case Invalid(reasons) =>
  (reasons.toSeq should contain).theSameElementsAs(
    Seq(
      "Username can't be blank",
      "Email does not appear to be valid",
      "Password must have at least 8 characters",
      "Password must contain at least one lowercase character",
      "Password must contain at least one uppercase character",
      "Password must contain at least one digit",
      "Password must contain at least one symbol",
    )
  )
}
```
:::

And with valid data, receive a constructed `User`:

:::{.numberLines}
```scala
val validatedUser = validateUser(
  username  = "commander.keen",
  email = "commander.keen@vorticonexterminator.net",
  password = "m4rti@an$Rul3",
)
inside(validatedUser) { case Valid(user) =>
  user shouldBe User(
    username = "commander.keen",
    email = "commander.keen@vorticonexterminator.net",
    password = "m4rti@an$Rul3",
  )
}
```
:::

> [See here]({{code_repo}}/src/test/scala/green/thisfieldwas/embracingnondeterminism/effects/ValidatedSpec.scala#L50) for the specs in the sample repository.

Applicatives thus enable entire computations to succeed if all context arguments are in the desired case. If any argument is in the undesired case, then this case is propagated and the computation as a whole fails.

Each of `validateUsername()`, `validateEmail()`, and `validatePassword()` act as levers on whether a `User` is successfully produced. Writing specific if-statements to guide whether a `User` is produced or errors returned instead is not required: the `Applicative` typeclass succinctly abstracts away the necessary plumbing to control the flow of logic required to handle undesired cases. Errors are declared where they should occur and the abstraction handles the rest.

### Independent evaluation of contexts

It may not have been obvious from `validateUser()`, but each validation function evaluates independently of the other validation functions. In the `Validation` context, this means that each function executes without impacting the other functions regardless of individual success or failure. Imagine for a moment, what if the functions were evaluated within an asynchronous context?

Let's define a function on the `Applicative` typeclass that gathers the results of some effectful operations:

:::{.numberLines}
```scala
def sequence[A](listFa: List[F[A]]): F[List[A]] =
  listFa.foldLeft(pure(List[A]()))((fListA, fa) => map2(fa, fListA)(_ :: _))
```
:::

> [See here]({{code_repo}}/src/main/scala/green/thisfieldwas/embracingnondeterminism/typeclasses/Applicative.scala#L70-L71) for the definition in the sample repository.

And then if we load three `User`s at once:

:::{.numberLines}
```scala
def loadUser(email: String): Future[User] = ???

val loadingUsers = Applicative[Future].sequence(List(
  loadUser("test@email.com"),
  loadUser("student@school.edu"),
  loadUser("admin@foundation.net"),
))
```
:::

The variable `loadingUsers` now contains `Future[List[User]]`. As each `Future[User]` resolves, they are collected into a `List`. Because each `loadUser()` function executes independently, this has a profound implication in the context of a `Future`: they are executed concurrently!

Should any `User` fail to load, the undesired case will propagate and the `sequence()` function will halt. All other `User`s are discarded.

The pattern offered by `Applicative` is an all-or-nothing result in its output. If all inputs are in the desired case, then the output will be in the desired case as well. But if any are in an undesired case, then the undesired case propagates and computation halts.

#### Products of contexts

Given a 2-tuple of functors `(F[A], F[B])` you can invert the nesting of the context and the 2-tuple using the `pure()` and `ap()` functions from `Applicative` with the following steps:

:::{.numberLines}
```scala
scala> :load console/imports.scala

scala> val productOfSomes = (Option(42), Option("banana"))
val productOfSomes: (Option[Int], Option[String]) = (Some(42), Some("banana"))

scala> val someOfProduct = (_: Int, _: String).curried.pure[Option].ap(productOfSomes._1).ap(productOfSomes._2)
val someOfProduct: Option[(Int, String)] = Some((42, "banana"))
```
:::

This has an important implication, specifically that unlike the `sequence()` function these operations allow for gathering effectful operations that produce contexts with heterogeneous terms. Scala allows for tuples up to 22 elements, and it makes sense to abstract the above operations for each tuple size, especially because even at just 2 elements writing all of these out is already clunky!

In the sample repository, I have written a macro which [adds two extension methods]({{code_repo}}/src/main/scala/green/thisfieldwas/embracingnondeterminism/data/GenerateTupleSyntax.scala) to each tuple size. Here's the code that is generated for the 2-tuple:

:::{.numberLines}
```scala
implicit class Tuple2Ops[F[_], T1, T2](val t: (F[T1], F[T2])) extends AnyVal {

  // Converts a product of contexts `(F[T1], F[T2])` into a context of a product `F[(T1, T2)]`
  def tupled(implicit F: Applicative[F]): F[(T1, T2)] =
    mapN(Tuple2.apply)

  // Applies a product of contexts `(F[T1], F[T1])` to function, giving a result in context
  def mapN[Out](f: (T1, T2) => Out)(implicit F: Applicative[F]): F[Out] =
    F.ap(F.ap(F.pure(f.curried))(t._1))(t._2)
}
```
:::

Being able to invert the nesting of a tuple and a context is most powerful when constructing case classes from arguments produced by effectful operations. By using the `mapN()` function, for example, validating arguments to the `User` constructor may be rewritten like this:

:::{.numberLines}
```scala
def validateUser(username: String, email: String, password: String): Validated[NonEmptyChain[String], User] =
  (
    validateUsername(username),
    validateEmail(email),
    validatePassword(password)
  ).mapN(User)
```
:::

This syntax afforded by the `mapN()` extension method is much more concise and closely matches the constructor arguments order passed to `User` itself. After all `User` is a product of results produced by contexts, which evaluate independently of each other as they do in the `sequence()` function.

## Becoming an Applicative

In order to become an `Applicative`, an effect type must implement the typeclass. Let's implement instances for the usual suspects, `Option`, `Either`, and `List`. As `Applicative` is a specialization of `Functor`, we can simply upgrade our current `Functor` instances to become `Applicatives`:

:::{.numberLines}
```scala
implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {

  // removed map() and now using the default

  override def pure[A](a: A): Option[A] = Some(a)

  override def ap[A, B](ff: Option[A => B])(fa: F[A]): F[B] =
    (ff, fa) match {
      case (Some(f), Some(a)) => Some(f(a))
      case _ => None
    }
}

implicit def eitherApplicative[X]: Applicative[Either[X, *]] = new Applicative[Either[X, *]] {

  // removed map() and now using the default

  override def pure[A](a: A): Either[X, A] = Right(x)

  override def ap[A, B](ff: Either[X, A => B])(fa: F[A]): F[B] =
    (ff, fa) match {
      case (Right(f), Right(a)) => Right(f(a))
      case (Left(x), _) => Left(x)
      case (_, Left(x)) => Left(x)
    }
}

implicit val listApplicative: Applicative[List] = new Applicative[List] {

  // removed map() and now using the default

  override def pure[A](a: A): List[A] = List(a)

  override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
    ff.foldLeft(List[B]()) { (outerResult, f) =>
      fa.foldLeft(outerResult) { (innerResult, a) =>
        f(a) :: innerResult
      }
    }.reverse
}
```
:::

> See the instances of `Applicative` for
> [`Option`]({{code_repo}}/src/main/scala/green/thisfieldwas/embracingnondeterminism/effects/Option.scala#L127-L155),
> [`Either`]({{code_repo}}/src/main/scala/green/thisfieldwas/embracingnondeterminism/effects/Either.scala#L122-L151),
> and [`List`]({{code_repo}}/src/main/scala/green/thisfieldwas/embracingnondeterminism/effects/List.scala#L206-L235) in the sample repository.

`Option` and `Either`'s instances of `Applicative` are straight-forward: if a function and argument are present, they are applied and the result returned in the desired case. If either are missing, then the undesired case is propagated instead.

`List` looks very different at first glance, but conceptually performs the same way. Specifically, `List` performs a Cartesian product of its functions and arguments, applying each pair together and building a new `List` from the results. If either the function or argument `List` are empty, then an empty result `List` is returned, as an empty `List` represents the undesired case.

## Applicative laws

`Option`, `Either`, and especially `List`'s `Applicative` instances look different. How do we know that they are well-behaved as applicatives? Just like functors, applicatives are expected to conform to a set of laws defined in the higher math of category theory.

There are four applicative laws, which must hold for all applicatives in addition to the functor laws.

1. **Preservation of identity functions**: A lifted identity function applied to a lifted argument is the same as the identity function applied directly to the lifted argument.
2. **Preservation of function homorphism**: Lifting a function and an argument then applying them produces the same result as applying the unlifted function and unlifted argument then lifting the result.
3. **Preservation of function interchange**: Applying a lifted function to a lifted value produces the same result as a lifted function of the unlifted function applied to the unlifted value applied to the lifted function. (What a mouthful! It's clearer in code.)
4. **Preservation of function composition**: Given lifted functions `ff: F[A => B]` and `fg: F[B => C]` and argument `fa: F[A]`: lifting `compose()` and applying `fg`, `ff`, and `fa` produces the same result as applying `fg` after applying `ff` to `fa`.

These laws are rigorous and we can write tests for these to prove that our applicative instances are defined correctly.

### Testing "for all" `F[_]`

In order to properly test our applicative instances, we need to be able to generate a broad range of inputs to verify that the applicative properties hold with a high degree of confidence. Specifically, "for all" `List`s, for example, the property checks for `Applicative` must pass for each generated instance. We will leverage `scalacheck` for property-based testing. `scalacheck` will generate for us a set of arbitrary instances of the contexts and execute tests called _property checks_ against each to verify that each check passes. If all checks pass, then the property may be considered to hold "for all" instances of the tested context.

Generating an arbitrary context `F[_]` containing an arbitrary `A` is not supported directly by `scalacheck`, however. We can leverage this typeclass below to enable generating instances of `F[A]` from any generator for `A`:

:::{.numberLines}
```scala
import org.scalacheck.Gen

trait LiftedGen[F[_]] {

   def lift[A](gen: Gen[A]): Gen[F[A]]
 }

 object LiftedGen {

   def apply[F[_]: LiftedGen]: LiftedGen[F] = implicitly[LiftedGen[F]]

   object Syntax {

     implicit class LiftedGenOps[A](val gen: Gen[A]) extends AnyVal {

       def lift[F[_]: LiftedGen]: Gen[F[A]] = LiftedGen[F].lift[A](gen)
     }
   }
 }
```
:::

> [See here]({{code_repo}}/src/test/scala/green/thisfieldwas/embracingnondeterminism/util/LiftedGen.scala) for the definition in the sample repository.

### Defining the applicative laws as properties

Now we will walk through defining the properties of `Applicative` in such a way that we simply supply the contexts as the argument to the test. This way we only define the properties once.

:::{.numberLines}
```scala
trait ApplicativeLaws { this: Laws with FunctorLaws =>

  /** Defined per Applicative laws taken from the Haskell wiki:
    * [[https://en.wikibooks.org/wiki/Haskell/Applicative_functors#Applicative_functor_laws]]
    *
    * These laws extend the functor laws, so `checkFunctorLaws[F]()` should be
    * executed alongside this function.
    *
    * @param TT The type tag of the context
    * @tparam F The context type being tested
    */
  def checkApplicativeLaws[F[_]: Applicative: LiftedGen]()(implicit TT: TypeTag[F[Any]]): Unit = {
    property(s"${TT.name} Applicative preserves identity functions") {
      // A lifted identity function applied to a lifted argument is the same as
      // the identity function applied directly to the lifted argument.
      forAll(arbitrary[Double].lift) { v =>
        // Haskell: pure id <*> v = v
        (identity[Double] _).pure.ap(v) mustBe identity(v)
      }
    }
    property(s"${TT.name} Applicative preserves function homomorphism") {
      // Lifting a function and an argument then applying them produces the
      // same result as applying the unlifted function and unlifted argument
      // then lifting the result.
      forAll(for {
        f <- arbitrary[Double => String]
        x <- arbitrary[Double]
      } yield (f, x)) { case (f, x) =>
        // Haskell: pure f <*> pure x = pure (f x)
        f.pure.ap(x.pure) mustBe f(x).pure
      }
    }
    property(s"${TT.name} Applicative preserves function interchange") {
      // Applying a lifted function to a lifted value produces the same result
      // as a lifted function of the unlifted function applied to the unlifted
      // value applied to the lifted function. (What a mouthful!)
      forAll(for {
        u <- arbitrary[Double => String].lift
        y <- arbitrary[Double]
      } yield (u, y)) { case (u, y) =>
        // Haskell: u <*> pure y = pure ($ y) <*> u
        u.ap(y.pure) mustBe ((f: Double => String) => f(y)).pure.ap(u)
      }
    }
    property(s"${TT.name} Applicative preserves function composition") {
      // Given lifted functions `ff: F[A => B]` and `fg: F[B => C]` and
      // argument `fa: F[A]`: lifting `compose()` and applying `fg`, `ff`, and
      // `fa` produces the same result as applying `fg` after applying `ff` to
      // `fa`.
      val compose: (String => Int) => (Double => String) => Double => Int = g => f => g compose f
      forAll(for {
        u <- arbitrary[String => Int].lift
        v <- arbitrary[Double => String].lift
        w <- arbitrary[Double].lift
      } yield (u, v, w)) { case (u, v, w) =>
        // Haskell: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
        compose.pure.ap(u).ap(v).ap(w) mustBe u.ap(v.ap(w))
      }
    }
  }
}
```
:::

> [See here]({{code_repo}}/src/test/scala/green/thisfieldwas/embracingnondeterminism/typeclasses/ApplicativeLaws.scala) for the full definition of the trait.

With this trait, laws specs can be written for each context.

:::{.numberLines}
```scala
abstract class Laws extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)
}

class OptionLawsSpec extends Laws with FunctorLaws with ApplicativeLaws {

  import Option.Instances._

  implicit val optionLiftedGen: LiftedGen[Option] = new LiftedGen[Option] {

    override def lift[A](gen: Gen[A]): Gen[Option[A]] =
      Gen.lzy(
        Gen.oneOf(
          gen.map(Some(_)),
          Gen.const(None),
        )
      )
  }

  checkFunctorLaws[Option]()
  checkApplicativeLaws[Option]()
}

class EitherLawsSpec extends Laws with FunctorLaws with ApplicativeLaws {

  import Either.Instances._

  implicit def eitherLiftedGen[X: Arbitrary]: LiftedGen[Either[X, *]] = new LiftedGen[Either[X, *]] {

    override def lift[A](gen: Gen[A]): Gen[Either[X, A]] =
      Gen.lzy(
        Gen.oneOf(
          gen.map(Right(_)),
          arbitrary[X].map(Left(_)),
        )
      )
  }

  checkFunctorLaws[Either[String, *]]()
  checkApplicativeLaws[Either[String, *]]()
}

class ListLawsSpec extends Laws with FunctorLaws with ApplicativeLaws {

  import List.Instances._

  implicit val listLiftedGen: LiftedGen[List] = new LiftedGen[List] {

    override def lift[A](gen: Gen[A]): Gen[List[A]] =
      for {
        size <- Gen.sized(Gen.choose(0, _))
        items <- Gen.listOfN(size, gen)
      } yield List(items: _*)
  }

  checkFunctorLaws[List]()
  checkApplicativeLaws[List]()
}
```
:::

> See these laws specs for
> [`Option`]({{code_repo}}/src/test/scala/green/thisfieldwas/embracingnondeterminism/effects/OptionSpec.scala#L116-L136),
> [`Either`]({{code_repo}}/src/test/scala/green/thisfieldwas/embracingnondeterminism/effects/EitherSpec.scala#L112-L135),
> and [`List`]({{code_repo}}/src/test/scala/green/thisfieldwas/embracingnondeterminism/effects/ListSpec.scala#L158-L181) in the sample repository.

_Try running these specs!_

### Implications of the applicative laws

Each of `Option`, `Either`, and `List` conform to the applicative laws and we only had to write the properties once. These properties prove that functions and arguments used within these contexts maintain referential transparency in their arrangements and that the specific contexts do not change the factoring semantics of the code.

What does change, however, are these contexts' specific effects. For example, you would not have to refactor code abstracted by applicative functions if you changed the backing implementation from `Either` to `List`, but your code would produce potentially more than one result in the desired case.

This is the goal, however, as these effects' dimensions of unknown quantity should not burden our code. Instead, we push the complexity to the edge of the context, where it is important that our context is an `Either` or a `List`, and keep our business logic focused on individual instances contained within each context.

## What is enabled by applicatives?

Applicatives primarily offer parallel computation. Specifically, the arguments to applicative functions such as `ap()`, `map2()`, or `sequence()` are evaluated independently of one another, and their individual outputs as a whole influence the combined output of the functions consuming them.

When all inputs to an applicative function are in the desired case, then the output of the function will also be in the desired case. Conversely, if any input is in the undesired case, then it will be propagated through the function's output, and the other cases will be discarded. In this regard, applicative functions provide an _all-or-nothing_ operation.

Parallel computation provides some level of control flow, but it doesn't guide execution to proceed only if the previous execution has succeeded, as all operations evaluate independently of each other. Applicatives therefore do not provide a mechanism to support imperative programming. For this kind of control flow, you need to further specialize the applicative functor.

In my next post, we will explore the infamous _**monad**_ and how it enables imperative control flow in functional programming.
