---
title: "Embracing Nondeterminism Part II: Products of success or failure"
description: Abstracting nondeterminism and complexity in contexts in order to consume products of two or more in parallel.
author: Logan McGrath
comments: true
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

Remember functors? Recall from my last post, {{ linkedTitle "_posts/2022-03-15-embracing-nondeterminism-part-1.md" }}, they are structures that define a single operation: `map()`. What `map()` allows you to do is lift a function into a context and apply it to its term if context is in its desired case, but performing no action if not.

This abstraction is very elementary as it only permits function application against instances produced by a single context. Specifically, you can't use `map()` to apply the following function to two contexts:

:::{.numberLines}
```scala
val fa: F[A]
val fb: F[B]

def combine(a: A, b: B): C
```
:::

<!--more-->

## Motivating applicatives as a design pattern

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

For any context `F[A]`, the `map()` function accepts another function `f: A => B` and applies it to the term within the context, giving back `F[B]`.

Contexts that are functors thus allow you to abstract over unknown cases. For example, your function `f: A => B` may be lifted into an `Option[A]` and applied if an instance of `A` is present. If no instance of `A` is present, then nothing happens. Specifically, you as the author of the code don’t have to worry about the unknown quantity of `A`’s presence in order to use function `f`. Many contexts encode dimensions of unknown quantities, and as functors their nondeterminism is completely abstracted by `map()`.

What this means is that for any context in the desired state, such as a `Some` of `Option[A]` or a `Right` of `Either[X, A]`, the function `f` will be applied when lifted with `map()`. In fact, any number of functions may be applied to the new context returned by `map()`, and they will all apply as the initial contexts were in the desired case. Functors thus represent a form of data transformation, as they transform data if data exists, or they simply return a void context if data does not exist.

Functors however only allow transformation of data from a sole instance of a context. This means that all functions applicable to `map()` have the form `f: A => B`. Multiple inputs to a function lifted with `map()` are not possible with this abstraction. Consequently, as there is only one input, control flow is not possible as `map()` always runs `f` if the context is in its desired case, and no instance of `A` will cause `map()` to return the context in an undesired case to halt further computation.

## Control flow

What function might allow for control flow using contexts? There is one, `map2()`:

:::{.numberLines}
```scala
def map2[F[_], A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
```
:::

This two-argument analog of `map()` unlocks a key capability: controlling for failure. If both `fa` and `fb` are in their desired case, then there are instances of `A` and `B` to apply to function `f`. But if either one is in its undesired case, `f` does not run, and an `F[C]` is returned in the undesired case. This means that `fa` or `fb` become levers with which to halt computations against the context `F`.

Take for example addition lifted into the context of `Option[Int]`:

:::{.numberLines}
```scala
>>> map2(Some(2), Some(3))(_ + _)
/// Some(5)

>>> map2(Some(2), None)(_ + _)
/// None

>>> map2(None, Some(3))(_ + _)
/// None

>>> map2(None, None)(_ + _)
/// None
```
:::

Only when both arguments are in their desired case does addition apply. If either or both arguments are in their undesired case, then a context in its undesired case is returned. This means all further operations against the context are skipped. The functions that produce the two input contexts are thus capable of indicating whether this computation proceeds or halts, and the specific complexity of the contexts’ cases are abstracted from the function consuming their contents.

## Applicative functors

The `map2()` function is implemented using a new structure, a specialization of a functor called an applicative functor, or simply just _applicative_.

Applicative specialization arises in the type of `A` contained within functor `F[_]`. If `A` is merely an opaque type, then `F[A]` is a functor and no more. But if however `A` is specifically known to have some type `A => B`, that is to say _`A` is a function_, then `F[A => B]` is an _applicative_ functor.

Applicatives define two operations from which many more are derived, including `map2()`:

:::{.numberLines}
```scala
trait Applicative[F[_]] extends Functor[F] {

  def pure[A](a: A): F[A]

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
}

object Applicative {

  def apply[F: Applicative]: Applicative[F] = implicitly[Applicative[F]]
}
```
:::

These operations offer two capabilities:

* `pure()` which lifts the result of a pure computation `A` into the context such that `pure: A => F[A]`. This is essentially a constructor for a context in the desired case, such as `Some` for `Option` or `Right` for `Either`.
* `ap()`, read as _apply_, for applying a lifted function to a lifted argument.

You might be wondering why a function would be lifted into the context to begin with. First, I will demonstrate how `ap()` works by defining `map2()` within `Applicative`:

:::{.numberLines}
```scala
def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
  ap(ap(pure(f.curried))(fa))(fb)
```
::: 

Let’s break it down:

* By currying the function `f: (A, B) => C`, it becomes `A => B => C`.
* Lifting it with `pure()` gives `F[A => B => C]` in the desired case.
* In this form, `ap()` may apply the function to context argument `fa: F[A]` and will give back `F[B => C]` in the desired case if `fa` is itself in the desired case.
* Then `ap()` may be applied again with context argument `fb: F[B]` which will give back `F[C]` in the desired state if `fb` is itself in the desired case.

Each step of lifted function application accounts for the case of the function and argument contexts and halts if either context is in the undesired case.

You might have noticed, `map2()` looks an awful lot like `map()`. In fact, `Applicative` provides a default implementation of `map()`:

:::{.numberLines}
```scala
def map[A, B](fa: F[A])(f: A => B): F[B] =
  ap(pure(f))(fa)
```
::: 

If your context is an applicative, it is thus also a functor with no extra work. But you can always provide your own implementation of `map()` if it makes sense to.

## Validation with applicatives

Let's walk through my favorite concrete usage of applicatives: validation. When we construct a `User` from data that we receive from an external source, such as a form or API, we can use `ap()` to lift `User`'s constructor into a validation context and apply its validated arguments. If all arguments are valid, then we should receive a validation context containing a valid `User`. If any arguments are invalid, then we should receive an invalid context with all reasons for validation failure.

Here is the definition for `User`:

:::{.numberLines}
```scala
case class User(username: String, email: String, password: String)
```
:::

Each of `username`, `email`, and `password` must to be valid in order for `User` itself to be valid. This requires the introduction of a validation context.

:::{.numberLines}
```scala
sealed trait Validated[+E, +A]
case class Invalid[+E](e: E) extends Validated[E, Nothing]
case class Valid[+A](a: A) extends Validated[Nothing, A]
```
:::

`Validated` contains the term you want or the reason you didn’t get it. Realistically, we could fail to receive a `User` for three or more reasons related to `username`, `email`, and `password` all being invalid. Let’s define an applicative instance for `Validated` and see what arises:

:::{.numberLines}
```scala
implicit def validatedApplicative[E]: Applicative[Validated[E, *]] =
  new Applicative[Validated[E, *]] {

    def pure[A](a: A): Validated[E, A] = Valid(a)

    def ap[A, B](ff: Validated[E, A => B])(fa: Validated[E, A]): Validated[E, B] =
      (ff, fa) match {
        case (Valid(f), Valid(a)) => Valid(f(a))
        case (Invalid(x), Invalid(y)) => ??? /* we got two E’s? */
      }
  }
```
:::

We’ve hit a snag. Out of the box our `Validated` doesn’t make a general validation utility, as it doesn’t have a way to report all errors. Specifically this is because when we have a pair of `Invalid`s, we don’t have any way to combine their values without concretely defining `E`, such as with `List[String]`. How do we keep `E` abstract?

### Modeling errors as combinable data 

There is a structure that defines combinable data. Specifically _semigroups_ are any structure that define an associative `combine()` operation:

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

All that a `Semigroup` must satisfy is the following law:

:::{.numberLines}
```scala
property("Semigroup combine() is associative") {
  forAll(for {
    x <- arbitrary[A]
    y <- arbitrary[A]
    z <- arbitrary[A]
  } yield (x, y, z)) { case (x, y, z) => 
    val S = Semigroup[A]
    S.combine(x, S.combine(y, z)) mustBe S.combine(S.combine(x, y), z)
  }
}
```
:::

We can define `Applicative` for `Validated` when `E` has an instance for `Semigroup`:

:::{.numberLines}
```scala
implicit def validatedApplicative[E](implicit S: Semigroup[E]): Applicative[Validated[E, *]] =
  new Applicative[Validated[E, *]] {

    def pure[A](a: A): Validated[E, A] = Valid(a)

    def ap[A, B](ff: Validated[E, A => B])(fa: Validated[E, A]): Validated[E, B] =
      (ff, fa) match {
        case (Valid(f), Valid(a)) => Valid(f(a))
        case (Invalid(x), Invalid(y)) => Invalid(S.combine(x, y)) // gather all the errors!
        case (Invalid(x), _) => Invalid(x)
        case (_, Invalid(x)) => Invalid(x)
      }
  }
```
::: 

This means that `Validated` is usable for any case where `E` is combinable. But what does that actually mean for us?

* Errors may exist in a non-zero amount when the context is `Invalid`
* When there are errors, the amount is unbounded
* Combining errors can be frequent, and should be computationally cheap

Naively, a `List[String]` works for `E`. It forms a `Semigroup` under concatenation, but concatenation isn’t cheap in Scala `List`s! It can also be empty per its type, which means that as `E` you have to code for invariants where it is actually empty.

There exists a better structure, and we can whip it together pretty quick. The `NonEmptyChain`:

:::{.numberLines}
```scala
sealed trait NonEmptyChain[+A] {

  def head: A

  def tail: Option[NonEmptyChain[A]]

  def append[B >: A](right: NonEmptyChain[B]): NonEmptyChain[B] =
    Append(this, right)

  def toList: List[String] =
    head :: tail.getOrElse(List.empty[A])
}

case class Singleton[+A](head: A) extends NonEmptyChain[A] {

  def tail: Option[NonEmptyChain[A]] = None
}

case class Append[A+](left: NonEmptyChain[A], right: NonEmptyChain[A]) extends NonEmptyChain[A] {

  def head: A = left.head

  def tail: Option[NonEmptyList[A]] = left.tail.map(_.append(right)).getOrElse(right)

object NonEmptyChain {

  implicit def nonEmptyChainSemigroup[A]: Semigroup[NonEmptyChain[A]] =
    new Semigroup[NonEmptyChain[A]] {
      
      def combine(left: NonEmptyChain[A], right: NonEmptyChain[A]): NonEmptyChain[A] =
        left.append(right)
  }
  
  def apply[A](a: A): NonEmptyChain[A] = Singleton(a)
}
```
:::

Using a `NonEmptyChain`, we can start writing validation functions for `User`:

:::{.numberLines}
```scala
def validateEmail(email: String): Validated[NonEmptyChain[String], String] =
  if (email.contains("@")) {
    Valid(email)
  } else { 
    Invalid(NonEmptyChain("Email is invalid"))
  }

def validatePassword(password: String): Validated[NonEmptyChain[String], String] = 
  if (password.length >= 8) {
    Valid(password)
  } else {
    Invalid(NonEmptyChain("Password must be at least 8 characters"))
  }

def validateUsername(username: String): Validated[NonEmptyChain[String], String] =
  if (username.nonEmpty) {
    Valid(username)
  } else {
    Invalid(NonEmptyChain("Username can’t be blank"))
  }

def validateUser(username: String, email: String, password: Password): Validated[NonEmptyChain[String], User] = {
  val F = Applicative[Validated[NonEmptyChain[String], *]]
  F.ap(
    F.ap(
      F.ap(
        F.pure(User.apply.curried)
      )(validateUsername(username))
    )(validateEmail(email))
  )(validatePassword(password))
}
```
:::

Using this function, we can attempt to create a `User` with nothing but invalid data and get the complete collection of errors back:

:::{.numberLines}
```scala
val userValidated = validateUser(
  username = "", 
  email = "not an email",
  password = "12345"
)

inside(userValidated) { case Invalid(errors) => 
  errors.toList should contain only (
    "Username can’t be blank",
    "Email is invalid",
    "Password must be at least 8 characters",
  )
}
```
:::

And with valid data, receive a constructed `User`:

:::{.numberLines}
```scala
val userValidated = validateUser(
  username = "myuser1337",
  email = "test@email.com",
  password = "password12345",
)

inside(userValidated) { case Valid(user) => 
  user shouldBe User(
    username = "myuser1337",
    email = "test@email.com",
    password = "password12345",
  )
}
```
:::

Applicatives thus enable entire computations to succeed if all contextual arguments are in their desired case. If any argument is in an undesired case, then this case is propagated and the computation as a whole fails.

Each of `validateUsername()`, `validateEmail()`, and `validatePassword()` act as levers on whether a `User` is successfully produced. Writing specific if-statements to guide whether a `User` is produced or errors returned instead is not required: the `Applicative` typeclass succinctly abstracts away the necessary plumbing to control the flow of logic required to handle undesired cases. Errors are declared where they should occur and the abstraction handles the rest.

### Implications of independent levers

It may not have been obvious from `validateUser()`, but each validation function evaluates independently of the other validation functions. In the `Validation` context, this means that each function executes without impacting the other functions. Imagine for a moment, what if the functions executed within an asynchronous context? 

Let’s define a function on the `Applicative` typeclass that gathers the results of some effectful operations:

:::{.numberLines}
```scala
def sequence[A](fas: List[F[A]]): F[List[A]] =
  fas match {
    case headF :: tailF => map2(headF, sequence(tailF))(_ :: _)
    case Nil => Nil
  }
```
::: 

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

Should any `User` fail to load, its undesired case will propagate and the rest of the computation will fail. All other users are discarded.

The pattern offered by `Applicative` is an all-or-nothing result in its output. If all inputs are in their desired case, then the output will be in the desired case as well. But if any are in an undesired case, then those cases propagate and computation halts.

## Becoming an Applicative

In order to become an `Applicative`, an effect type must implement the typeclass. Let’s implement instances for the usual suspects, `Option`, `Either`, and `List`. As `Applicative` is a specialization of `Functor`, we can simply upgrade our current `Functor` instances to become `Applicatives`:

:::{.numberLines}
```scala
implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {

  // removed map() and now using the default

  def pure[A](a: A): Option[A] = Some(a)

  def ap[A, B](ff: Option[A => B])(fa: F[A]): F[B] =
    (ff, fa) match (
      case (Some(f), Some(a)) => Some(f(a))
      case _ => None
    }
}

implicit def eitherApplicative[X]: Applicative[Either[X, *]] = new Applicative[Either[X, *]] {

  // removed map() and now using the default

  def pure[A](a: A): Either[X, A] = Right(x)

  def ap[A, B](ff: Either[X, A => B])(fa: F[A]): F[B] =
    (ff, fa) match {
      case (Right(f), Right(a)) => Right(f(a))
      case (Left(x), _) => Left(x)
      case (_, Left(x)) => Left(x)
    }
}

implicit val listApplicative: Applicative[List] = new Applicative[List] {

  // removed map() and now using the default

  def pure[A](a: A): List[A] = List(a)

  def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
    ff.foldRight(List[B]()) { (acc1, f) =>
      fa.foldRight(acc1) { (acc2, a) =>
        f(a) :: acc2
      }
    }
}
```
:::

`Option` and `Either`’s instances of `Applicative` are straight-forward: if a function and argument are present, they are applied and the result returned in the desired case. If either are missing, then the undesired case is propagated instead. 

`List` looks very different at first glance, but conceptually performs the same way. Specifically, `List` performs a Cartesian product of its functions and arguments, applying each together to build a new `List` from the results. If either the function or argument `List` are empty, then an empty result `List` is returned, as an empty `List` represents the undesired case.

## Applicative laws

`Option`, `Either`, and especially `List`’s `Applicative` instances look different. How do we know that they are well-behaved as applicatives? Just like functors, applicatives are expected to conform to a set of laws defined in the higher math of category theory.

There are four applicative laws, which must hold for all applicatives in addition to the functor laws.

1. **Preservation of identity functions**: A lifted identity function applied to a lifted argument is the same as the identity function applied directly to the lifted argument.
2. **Preservation of function homorphism**: Lifting a function and an argument then applying them produces the same result as applying the unlifted function and unlifted argument then lifting the result.
3. **Preservation of function interchange**: Applying a lifted function to a lifted value produces the same result as a lifted function of the unlifted function applied to the unlifted value applied to the lifted function. (What a mouthful! It's clearer in code.)
4. **Preservation of function composition**: Given lifted functions `ff: F[A => B]` and `fg: F[B => C]` and argument `fa: F[A]`: lifting `compose()` and applying `fg`, `ff`, and `fa` produces the same result as applying `fg` after applying `ff` to `fa`.

These laws are rigorous and we can write tests for these to prove that our applicative instances are defined correctly.

### Testing "for all" `F[_]`

In order to properly test our applicative instances, we need to be able to generate a broad range of inputs to verify that the applicative properties hold with a high degree of confidence. Specifically, "for all" `List`s, for example, the properties of `Applicative` must hold. Therefore we will leverage `scalacheck` for property-based testing.

Generating an arbitrary context `F[_]` containing an arbitrary `A` is not supported directly by `scalacheck`. We can leverage this typeclass to enable generating instances of `F[A]` from any generator for `A`:

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

### Defining the applicative laws as properties

Now we will walk through defining the properties of `Applicative` in such a way that we simply supply the contexts as the argument to the test. This way we only define the properties once.

:::{.numberLines}
```scala
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.matchers.must.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

trait ApplicativeLaws { this: AnyPropSpec with ScalaCheckPropertyChecks with Matchers =>

  import Applicative.Syntax._
  import LiftedGen.Syntax._

  def checkApplicativeLaws[F[_]: Applicative: LiftedGen](): Unit = {
    property("Applicative preserves identity functions") {
      forAll(arbitrary[Double].lift) { v =>
        (identity[Double] _).pure.ap(v) mustBe identity(v)
      }
    }
    property("Applicative preserves function homomorphism") {
      forAll(for {
        f <- arbitrary[Double => String]
        x <- arbitrary[Double]
      } yield (f, x)) { case (f, x) =>
        f.pure.ap(x.pure) mustBe f(x).pure
      }
    }
    property("Applicative preserves function interchange") {
      forAll(for {
        u <- arbitrary[Double => String].lift
        y <- arbitrary[Double]
      } yield (u, y)) { case (u, y) =>
        u.ap(y.pure) mustBe ((f: Double => String) => f(y)).pure.ap(u)
      }
    }
    property("Applicative preserves function composition") {
      val compose: (String => Int) => (Double => String) => Double => Int = g => f => g compose f
      forAll(for {
        u <- arbitrary[String => Int].lift
        v <- arbitrary[Double => String].lift
        w <- arbitrary[Double].lift
      } yield (u, v, w)) { case (u, v, w) =>
        compose.pure.ap(u).ap(v).ap(w) mustBe u.ap(v.ap(w))
      }
    }
  }
}
```
:::

With this trait, laws specs can be written for each context. 

:::{.numberLines}
```scala
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.matchers.must.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class OptionLawsSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers with ApplicativeLaws {

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
 
   checkApplicativeLaws[Option]()
}

class EitherLawsSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers with ApplicativeLaws {

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
 
   checkApplicativeLaws[Either[String, *]]()
}

class ListLawsSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers with ApplicativeLaws {

  import List.Instances._
 
   implicit val listLiftedGen: LiftedGen[List] = new LiftedGen[List] {
 
     override def lift[A](gen: Gen[A]): Gen[List[A]] = {
       def go(length: Int, acc: List[A]): Gen[List[A]] =
         if (length == 0) {
           Gen.const(acc)
         } else {
           gen.flatMap(a => go(length - 1, a :: acc))
         } 
       Gen.choose(0, 100).flatMap(go(_, Nil))
     } 
   }
 
   checkApplicativeLaws[List]()
}
```
:::

Try running these specs!

### Implications of the applicative laws

Each of `Option`, `Either`, and `List` conform to the applicative laws and we only had to write the properties to test them once. These properties prove that functions and arguments used within these contexts maintain referential transparency in their arrangments and that the specific contexts do not change the factoring semantics of the code.

What does change, however, are these contexts' specific effects. For example, you would not have to refactor code abstracted by applicative operations if you changed the backing implementation from `Either` to `List`, but your code would produce potentially more than one result in the desired case.

This is the goal, however, as these effects' dimensions of unknown quantity should not burden our code. Instead, we push the complexity to the edge of the context, where it is important that our context is an `Either` or a `List`.
