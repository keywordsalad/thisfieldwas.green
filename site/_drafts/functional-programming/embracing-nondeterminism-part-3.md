---
title: "Embracing Nondeterminism Part III: Imperative Programming"
author: Logan McGrath
comments: false
date: 2022-01-24T17:14:03-0800
tags: functional programming, programming, scala, design patterns
layout: post
---

<!--more-->

## Motivating Monads

Functors give you `map()` so that you may consume a term from a single context. Applicatives give you `pure()` and `ap()` so that you may consume terms from two or more contexts in parallel. But what if you want to sequence consumption of terms? Functors for example only consume from the scope of a single context, and Applicatives only consume from contexts in parallel. Neither Functors nor Applicatives allow you to alter the state of the context such that downstream operations are dependent upon previous operations succeeding.

Concretely, at some point in your program you will want to write code that operates against the result of a previous operation and decide whether to continue:

:::{.numberLines .nowrap}
```scala
val userId = session("userId")
if (!userId) {
  redirect("/login")
} else {
  val user = getUser(userId)
  if (!user) {
    redirect("/error", message = "User not found")
  } else if (!sessionIsValid(user)) {
    redirect("/login")
  } else {
    bankAccount = getBankAccount(user)
    if (!bankAccount) {
      display("/error", message = "User doesn't have bank account")
    } else if (!bankService.depositMoney(bankAccount, "$20")) {
      display("/error", message = "Failed to deposit money")
    } else {
      display("/success", message = "Got the money!")
    }
  }
}
```
:::

As you can see from the above, this code deposits $20 only if:

   1. There is a user ID present in the session.
   2. The user exists.
   3. The user has a valid session.
   4. The user has a bank account.
   5. The bank service successfully deposits money into the account.

There is a great deal of branching logic in this code that separates success and failure cases as the result of previous operations. This _imperative_ sequencing of operations requires that you manually short-circuit the program if a previous operation fails. This requirement to sequence operations is what motivates Monads.

**Monads** are an abstraction that permit the sequencing of operations. Recall that `map()` accepts a function `f: A => B` as one of its arguments. Monads are a _special case_ of Functor that arise when the term `B` in `f: A => B` is known to have the shape `F[_]`. Specifically, the term `B` itself is an instance of the same context `F[_]`. In the general case, when `map()` is applied with a function `f: A => F[B]` you will receive a nested context `F[F[B]]`. How would you specialize `map()` such that you receive `F[B]` instead?

Monads, like Functors, are a simple structure and define a single function `flatMap()` which joins a nested context with the outer context, or in other words, it _flattens_ the context after it has been mapped:

:::{.numberLines .nowrap}
```scala
def flatMap(fa: F[A])(f: A => F[B]): F[B]
```
:::

What this function enables is a capability like `map()` in that term `A` may be consumed with a provided function `f: A => F[B]` and specialized in that new contexts such as `F[B]` may be created as the result of their operation. Two key capabilities are enabled:

* _Imperative programming_ where subsequent operations are dependent upon the results of previous operations.
* _Control flow_ by exploiting the _void effect_ as Monads allow you to inject a void context to short-circuit an operation.

Armed with Monads the previous code may be rewritten using `flatMap()`:

:::{.numberLines .nowrap}
```scala
def depositTwentyBucks(): Future[DepositResponse] =
  session("userId") match {
    case None => fail(RequireUserLogin)
    case Some(userId) =>
      getUser(userId).flatMap {
        case None => fail(UserNotFound)
        case Some(user) =>
          if (!sessionIsValid(user)) {
            fail(RequireUserLogin)
          } else {
            getBankAccount(user).flatMap {
              case None =>
                fail(BankAccountNotFound)
              case Some(bankAccount) =>
                bankService.depositMoney(bankAccount, "$20")
            }
          }
      }
```
:::

This code seems a bit hard on the eyes, doesn't it? The deep _V_ shape of the code is referred to as the _pyramid of death_ and is an unfortunate feature of `flatMap()`-heavy code. There is still branching logic as two contexts, `Option[_]` and `Future[_]`, are being used simultaneously. However, Monads and Monad-like structures are so common in Scala that there is a special syntax for this kind of code, and with a little refactoring the code may be brought into a flattened layout:

:::{.numberLines .nowrap}
```scala
def depositTwentyBucks(): Future[DepositResponse] =
  for {
    userId <- someOrElse(session("userId"), fail(RequireUserLogin))
    user <- getUser(userId).flatMap(someOrElse(_, fail(UserNotFound)))
    _ <- cond(sessionIsValid(user), (), fail(RequireUserLogin))
    bankAccount <- getBankAccount(user).flatMap(someOrElse(_, fail(BankAccountNotFound)))
    result <- bankService.depositMoney(bankAccount, "$20")
  } yield result

def someOrElse[F[_]: Applicative, A](opt: Option[A], defaultCtx: => F[A]): F[A] =
  opt match {
    case Some(x) => Applicative[F].pure(x)
    case None    => defaultCtx
  }

def cond[A](success: Boolean, trueCase: => A, falseCase: => A): A =
  if (success) trueCase
  else falseCase
```
:::

This code is markedly different, and demonstrates a few features:

* Each logical instruction is on its own line and branching logic has practically disappeared from the primary operation.
* Each instruction is dependent upon the previous instruction succeeding and the entire operation short-circuits on failure.
* Concretely the Monad being used here is a `Future[_]`, which is asynchronous. At no point in the code is complexity imposed by using asynchronous operations.
* Explicit branching is moved to dedicated generalized functions which provide mechanisms for injecting different contexts by condition.
* Instances of the `Option[_]` context are destroyed in order to advance logic. These specific examples demonstrate a valid use case for destroying contexts.

How does this code look to you?

### Becoming a Monad

Like Functor and Applicative, each context must provide their own implementation of `flatMap()` in order for it to be used as a Monad. Any type with the shape `F[_]` may become a Monad by implementing the following typeclass:

:::{.numberLines .nowrap}
```scala
trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  // Monad instances get join() for free!
  def join[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(fa => fa)

  // Monad instances get ap() for free!
  override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    flatMap(ff)(f => map(fa)(f))
}
object Monad {
  def apply[F[_]: Monad]: Monad[F] = implicitly[Monad[F]]
}
object MonadSyntax {
  implicit class MonadOps[F[_], A](fa: F[A]) extends AnyVal {
    def flatMap[B](f: A => F[B])(implicit F: Monad[F]): F[B] =
      F.flatMap(fa)(f)
  }
}
```
:::

Notice that Monads build on top of Applicatives. In the above typeclass, `join()` is provided a default implementation in terms of `flatMap()`, and `ap()` in terms of `flatMap()` and `map()`. Instances may override these functions if they choose, and in particular must override either `map()` or `ap()`. Our Applicative instances from before may now be upgraded to Monads:

:::{.numberLines .nowrap}
```scala
object MonadInstances {
  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    def pure[A](a: A): Option[A] = Some(A)
    def map[A, B](fa: Option[A])(f: A => B): F[B] =
      fa match {
        case Some(a) => Some(f(a))
        case None => None
      }
    def flatMap[A](fa: Option[A])(f: A => F[B]): Option[B] =
      fa match {
        case Some(a) => f(a)
        case None => None
      }
  }
  implicit def eitherMonad[X]: Monad[Either[X, *]] = new Monad[Either[X, *]] {
    def pure[A](a: A): Either[X, A] = Right(a)
    def map[A, B](fa: Either[X, A])(f: A => B): F[B] =
      fa match {
        case Right(a) => Right(f(a))
        case Left(x) => Left(x)
      }
    def flatMap[A, B](fa: Either[X, A])(f: A => Either[X, B]): Either[X, B] =
      fa match {
        case Right(a) => f(a)
        case Left(x) => Left(x)
      }
  }
  implicit val listMonad: Monad[List] = new Monad[List] {
    def pure[A](a: A): List[A] = List(a)
    def map[A, B](fa: List[A])(f: A => B): F[B] =
      fa match {
        case a :: ta => f(a) :: map(ta)(f)
        case Nil => Nil
      }
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
      fa match {
        case a :: ta => f(a) ++ map(ta)(f)
        case Nil => Nil
      }
  }
}
```
:::
