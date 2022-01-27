---
title: Motivating Functors, Applicatives, and Monads
author: Logan McGrath
comments: false
date: 2022-01-24T17:14:03-0800
tags: functional programming, programming, scala
layout: post
---

What’s a Functor? An Applicative? Or a Monad? The internet is teeming with articles that answer some facet of their character, but few provide a concrete motivation for why these structures exist or appear in the forms that they do.

In this article, I will demonstrate the motivation of Functors, Applicatives, and Monads.

The code that accompanies this post may be found [here]().

## Conventions

I will provide Scala code for concrete examples.

Terminology will leverage common functional programming vocabulary.

Where there is conceptual overlap with object oriented programming, I will leverage those terms to drive the intent behind abstractions.

### Terminology

**Expressions** are values that are described by some type `A`.

**Functions** are a _special case_ of expressions that map some type `A` to some type `B`. They are described by `f: A => B`, read as _f is A to B_.

**Terms** are unitary expressions that are composed by no more than an identifier. For example, in the declaration `map(fa: F[A])(f: A => B)` the terms are `fa` and `f`. **Term** also describes a type named by a variable as these types are also unitary, such as the type argument `A` in `F[A]`.

**Lifting** describes any term `A` becoming wrapped in a context `F` such that `A => F[A]`. A **lifted** term or expression already has the form `F[A]`.

**Composition** describes chaining the output of a function `f: A => B` to the input of function `g: B => C` such that a new function `h: A => C` may defined as `h := g ∘ f`, read as _h is g after f_.

* This alternative notation in Scala `def h(x: A): C = g(f(x))` explicitly defines `h` as an application of the function `g` _after_ an application of the function `f` to argument `x`.

**Nondeterminism** occurs when a function `f: A => B` maps to a different member of type `B` for any number of times the same member of `A` has been applied. This means that `f: A => B` is driven by **side effects** that occur independent of the signature of the function.

* An extreme example of a nondeterministic function is a random number generator `rng: () => Int` as it maps the solitary unit value `()` to all members of type `Int`. This mapping is influenced by some side effect _external to the function's signature_.
* Reading from a file is driven by side effects in the same way, as the contents of the file may change between reads.
* Interacting with any system over the network implies affecting and being affected by these systems’ states, as in databases and third-party API’s.
* Side effects also include **faults** such as the _divide by zero error_, thrown exceptions, and panics as they constitute secondary **implicit outputs** of a function. They impose an additional layer of protection to prevent or recover from them.
* Exceptions and panics are fully nondeterministic as there is no single input that guarantees that an exception will never be thrown, as some secondary **implicit input** influences the outcome. In contrast, a _division by zero error_ only occurs if the input divisor is `0`; it normally is not treated as a side effect for this reason.

## Functions and Complexity

### Functions are the backbone of functional programming

Given a function `f: A => B` and another `g: B => C`: a third function `h: A => C` may be composed of `h := g ∘ f` or _h is g after f_. Programs may be modeled as a function `prog: A => B`, where `prog` is composed of innumerable smaller functions, building the necessary mappings to generate the desired program output.

While the interface of a program may be modeled as an input mapped to an output, many functions internally must interact with implicit inputs and outputs _not present in the program's signature_ of `prog: A => B`. An employee payroll system for example must make database queries and integrate with banks. These _implicit inputs and outputs_ have **effects** that dictate how they are received and sent: database queries may return non deterministic responses and an error might occur when performing a direct deposit.

### Effects impose complexity on code.

Can you think of how complexity occurs in a server application?

* Configuration comes from any number of sources, both when starting the application and during runtime.
* Database queries each need to be protected against errors and checked if any occur. Each type requires different handling.
* External API calls will respond in an indeterminate amount of time, if at all.
* Transforming API responses is fraught with surprises. No data coming from an external source is safe.
* Errors need to be logged.
* Metrics need to be gathered.

How might complexity appear in code?

* Configuration may be read when starting from the environment or from config files. At runtime a database might be used.
* Database queries are surrounded by exception handling, and only handled insofar as the compiler requires.
* API calls require asynchronous IO.
* API responses are validated and transformed into domain objects.
* Logging libraries are used to track errors, and might require file system or network access.
* Metrics libraries are used and require network access.

These complexities can be described in terms of effects. Each of the following effects center on some dimension of nondeterminism:

* **Time and Async**: API calls and network
* **IO**: File and network
* **Presence or absence**
* **Length**: Database queries
* **Correct or incorrect**: Input validation
* **Implicit input**: Configuration
* **Implicit output**: Logging and metrics
* **State**: Change over time

The actual list of effects is innumerable, but these ones are common. Fortunately there are ways to model subsets and contain the scope of their impact so that code is less complex.

Previously I described function composition as a simple case of `h := g ∘ f`. Functors, Applicatives, and Monads address a particular kind of function composition, **composition of functional effects**, which I will demonstrate in the following sections.

## Contexts and Effects

Let me start with a vague term: **Context**. What is a context? A context is a setting within which some term `A` might be produced.

> Contexts in Scala may be neatly represented by a letter and brackets such as `F[_]` when the type of the term is unknown, and `F[A]` when the term is known to be of type `A`. Other letters work nicely of course, as do proper names, as in `Option[Int]`.

Each kind of context carries with it a set of _effects_. Effects are modeled by specific contexts that determine how their terms may be produced. Names of contexts can hint at the effects they model, and with some intuition you may be able to figure out what each context’s effects may be.

### Common contexts and some of their effects

**Contexts representing presence:**

* `Option[A]`: Presence or absence of some instance of term `A`.
* `Either[A, B]`: Conventionally treated as term `B` if valid, term `A` if invalid (`B` is the term you want).
* `List[A]`: Nondeterminism of sort, cardinality, and length of term `A`.

**Contexts representing acquisition:**

* `IO[A]`: External nondeterminism of term `A`.
    * Term `A` may be here already, eventually, or never.
    * Term `A` must be acquired from the outside world via processes that rely on side effects.
    * External factors may interrupt the acquisition of term `A`.
    * Execution must await term `A` before it continues.
* `Future[A]`: Asynchronous and temporal nondeterminism of term `A`.
    * Term `A` may be here already, at some point in the future, or may never arrive at all.
    * Term `A` must be acquired from the outside world via processes that rely on side effects.
    * External factors may interrupt the acquisition of term `A`.
    * Execution may be suspended, awaited, or acquire in parallel of any number of other terms.

**Higher-order contexts:**

* `ReaderT[F, A]`: Reading of implicit inputs in the context of `F`.
    * Propagation of configuration usually leverages this effect.
* `WriterT[F, A]`: Writing of implicit outputs in the context of `F`.
    * Logging usually leverages this effect.
* `StateT[F, A]`: Modifying implicit inputs and outputs in the context of `F`.
    * Models state changing over time.

Each of these contexts have two shared qualities in that they _produce some term `A`_ and their effects dictate _how term `A` is produced_. But with such a wide array of effects, and with so little overlap between each context, how can `A` be consumed in a manner unburdened of complexity?

In order to generalize contexts, the key differentiator between them must be abstracted: **effects**. By shedding effects as an _implementation detail_, the consumption of a term `A` becomes a shared capability. How is that interface exposed?

## Motivating Functors

**For any context `F[_]`, it produces some term `A`.** If you have a function `f: A => B`, how would you apply it to a term produced by the context `F[A]`? That would require extracting the term, right? Specifically, you can’t apply the function directly to the context:

```{.scala .numberLines}
// (pseudo code)
// given a context producing term A
val fa: F[A]
// given a function A => B
def f(x: A): B
// try to apply the function
f(fa) // compile error!
```

Recall from the previous section, contexts only share two qualities: that they produce a term, and that they have effects. After abstracting effects, contexts do not expose an obvious shared interface to extract the term. Consider the following definitions for `Option`, `Either`, and `List`:

**`Option[A]`** models the presence or absence of an instance of term `A`:

```{.scala .numberLines}
sealed trait Option[+A] {
  def get: A = throw new Exception()
  def isSome: Boolean = !isNone
  def isNone: Boolean = this == None
}
case class Some[A](override val get: A) extends Option[A]
case object None extends Option[Nothing]
```

**`Either[A, B]`** (by convention) models failure with term `A` and success with term `B`. Term `B` is the instance you want:

```{.scala .numberLines}
sealed trait Either[A, B] {
  def left: A = this match {
    case Left(x) => x
    case Right(_) => throw new Exception()
  }
  def right: B = this match {
    case Left(_) => throw new Exception()
    case Right(y) => y
  }
  def isRight: Boolean = !isLeft
  def isLeft: Boolean = this match {
    case Left(_) => true
    case Right(_) => false
  }
}
case class Right[A, B](override val right: B) extends Either[A, B]
case class Left[A, B](override val left: A) extends Either[A, B]
```

**`List[A]`** models zero or more instances of term `A`, with an unknown sort and cardinality:

```{.scala .numberLines}
sealed trait List[+A] {
  def isNil: Boolean = this == Nil
  def head: A = throw new Exception
  def tail: List[A] = throw new Exception
}
case class ::[A](override val head: A, override val tail: List[A]) extends List[A]
case object Nil extends List[Nothing]
```

### Extracting the instance of the term `A`

Both `Option[A]` and `Either[X, A]` have roughly the same shape in that there either is or isn’t an instance of the desired term `A`. Because of this, an operation of the form `extract(): F[A] => A` is possible as it means the same thing between both of them: `extract()` either gets the existing instance of the term `A` or it fails. In object oriented programming, `Option[A]` and `Either[X, A]` might share such an interface:

```{.scala .numberLines}
trait Extractable[A] {
  def extract(): A
}
sealed trait Option[A] extends Extractable[A] {
  override def extract(): A = get
}
sealed trait Either[X, A] extends Extractable[A] {
  override def extract(): A = right
}
```

### Extracting an unknown length of instances

What does it mean to `extract()` the term `A` from a `List[A]` such that it means the same thing as in `Option[A]` and `Either[X, A]`?

As in `Option[A]` and `Either[A]` there is a notion of the presence or absence of an instance of the term `A`, but presence in `List[A]` implies one or more instances. A solution inspired by object oriented programming might change the interface thusly:

```{.scala .numberLines}
trait Extractable[A] {
  def extract(): Seq[A]
}
sealed trait Option[A] extends Extractable[A] {
  override def extract(): Seq[A] = Seq(get)
}
sealed trait Either[X, A] extends Extractable[A] {
  override def extract(): Seq[A] = Seq(right)
}
sealed trait List[A] extends Extractable[A] {
  override def extract(): Seq[A] = this.toSeq
}
```

This interface however is _not coherent_. Absence as a behavior is preserved in `Option[A]` and `Either[X, A]`, but in `List[A]` however `extract()` could return an empty `Seq[A]` and whether that can be interpreted as absence is ambiguous. This interface also imposes the effects of `List[A]` upon all client code using it. You would probably be very unhappy using this interface in your own code.

When applied to `Future[A]`, the `extract()` function is by its own signature a blocking call. You probably want something that is properly asynchronous.

### Abstracting over effects

`Option`, `Either`, `List`, `Future`, and `IO` all have different effects that dictate how term `A` is produced. An axiom from object oriented programming would be to abstract what changes. Therefore you have to shed effects as implementation details. How might that impact extracting the term `A`?

The answer: _extraction cannot be generalized_. All you know is that there is term `A`. How would you consume term `A` if it can't be extracted?

**Functors** are an abstraction that allow you to consume term `A`. A Functor is a simple structure, a single function called `map()`:

```{.scala .numberLines}
def map(fa: F[A])(f: A => B): F[B]
```

What `map()` does is _lift_ the function `f: A => B` into the context so that it becomes `F[A] => F[B]`, giving back `F[B]`.

This _lifting_ of functions that `map()` performs is _coherent_ across contexts. You can apply `f: A => B` to any `List[A]` just as you can an `IO[A]` and the results of both operations are predictable. Your `List[A]` maps to `List[B]` and your `IO[A]` maps to `IO[B]`.

How would you consume the term produced by `Future` or `Option`? You would also use a Functor.

What this enables is your function `f: A => B` to be used with any Functor regardless of its specific effects. Your function `f: A => B` is immediately reusable, and this means is that you already know how to use other Functors.

### Why does the Functor's `map()` return `F[B]`?

Recall that contexts generally do not permit extracting terms. Think for a moment: what does extracting a term mean if you’re using a context like `Option`? What about `Future`? Would their effects change how extraction of a term would work?

Extracting _the_ term from `List` flatly doesn't make sense as it has the effect of an unknowm number of instances.

Because there is no way to generalize extracting a term from a context, Functors don’t allow you to operate on contexts in such a way that the term can "escape" them. Extracting terms is implementation-specific, so this capability is not generalized.

Most importantly, by keeping all operations against terms within the context, the context’s specific effects remain abstracted. Asynchronous operations with `Future` remain asynchronous, the length of `List` remains nondeterministic, and `Option` may or may not be present.

Functors _preserve structure_ by keeping operations within the context. For example, applying `map()` on a `List[A]` or `BinaryTree[A]`:

```{.text .nowrap .numberLines}
[1, 2, 3, 4] -> map (*2) -> [1, 4, 6, 8]

      4                           8
    /   \                       /   \
   2     6   -> map (*2) ->   4       12
  / \   / \                  / \     /  \
 1   3 5   7                1   6  10    14
```

In both cases, the output of `map()` produces an identifiable `List[B]` and `BinaryTree[B]`. The values internally may change, as they have been mapped over by a function, and `BinaryTree[B]` specifically may rebalance itself. What matters here is that the structures are coherent and identifiable.

Compare with iteration using a `for` loop:

```{.text .nowrap .numberLines}
[1, 2, 3, 4] -> for(x) -> x={1, 2, 3, 4}

      4
    /   \
   2     6   -> for(x) -> x={1, 2, 3, 4, 5, 6, 7}
  / \   / \
 1   3 5   7
```

Iteration thus _destroys structure_. In order to get a `List[B]` back you would have to rebuild it yourself and any guarantees are purely manual. The term _procedural_ describes the steps required to preserve or create structure from this operation.

This isn't to say that functional programming is only about iteration and loops. Can you think of other operations that might destroy structure? For example if you use an `await()` operation on a `Future` you will destroy its asynchronous structure and potentially harm the performance of your application.

### Context `F[A]` must produce some term `A`

I stated above: _"For any context `F[_]`, it produces some term `A`."_ If a context were guaranteed to have an instance of a term `A` then you should be able to consume it with your function `f: A => B`, right?

But what if there’s nothing there, as in there are _zero instances_ of term `A`? Can you do anything? When a context has this kind of effect, a sort of "nothing here" or _void_ effect, then the `map()` function above doesn’t do anything because there isn’t anything to do. If you try to `map()` an `F[A]` with `f: A => B` then it returns a void `F[B]` as there’s "nothing here". It does this without having used `f: A => B` to get there.

This behavior is referred to as _short-circuiting_ and it is a key feature of all Functors, Applicatives, and Monads. It is exploited in particular to enable _control flow_ and _error handling_, which I will expand on later.

`Option` and `Either` are two prime examples of short-circuiting in Functors. An `Option[A]` will only `map()` an instance of its term is present, and an `Either[A, B]` will only `map()` if an instance of the desired term `B` is present.

### Becoming a Functor

Each context of course must provide its own implementation of `map()` in order for it to be a Functor. Below I outline some examples against a definition of Functor:

```{.scala .numberLines}
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

implicit val optionFunctor = new Functor[Option] {
  def map[A, B](fa: Option[A])(f: A => B): Option[B] =
    fa match {
      case Some(x) => Some(f(x)) // apply map
      case None => None // void
    }
}

implicit def eitherFunctor[X] = new Functor[Either[X, *]] {
  def map[A, B](fa: Either[X, A])(f: A => B): Either[X, B] =
    fa match {
      case Left(x) => Left(x) // void
      case Right(y) => Right(f(y)) // apply map
    }
}

implicit val listFunctor = new Functor[List] {
  def map[A, B](fa: List[A])(f: A => B): List[B] =
    fa match {
      case head :: tail => f(head) :: map(tail)(f) // apply map, recurse
      case Nil => Nil // void
    }
}
```

Each of these show remarkable similarities, and this isn’t uncommon across Functors for most data structures. Note in particular how `List` is recursive, with the base case `Nil` representing void. Functor implementations are more complex in contexts such as `IO` and `Future` because they are managing side effects.

Can you see how Functors enable control flow and short-circuiting? The void cases are the specific branches of logic that enable these. If there’s "nothing here", then they don’t do anything. In the specific case of `Either[X, A]`, `Left` may be used to carry error state in its term `X`. `Left` being able to carry its own term is one of `Either`’s specific effects.

Functors allow for the following function definitions:

EXAMPLES

## Motivating Applicatives

Functors permits you to consume a term via map():

def map(fa: F[A])(f: A => B): F[B]

Consider for a moment: with a Functor you are able to work within the scope of a single term within a context. But what happens if you have two contexts and you want to operate on the terms from both?

Take for example these two contexts and the function signature for product:

val fa: F[A]
val fb: F[B]

def product(a: A)(b: B): (A, B)

How do you apply product() to the terms produced by the two contexts? This is what motivates Applicatives.

Like a Functor, an Applicative is also a structure. It requires a Functor, and defines two more functions.

A constructor with which an term may be lifted into a context:
* def pure(a: A): F[A]

A function that is able to apply a function lifted into a context to a lifted term:
* def ap(ff: F[A => B])(fa: F[A]): F[B]

These two functions together enable sequencing the consumption of terms so that their products may be worked with together. I will demonstrate how this works by defining these for Option:

def pure(a: A): Option[A] =
  Some(a)

def ap(ff: Option[A => B])(fa: Option[A]): Option[B] =
  ff match {
    case Some(f) => fa match {
      case Some(x) => Some(f(x))
      case None => None
    case None => None
  }

And then to define product():

def product(fa: F[A])(fb: F[B]): F[(A, B)] =
    ap(ap(pure(x => y => (x, y)))(fa))(fb)

### Motivating Monads

Functors give you map() so that you may consume a term from a single context. Applicatives give you pure() and ap() so that you may consume terms from two or more contexts. But what if one of your functions returns a context itself? You can use A => B for either map() or with ap() if you lift it with pure(). But what if you’re function is A => F[B]?

## Functors, Applicatives, and Monads: A philosophical perspective

Above I referred to the term borne by a context as a monad in a philosophical sense. I think this is a key concept that no Monad tutorial I’ve read online had made a connection with: that when we work with Monads we only care about the singular A that they produce, or another way to look at it is as the one A that we’re currently operating on. Specifically I call this out because what I’m going to outline reinforces the Monad as a concept and hopefully wraps the name in a little meaning.

If I’m operating in some context of code that is a Monad by type, and it has also been abstracted such that I only know that the code is a Monad and nothing more, then I really don’t have any reason to believe that I am operating on anything other than a singular, indivisible A. It does not matter that the code when called is always reified using a List as its context, as the code itself still operates as though it were operating on just that one A.

The Monad is singular and indivisible. It is the sole focus of my code when I use it, and any context around it is purely abstract. I worry only about the A as everything else is circumstantial.

And if there’s "nothing here"? It doesn’t matter, and I don’t have to worry because that code isn’t running anyway.
