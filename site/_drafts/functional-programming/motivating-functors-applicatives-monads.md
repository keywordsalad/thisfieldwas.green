---
title: Motivating Functors, Applicatives, and Monads
author: Logan McGrath
comments: false
date: 2022-01-24T17:14:03-0800
tags: functional programming, programming, scala
layout: post
---

What’s a Functor? An Applicative? Or a Monad? The internet is teeming with articles that answer some facet of their character, but none really provide a concrete motivation for why these structures exist or appear in the forms that they do.

In this article, I will demonstrate the motivation of Functors, Applicatives, and Monads.

The code that accompanies this post may be found [here]().

## Conventions

I will provide Scala code for concrete examples.

Terminology will leverage common functional programming vocabulary.

Where there is conceptual overlap with object oriented programming, I will leverage those terms to drive the intent behind abstractions.

## Terminology

**Expressions** are values that are described by some type A.

**Functions** are a _special case_ of expressions that map some type A to some type B, such that they are described by A => B or "A to B".

**Terms** are unitary expressions that are composed by no more than an identifier. For example, in the declaration map(fa: F[A])(f: A => B) the terms are fa and f. Term may also describe a type named by a variable as these types are also unitary, such as the argument A in F[A].

**Lifting** describes any term A becoming wrapped in a context F such that A => F[A]. A **lifted** term or expression already has the form F[A].

**Composition** describes chaining the output of a function f: A => B to the input of function g: B => C such that a new function h: A => C may defined as h := g . f or "g after f". This alternative definition "h(x) = g(f(x))" explicitly demonstrates applying the function g after applying function f to argument x.

**Side effects** occur when a mapping of A => B maps to a different member of type B for any number of times the same member of A has been mapped to B. This means that A => B is nondeterministic. An extreme example of such a function is a random number generator rng: () => Int as it maps solitary unit () to all members of type Int if applied enough times. Reading from a file can be driven by side effects as the contents of the file may change, and interacting with any system over the network implies changing these systems’ states, as in databases and third-party API’s. Cases such as "divide by zero" and "Exceptions" constitute a secondary output of a function and are treated as side effects as they impose an additional layer of protection to prevent or recover from them.

## Functions and Complexity

_Functions are the backbone of functional programming._

Given a function f: A => B and another g: B => C: a third function h: A => C may be composed of g . f, read as "g after f". Programs may be modeled as a function f: A => B, where f is composed of enumerable smaller functions, building the necessary mappings to generate the desired program output.

While the interface of a program may be modeled as an input mapped to an output, many functions internally must interact with implicit inputs and outputs. An employee payroll system for example must make database queries and integrate with banks. These implicit inputs and outputs have effects that dictate how they are received and sent, as database queries may return non deterministic responses and an error might occur when performing a direct deposit. 

Effects impose complexity on code. Can you think of how complexity occurs in a server application?

* Configuration comes from any number of sources, both when starting the application and during runtime. 
* Database queries each need to be protected against errors and checked if any occur. Each type requires different handling.
* External API calls will respond in an indeterminate amount of time, if at all.
* Transforming API responses is fraught with surprises. No data coming from an external source is safe.
* Errors need to be logged.
* Metrics need to be gathered.

How is this complexity handled?

* Configuration may be read when starting from the environment or from config files. At runtime a database might be used.
* Database queries are surrounded by exception handling, and only handled insofar as the compiler requires.
* API calls require asynchronous IO.
* API responses are validated and transformed into domain objects.
* Logging libraries are used to track errors, and might require file system or network access.
* Metrics libraries are used and require network access.

These complexities can be described in terms of effects. Each of the following effects center on some dimension of nondeterminism:

* Time and Async: API calls and network
* IO: File and network
* Presence or absence
* Length: Database queries
* Correct or incorrect: Input validation
* Implicit input: Configuration
* Implicit output: Logging and metrics
* State: Change over time

The actual list of effects is enumerable, but these ones are common. Fortunately there are ways to model subsets and contain the scope of their impact so that code is less complex. 

Previously I described function composition as a simple case of h: g . f. Functors, Applicatives, and Monads address a particular kind of function composition, the composition of functional effects, which I will demonstrate in the following sections.

## Contexts and Effects

Let me start with a vague term: _Context_. What is a context? A context is a setting within which some term A might be produced. Contexts in Scala may be neatly represented by a letter and brackets such as F[_] when the type of the term is unknown, and F[A] when the term is known to be of type A. Other letters work nicely of course, as do proper names, as in Option[Int].

Each kind of context carries with it a set of effects. These effects are modeled by specific contexts as if they were gremlins pulling at levers that determine how their terms may be produced. Names of contexts can hint at the effects they model, and with some intuition you may be able to figure out what each context’s effects may be.

Here are some common contexts and some of their effects:

Some easy ones:

* Option[A]: Presence or absence of term A.
* Either[A, B]: Presence of term B if valid, term A if invalid (B is the term you want).
* List[A]: Nondeterminism of sort, cardinality, and length of term A.

Two hard ones:

* Future[A]: Asynchronous and temporal nondeterminism of term A.
    * Term A may be here already, at some point in the future, or may never arrive at all.
    * Term A must be acquired from the outside world via processes that rely on side effects.
    * External factors may interrupt the acquisition of term A.
* IO[A]: External nondeterminism of term A.
    * Term A may be here already, eventually, or never.
    * Term A must be acquired from the outside world via processes that rely on side effects. 
    * External factors may interrupt the acquisition of term A.

Some of the specialized ones:

* ReaderT[F, A]: Reading of nondeterministic implicit inputs.
    * Propagation of configuration usually leverages this effect.
* WriterT[F, A]: Writing of nondeterministic implicit outputs.
    * Logging usually leverages this effect.
* StateT[F, A]: Nondeterministic implicit inputs and outputs.
    * Models state changing over time.

Each of these contexts have two shared qualities in that they produce some term A and their effects dictate how term A is produced. But with such a wide array of effects, and with so little overlap between each context, how can A be consumed in a general manner?

In order to generalize contexts, the key differentiator between them must be abstracted: effects. By shedding effects as an implementation detail, the consumption of a term A becomes a shared interface. How is that interface exposed?

Motivating Functors

For any context F[_], it produces some term A. If you have a function A => B, how would you apply it to a term produced by the context? That would require extracting the term, right? Specifically, you can’t do this: 

(pseudo code)
val fa: F[A]
def f(x: A): B = ???
f(fa) // compile error!

Recall from the previous section, contexts only share two qualities: that they might produce a term, and that they have effects. After abstracting effects, contexts do not expose an obvious shared interface to extract the term.  Consider the following definitions for Option, Either, and List:

sealed trait Option[+A] {
  def get: A
  def isNone: Boolean = this == None
  def isSome: Boolean = !isNone
}
case object None extends Option[Nothing] {
  def get: A = throw new Exception()
}
case class Some[A](get: A) extends Option[A]

sealed trait Either[A, B] {
  def getLeft: A = this match {
    case Left(x) => x
    case Right(_) => throw new Exception()
  }
  def getRight: B = this match {
    case Left(_) => throw new Exception()
    case Right(y) => y
  }
  def isLeft: Boolean = this match {
    case Left(_) => true
    case Right(_) => false
  }
  def isRight: Boolean = !isLeft
}
case class Left[A, B](getLeft: A) extends Either[A, B]
case class Right[A, B](getRight: B) extends Either[A, B]

sealed trait List[+A] {
  def isNil: Boolean = this == Nil
  def head: A
  def tail: List[A]
}
case object Nil extends List[Nothing] {
  def head: Nothing = throw new Exception()
  def tail: Nothing = throw new Exception()
}
case class ::[A](head: A, tail: List[A]) extends List[A]

Both Option and Either have roughly the same shape in that there either is or isn’t an instance of the desired term. Because of this, an extract(): F[A] => B operation is possible as it means the same thing between both of them, and extracting fails if there’s no term. In object oriented programming, Option and Either might share such an interface:

trait Extractable[A] {
  def extract(): A
}
sealed trait Option[A] extends Extractable[A] {
  // …
  override def extract(): A = get
}
sealed trait Either[A, B] extends Extractable[B] {
  // ..
  override def extract(): B = getRight
}

But what about a List?

What does it mean to extract() the term A from a List such that it means the same thing as in Option and Either? As in Option and Either there is a notion of presence and absence, but presence implies one to many instances of term A and not just one. A solution inspired by object oriented programming might change the interface thusly:

trait Extractable[A] {
  def extract(): Seq[A]
}
sealed trait Option[A] extends Extractable[A] {
  // …
  override def extract(): Seq[A] = Seq(get)
}
sealed trait Either[A, B] extends Extractable[B] {
  // ..
  override def extract(): Seq[B] = Seq(getRight)
}
sealed trait List[A, B] extends Extractable[A] {
  // …
  override def extract(): Seq[A] = this
}

This interface however is not coherent. Absence as a behavior is preserved in Option and Either, but in a list extract() could return an empty list and whether that can be interpreted as absence is ambiguous. This interface also imposes the effects of List upon all client code using it. You would probably be very unhappy using this interface in your own code.

This interface also doesn’t make sense when trying to apply it to Future: an extract() function is by its own signature a blocking call, and you probably want something that is properly asynchronous.

Option, Either, List, Future, and IO all have different effects that dictate how term A might be produced. An axiom from object oriented programming would be to abstract what changes. Thus you have to shed the effects as implementation details. How might that impact extracting the term A?

The answer: extraction cannot be generalized. All you know is that there a some term A. So how would you consume term A?

There is a way to consume the term by using an abstraction called a Functor. What is a Functor? A Functor is a simple structure, a single function called map():

def map(fa: F[A])(f: A => B): F[B]

What map() does is _lift_ the function A => B into the context so that it becomes F[A] => F[B], giving back F[B].

This lifting of functions that map() performs is coherent across contexts. You can apply A => B to a List just as easily as you can an IO and the results of both operations are predictable. Your List[A] becomes List[B] and your IO[A] becomes IO[B].

How would you consume the term produced by a Future or an Option? You would also use a Functor.

What this enables is your function A => B to be used with any Functor regardless of its specific effects. Your function A => B is immediately reusable, and this means is that you already know how to use other Functors.

### Why does map() return F[B]?

Recall that contexts generally do not permit extracting terms. Think for a moment: what does extracting a term mean if you’re using a context like Option? What about Future? Would their effects change how extraction of a term would work? Lists contain more than one instance of a term, after all!

Because there is no way to generalize extracting a term from a context, Functors don’t allow you to operate on contexts in such a way that the term can "escape" them. Extracting terms is implementation-specific, so this capability is not generalized. 

Most importantly, by keeping all operations against terms within the context, the context’s specific effects remain abstracted. Asynchronous operations with Future remain asynchronous, the length of a List remains nondeterministic, and an Option may or may not do anything.

### F[A] must produce some term A

I stated above: _"For any context F[_], it must produce some term of type A."_ If the context were guaranteed to have an instance of a term of type A then you should be able to consume it with your function A => B, right?

But what if there’s nothing there, as in there are zero instances of a term of type A? Can you do anything? When a context has this kind of effect, a sort of "nothing here" or _void_ effect, then the map() function above doesn’t do anything because there isn’t anything to do. If you try to map() an F[A] with A => B then it returns a void F[B] as there’s "nothing here". It also does this without having used A => B to get there.

This behavior is referred to as _short-circuiting_ and it is a key feature of all Functors, Applicatives, and Monads. It is exploited in particular to enable control flow and error handling, which I will expand on later.

Option and Either are two prime examples of short-circuiting in Functors. An Option will only map() if its term is present, and an Either will only map if its term is valid.

### Specific examples of Functors

Each context of course must provide its own implementation of map() in order for it to be a Functor. Below I outline some examples against a definition of Functor:

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B)
}
object Functor {
  def apply[F[_]](implicit F: Functor[F]) = F
}
object FunctorImplicits {
  implicit class FunctorOps[F[_], A](val fa: F[A]) extends AnyVal {
    def map[A](f: A => B)(implicit F: Functor[A]): F[B] = F.map(fa)(f)
  }

  implicit val optionFunctor: Functor[Option] =
    new Functor[Option] {
      def map[A, B](fa: Option[A])(f: A => B): Option[B] =
        fa match {
          case Some(x) => Some(f(x))
          case None => None
        }
    }

  implicit def eitherFunctor[X]: Functor[Either[X, *]] =
    new Functor[Either[X, *]] {
      def map[A, B](fa: 
}

def map(fa: Option[A])(f: A => B): Option[B] =
  fa match {
    case Some(x) => Some(f(x))
    case None => None // void
  }

def map(fa: Either[E, A])(f: A => B): Either[E, B] =
  fa match {
    case Right(x) => Right(f(x))
    case Left(e) => Left(e) // void
  }

def map(fa: List[A])(f: A => B): List[B] =
  fa match {
    case x :: tail => f(x) :: map(tail)(f)
    case Nil => Nil // void
  }

Each of these show remarkable similarities, and this isn’t uncommon across data structures. Note in particular how Lists are recursive, with the base case representing void. Functor implementations are more complex in contexts such as Futures and IO because they are working with complex state and side effects.

Can you see how Functors enable control flow and short-circuiting? The void cases are the specific branches of logic that enable these. If there’s "nothing here", then they don’t do anything. In the specific case of Either, Left may be used to carry error state as it’s able to carry its own data. Left being able to carry its own data is one of Either’s specific effects.

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