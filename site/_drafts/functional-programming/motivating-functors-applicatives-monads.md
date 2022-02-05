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

While the interface of a program may be modeled as an input mapped to an output, many functions internally must interact with implicit inputs and outputs _not present in the program's signature_ of `prog: A => B`. An employee payroll system for example must make database queries and integrate with banks. These _implicit inputs and outputs_ have **effects** that dictate how they are received and sent. For example database queries may return non deterministic responses and an error might occur when performing a direct deposit. 

Faults, errors, and nondetermism as effects of these operations are effectively opaque in functions modeled as simple input and output, as in `getUser: Int => User`. The signature of this function requires a sort of _tribal knowledge_ in order for the programmer to be aware of what effects may dictate how a `User` is retrieved:

* An associated `User` may not be found.
* The returned `User` may change between function applications.
* The database or network may fault and the function generates an exception that must be handled.

You might be thinking that these cases are a given when working with database code, and that _is_ tribal knowledge. These cases are _effects_ that dictate the circumstances under which a `User` might be produced and can be modeled accordingly as part of the typed API of `getUser`.

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

The actual list of effects is innumerable, but these ones are common.

### Demonstrating effects in code

Take for example this code from a hypothetical payroll system:

```{.java .numberLines}
EmployeeRepo employeeRepo;
BankAchClient achClient;
PayCalculator payCalc;

bool runPayroll(long employeeId) {
    Employee employee = employeeRepo.find(employeeId)
    if (employee == null) {
        Logger.error("Missing employee " + employeeId.toString());
        return false;
    }
    Paycheck paycheck = payCalc.calculatePaycheck(employee);
    if (paycheck == null) {
        Logger.error("No paycheck for " + employeeId.toString());
        return false;
    }
    String companyAcctNo = PayrollConfig.get("companyAcctNo");
    String companyRoutingNo = PayrollConfig.get("companyRoutingNo");
    String response = achClient.depositPaycheck(companyAcctNo, companyRoutingNo, paycheck);
    return response.equals("SUCCESS");
}
```

The code above demonstrates complexity in the dimensions of:

* **Absence** as the employee may not be found nor their paycheck calculated.
* **Async IO** where database queries and network calls may be blocking.
* **Validation** by reading the response returned by the bank.
* **Implicit inputs** through configuration of the company's banking information.
* **Implicit outputs** through logging and allowing exceptions to bubble up from database and network operations.
* This function's output is also _unclear_ as `false` can mean failure for any reason, but exceptions can also occur.

Fortunately there are ways to model subsets of these effects and contain the scope of their impact so that code is less complex. An example in Scala psuedocode might appear as the following:

```{.scala .numberLines}
def runPayroll(employeeId: Long): PayrollEffect =
  for {
    employee <- employeeRepo.find(employee).flatMap {
      case Some(e) => pure(e)
      case None    => fail(EmployeeMissing)
    }
    paycheck <- payCalc.calculatePaycheck(employee) match {
      case Some(p) => pure(p)
      case None    => fail(PaycheckMissing)
    }
    companyAcctNo <- getConfig("companyAccountNo")
    companyRoutingNo <- getConfig("companyRoutingNo")
    response <- achClient.depositPaycheck(companyAcctNo, companyRoutingNo, paycheck)
      .flatMap {
        case "SUCCESS" => pure(PayrollDeposited)
        case msg       => fail(PayrollError(msg))
      }
  } yield response
```

This code looks the same, doesn't it? What hides between the lines here is a custom effect model `PayrollEffect` that abstracts away most effects by making explicit all possible outcomes and allowing for async IO. The flow of execution through this code is not performed procedurally: instead flow is controlled by declaring where errors occcur and the effect abstraction short-circuits itself. This abstraction of effects coupled with explicit typing allows for safer code. But how are abstractions over effects created?

Previously I described function composition as a simple case of `h := g ∘ f`. Functors, Applicatives, and Monads address a particular kind of function composition, **composition of functional effects**, which I will demonstrate in the following sections.

## Contexts and Effects

Let me start with a vague term: **Context**. What is a context? A context is a setting within which some term `A` might be produced.

> Contexts in Scala may be neatly represented by a letter and brackets such as `F[_]` when the type of the term is unknown, and `F[A]` when the term is known to be of type `A`. Other letters work nicely of course, as do proper names, as in `Option[Int]`.

Each kind of context carries with it a set of _effects_. Effects are modeled by specific contexts that determine how their terms may be produced. Names of contexts can hint at the effects they model, and with some intuition you may be able to figure out what each context’s effects may be.

### Common contexts and some of their effects

**Contexts representing presence:**

* `Option[A]`: Presence or absence of some instance of term `A`.
* `Either[X, A]`: Conventionally treated as term `A` if valid, term `X` if invalid.
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
case class Some[+A](override val get: A) extends Option[A]
case object None extends Option[Nothing]
```

**`Either[X, A]`** by convention models failure with term `X` and success with term `A`:

```{.scala .numberLines}
sealed trait Either[+X, +A] {
  def left: X = throw new Exception()
  def right: A = throw new Exception()
  def isRight: Boolean = !isLeft
  def isLeft: Boolean = this match {
    case Left(_) => true
    case Right(_) => false
  }
}
case class Left[+X, +A](override val left: X) extends Either[X, A]
case class Right[+X, +A](override val right: A) extends Either[X, A]
```

**`List[A]`** models zero or more instances of term `A`, with an unknown sort and cardinality:

```{.scala .numberLines}
sealed trait List[+A] {
  def isNil: Boolean = this == Nil
  def head: A = throw new Exception
  def tail: List[A] = throw new Exception
  def ::[B >: A](newHead: B): List[B] = newHead :: this
}
case class ::[A](override val head: A, override val tail: List[A]) extends List[A]
case object Nil extends List[Nothing]

object List {
  def apply[+A](values: A*): List[A] = values.foldRight(Nil)(_ :: _)
}
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

Iteration thus _destroys structure_. In order to get a `List[B]` back you would have to rebuild it yourself and any guarantees are purely manual. Structure is discontinuous and to rebuild it requires following _procedural_ steps.

This isn't to say that functional programming is only about iteration and loops. Can you think of other operations that might destroy structure? For example if you use an `await()` operation on a `Future` you will destroy its asynchronous structure and potentially harm the performance of your application.

### Context `F[A]` must produce some term `A`

I stated above: _"For any context `F[_]`, it produces some term `A`."_ If a context were guaranteed to have an instance of a term `A` then you should be able to consume it with your function `f: A => B`, right?

But what if there’s nothing there, as in there are _zero instances_ of term `A`? Can you do anything? When a context has this kind of effect, a sort of "nothing here" or _void effect_, then the `map()` function above doesn’t do anything because there isn’t anything to do. If you try to `map()` an `F[A]` with `f: A => B` then it returns a void `F[B]` as there’s "nothing here". It does this without having used `f: A => B` to get there.

This behavior is referred to as _short-circuiting_ and it is a key feature of all Functors, Applicatives, and Monads. It is exploited in particular to enable _control flow_ and _error handling_, which I will expand on later.

`Option` and `Either` are two prime examples of short-circuiting in Functors. An `Option[A]` will only `map()` an instance of its term is present, and an `Either[X, A]` will only `map()` if an instance of the desired term `A` is present.

### Becoming a Functor

Each context of course must provide its own implementation of `map()` in order for it to be used as a Functor. The functionality of a Functor is not inherited, however, as some contexts might be defined in third-party code where they may not be modified. To circumvent this restriction, Functor implementations are provided via typeclasses. Any type that has the shape `F[_]` may become a functor by implementing the following typeclass:

```{.scala .numberLines}
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
object Functor {
  def apply[F[_]: Functor[F]]: Functor[F] = implicitly[Functor[F]]
}
object FunctorSyntax {
  implicit class FunctorOps[F[_]](val fa: F[A]) extends AnyVal {
    def map[B](f: A => B)(implicitly F: Functor[F]): F[B] = F.map(fa)(f)
  }
}
```

Instances of the Functor typeclass simply extend the typeclass trait as implicits so that they may be imported:

```{.scala .numberLines}
object FunctorInstances {
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
}
```

Each of these instances show remarkable similarities, and this isn’t uncommon across Functors for most data structures. Note in particular how `List` is recursive, with the base case `Nil` representing void. Functor implementations are more complex in contexts such as `IO` and `Future` because they are managing side effects.

Can you see how Functors enable control flow and short-circuiting? The void cases are the specific branches of logic that enable these. If there’s "nothing here", then they don’t do anything. In the specific case of `Either[X, A]`, `Left` may be used to carry error state in its term `X`. `Left` being able to carry its own term is one of `Either`’s specific effects.

Defining a `fizzBuzz()` function that uses a Functor looks like this:

```{.scala .numberLines}
import Functor
import FunctorOps._
def fizzBuzz[F[_]: Functor](context: F[Int]): F[String] =
  context.map {
    case x if x % 3 == 0 && x % 5 == 0 => "fizzbuzz"
    case x if x % 3 == 0 => "fizz"
    case x if x % 3 == 0 => "buzz"
    case x => x.toString
  }
```

And then `fizzBuzz()` may be used for all contexts implementing the Functor typeclass:

```{.scala .numberLines}
// import the Functor instance implicits
import FunctorInstances._

println(fizzBuzz(Some(3)))
// => Some("fizz")
println(fizzBuzz(None))
// => None

println(fizzBuzz(Right(5)))
// => Right("buzz")
println(fizzBuzz(Left("no fizz or buzz")))
// => Left("no fizz or buzz")

println(fizzBuzz(List(1, 2, 3, 4, 5, 15)))
// => List("1", "2", "fizz", "4", "buzz", "fizzbuzz")
println(fizzBuzz(List()))
// => List()
```

## Motivating Applicatives

Functors permits you to consume a term via `map()`:

```{.scala .numberLines}
def map(fa: F[A])(f: A => B): F[B]
```

Consider for a moment: with a Functor you are able to work within the scope of a term within a single context. But what happens if you have two contexts and you want to operate on the terms from each at the same time?

Take for example these two contexts and the function signature for `combine()`:

```{.scala .numberLines}
val fa: F[A]
val fb: F[B]

def combine(x: A, y: B): C
```

How do you apply `combine()` to the terms `A` and `B` produced by the two contexts? This is what motivates Applicatives.

**Applicatives** are an abstraction that allow you to consume terms `A` and `B` in parallel. They are also known by their more formal name _Applicative Functor_ as they are a _special case_ of Functor. Like Functors, they are also a simple structure and define two functions:

1. A constructor to _lift_ a term into the context:

    ```{.scala .numberLines}
    def pure(a: A): F[A]
    ```

    The name `pure()` might feel alien. You can remember the name by thinking of it like this: By taking an instance of term `A`, `ap()` gives you back a context with a present instance of the term `A`, which makes it a _non void_, _valid_, or _**pure**_ context.

2. A function that is able to apply a _lifted function_ to a _lifted term_:

    ```{.scala .numberLines}
    def ap(ff: F[A => B])(fa: F[A]): F[B]
    ```

    The name of the function `ap()` is _apply_, but it is written _ap_ as it has its roots in higher math, where names are frequently one letter.

Here is an example of the two functions defined for the `Option` context:

```{.scala .numberLines}
def pure(a: A): Option[A] = Some(a)

def ap(ff: Option[A => B])(fa: Option[A]): Option[B] = {
  (ff, fa) match {
    case (Some(f), Some(x)) => Some(f(x))
    case _ => None // void
  }
```

Notice that Applicatives also respect a _void effect_ like a Functor does with some specialization: _both_ terms must be present as otherwise there would be no function to apply or no term to apply the function to. Applicative `ap()` thus is an all-or-nothing operation.

Applicatives permit the definition of a higher-order function called `map2()` which may be defined in terms of both `pure()` and `ap()`. It is the parallel analog to Functor's `map()` function:

```{.scala .numberLines}
def map2(fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] =
  ap(map(fa)(a => b => f(a, b)))(fb)
```

The function argument to `map2()` is lifted into the context `F` where it may be applied to the terms produced by the first two contexts `A` and `B`. If either `A` or `B` are absent, then `f` is not applied and a _void_ `F[C]` is returned.

With this `map2()` function, you are able to apply `combine()` to the terms produced by both contexts:

```{.scala .numberLines}
// pseudocode
val fa: F[A]
val fb: F[B]

def combine(x: A, y: B): C

map2(fa)(fb)(combine)
// => F[C]
```

### Becoming an Applicative

Like Functor, each context must provide its own implementation of `pure()` and `ap()` in order for it to be used as an Applicative. Any type with the shape of `F[_]` may become an Applicative by implementing the following typeclass:

```{.scala .numberLines}
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

Notice that Applicatives build on top of Functors. In the above typeclass, `map()` and `map2()` are provided default implementations in terms of `pure()` and `ap()`. Instances may override these functions if they choose. Our Functor instances from before may now be upgraded to Applicatives:

```{.scala .numberLines}
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

## Motivating Monads

Functors give you `map()` so that you may consume a term from a single context. Applicatives give you `pure()` and `ap()` so that you may consume terms from two or more contexts in parallel. But what if you want to sequence consumption of terms? Functors for example will only ever operate in the scope of a single context, and Applicatives only consume from contexts in parallel. Neither Functors nor Applicatives allow you to alter the state of the context such that downstream operations are dependent upon them succeeding.

Concretely, at some point in your program you will want to write code that operates against the result of a previous operation and decide whether to continue:

```{.scala .numberLines}
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

As you can see from the above, this code deposits $20 only if:

   1. There is a user ID present in the session.
   2. The user exists.
   3. The user has a valid session.
   4. The user has a bank account.
   5. The bank service successfully deposits money into the account.

There is a great deal of branching logic in this code that separates success and failure cases as the result of previous operations. This _imperative_ sequencing of operations requires that you manually short-circuit the program if a previous operation fails. This requirement to sequence operations is what motivates Monads.

**Monads** are an abstraction that permit the sequencing of operations. Recall that `map()` accepts a function `f: A => B` as one of its arguments. Monads are a _special case_ of Functor that arise when the term `B` in `f: A => B` is known to have the shape `F[_]`. Specifically, the term `B` itself is an instance of the same context `F`. Consequently, when `map()` is applied with a function `f: A => F[B]` you will receive a nested context `F[F[B]]`. How would you specialize `map()` such that you receive `F[B]` instead?

Monads, like Functors, are a simple structure and define a single function `flatMap()` which joins a nested context with the outer context, or in other words, it _flattens_ the context after it has been mapped:

```{.scala .numberLines}
def flatMap(fa: F[A])(f: A => F[B]): F[B]
```

What this function enables is a capability like `map()` in that term `A` may be consumed with a provided function `f: A => F[B]` and specialized in that new contexts such as `F[B]` may be created as the result of their operation. Two key capabilities are enabled:

* _Imperative programming_ where subsequent operations are dependent upon the results of previous operations.
* _Control flow_ by exploiting the _void effect_ as Monads allow you to inject a void context to short-circuit an operation.

Armed with Monads the previous code may be rewritten using `flatMap()`:

```{.scala .numberLines}
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
        )
      )
```

This code seems a bit hard on the eyes, doesn't it? The deep `>` or _V_ shape of the code is referred to as the _pyramid of death_ and is an unfortunate feature of `flatMap()`-heavy code. There is still branching logic as two contexts, `Option` and `Future`, are being used simultaneously. However, Monads and Monad-like structures are so common in Scala that there is a special syntax for this kind of code, and with a little refactoring the code may be brought into a flattened layout:

```{.scala .numberLines}
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

This code is markedly different, and demonstrates a few features:

* Each logical instruction is on its own line and branching logic has practically disappeared from the primary operation.
* Each instruction is dependent upon the previous instruction succeeding and the entire operation short-circuits on failure.
* Concretely the Monad being used here is a `Future`, which is asynchronous. At no point in the code is complexity imposed by using asynchronous operations.
* Explicit branching is moved to dedicated generalized functions which provide mechanisms for injecting different contexts by condition.

How does this code look to you?

### Becoming a Monad

Like Functor and Applicative, each context must provide their own implementation of `flatMap()` in order for it to be used as a Monad. Any type with the shape `F[_]` may become a Monad by implementing the following typeclass:

```{.scala .numberLines}
trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def flatten[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(fa => fa)
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

Notice that Monads build on top of Applicatives. In the above typeclass, `flatMap()` is provided a default implementation in terms of `map()` and `join()`. Instances may override this function if they choose. Our Applicative instances from before may now be upgraded to Monads:

```{.scala .numberLines}
object MonadInstances {
  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    def flatMap[A](fa: Option[A])(f: A => F[B]): Option[B] =
      ffa match {
        case Some(a) => f(a)
        case None => None
      }
    // ...
  }
  implicit def eitherApplicative[X]: Applicative[Either[X, *]] = new Applicative[Either[X, *]] {
    def flatMap[A, B](fa: Either[X, A])(f: A => Either[X, B]): Either[X, B] =
      fa match {
        case Right(a) => f(a)
        case Left(x) => Left(x)
      }
    // ...
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


## Functors, Applicatives, and Monads: A philosophical perspective

Above I referred to the term borne by a context as a monad in a philosophical sense. I think this is a key concept that no Monad tutorial I’ve read online had made a connection with: that when we work with Monads we only care about the singular A that they produce, or another way to look at it is as the one A that we’re currently operating on. Specifically I call this out because what I’m going to outline reinforces the Monad as a concept and hopefully wraps the name in a little meaning.

If I’m operating in some context of code that is a Monad by type, and it has also been abstracted such that I only know that the code is a Monad and nothing more, then I really don’t have any reason to believe that I am operating on anything other than a singular, indivisible A. It does not matter that the code when called is always reified using a List as its context, as the code itself still operates as though it were operating on just that one A.

The Monad is singular and indivisible. It is the sole focus of my code when I use it, and any context around it is purely abstract. I worry only about the A as everything else is circumstantial.

And if there’s "nothing here"? It doesn’t matter, and I don’t have to worry because that code isn’t running anyway.
