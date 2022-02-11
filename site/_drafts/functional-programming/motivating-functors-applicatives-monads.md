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

**Terms** are identifiers naming unitary or indivisible variables and types.

  * For example, in the declaration `def map[A, B](fa: F[A])(f: A => B): F[B]`{.scala} the variable terms are `fa` and `f`, and the type terms are `A` and `B`.

**Lifting** describes any term `A` becoming wrapped in a context `F[_]` such that `lift: A => F[A]`. A **lifted** term or expression already has the form `F[A]`.

**Composition** describes chaining the output of a function `f: A => B` to the input of function `g: B => C` such that a new function `h: A => C` may defined as `h := g ∘ f`, read as _h is g after f_.

* This alternative notation in Scala `def h[A, C](x: A): C = g(f(x))`{.scala} explicitly defines `h` as an application of the function `g` _after_ `f` is applied to argument `x`.

## Complexity in programming

Complexity is imposed by the _nondeterministic nature_ of programs in the real world.

**Nondeterminism** occurs when a function `f: A => B` maps to a different member of type `B` for any number of times the same member of `A` has been applied. This means that `f: A => B` is driven by **side effects** that occur independent of the signature of the function.

An extreme example of a nondeterministic function is the random number generator `rng: () => Int` as it maps the solitary unit value `()` to all members of type `Int`. This mapping is influenced by some side effect _external to the function's signature_.

Interacting with disk resources is affected by the state of resources on the disk as the contents of these resources may change out of band. For example, reading the same file via `read: FilePath => String` is affected when the contents of the file change on disk. Operations that read and write to sockets are inherently side-effecting.

Interacting with any system over the network implies affecting and being affected by these systems’ states, as in databases and third-party API’s.

Side effects also include **faults** such as the _divide by zero error_, thrown exceptions, and panics as they constitute secondary **implicit outputs** of a function. They impose an additional layer of protection to prevent or recover from them.

Exceptions and panics are fully nondeterministic as there is no single input that guarantees that an exception will never be thrown, as some secondary **implicit input** may influence the outcome. In contrast, a _division by zero error_ only occurs if the input divisor is `0`; it normally is not treated as a side effect for this reason.

Nondeterminism is not defined by mere disk IO and external state. Some operations may produce an unknown number of results relative to their input. A simple example is a function `toBits: Int => [Boolean]` where the known quantity of `Boolean` bits returned requires specific knowledge of the input value. Being coupled with IO, database queries exhibit nondeterminism in the number of rows they return, and in what sort and cardinality the rows have.

Nondeterminisim makes programs complex. Consider for a moment: nondeterminism is complex, but side effects are what allpw our programs to be useful in the real world. How might complexity in programs be reduced if they must also be nondeterministic?

### Modeling complexity

Given a function `f: A => B` and another `g: B => C`: a third function `h: A => C` may be composed of `h := g ∘ f` or _h is g after f_. Programs may be modeled as a function `prog: A => B`, where `prog` is composed of innumerable smaller functions, building the necessary mappings to generate the desired program output.

While the interface of the program `prog` models a simple input mapped to an output, many functions in real world programs must internally interact with implicit inputs and outputs _not present in the program's signature_ of `prog: A => B`. An employee payroll system for example must make database queries and integrate with banks. These _implicit inputs and outputs_ have **effects** that dictate how their associated functions produce their outputs. For example, database queries may return nondeterministic responses and an error might occur when performing a direct deposit.

Faults, errors, and nondeterminism as effects of these operations are opaque in functions modeled as simple input and output, as in `getUser: Int => User`. The signature of this function requires a sort of _tribal knowledge_ in order for the programmer to be aware of what effects may dictate how a `User` is produced from it:

* An associated `User` may not be found.
* The returned `User` may change between function applications.
* The database or network may fault and the function generates an exception that must be handled.

You might be thinking that these cases are a given when working with database code, and that _is_ tribal knowledge. These cases are **effects** that dictate the circumstances under which a `User` may be produced and can be modeled accordingly as part of the typed API of `getUser`. I will explain how this modeling works later; first we will consider how to characterize complexity.

### Effects impose complexity on code

Can you think of some program capabilities that necesitate complexity?

* Configuration comes from any number of sources, both when starting the program and during runtime.
* Database queries each need to be protected against errors and checked if any occur. Each error type requires different handling.
* External API calls will respond in an indeterminate amount of time, if at all.
* Transforming API responses is fraught with surprises. No data coming from an external source is safe.
* Errors need to be logged.
* Metrics need to be gathered.

How might this complexity be handled in code?

* When a program starts, it may read configuration from the environment, a database, or files. Configuration may also be a continuous process at runtime, which may affect the entire architecture of the program.
* Feature flags and ramps need to be queried in real-time. Error handling modes must be provided in the event that a behavior-modifying query fails.
* Database queries are surrounded by code that handles exceptions and recovers from errors. Some languages encourage a hands-off approach to exception handling, leaving a minefield of potential errors.
* API calls require async IO in order to be performant. Async IO "infects" entire codebases requiring its capability.
* External API calls can fail for any reason. Different types of failures may dictate aborting the associated operation or retrying it. As in database queries, exception handling may be deferred to the minefield.
* API responses are validated and transformed into domain objects. Sometimes the responses returned are in an unexpected format, requiring meticulous validation logic and recovery from malformed responses.
* Logging libraries are used to report errors and might require file system or network access. Logging may necessitate async IO itself so that the program remains performant.
* Metrics libraries may be used and require network access, possibly also async IO.

These complexities can be characterized in terms of **effects**. Each of the following effects center on some dimension of nondeterminism:

* **Time and Async** as in asynchronous operations against disk access and network boundaries, such as API calls, database queries, or streaming from files.
* **IO** as in synchronous operations against disk access and network boundaries.
* **Presence** as some functions may not produce anything for some inputs.
* **Length** as database queries return zero or many rows, streaming data over the network implies infinitely many of "something", and long-running programs act as consumers of an infinite input.
* **Correctness** requiring sanitizing and validating program inputs or permissively-structured outputs from applied functions or network calls.
* **Implicit input** in the form of configuration, feature flags and ramps, or other inputs that aren't themselves explicitly provided as part of an API boundary or client interaction.
* **Implicit output** in the form of logging and metrics, as well as exceptions and other faults.
* **State** representing change over time and how it propagates across a program's architecture.

The actual list of effects is innumerable, but these ones are common.

### Demonstrating effects in code

Take for example this code from a hypothetical payroll system:

:::{.numberLines .nowrap}
```java
EmployeeRepo employeeRepo;
BankAchClient achClient;
PayCalculator payCalc;

boolean runPayroll(long employeeId) {
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
:::

The code above demonstrates complexity in the dimensions of:

* **Synchronous IO** as database queries and network calls are implicitly synchronous.
* **Presence** as a `User` may not be found or their `Paycheck` not calculated.
* **Correctness** where the `"SUCCESS"` response from the `achClient` is the only check, any other responses are simply not handled.
* **Implicit input** where the company's bank routing and account numbers are read from an external location.
* **Implicit output** where logging occurs but swallows errors, resulting in opaque `false` return cases, exceptions also occur in a number of cases directly within the code but may also bubble up from database and network operations.

Fortunately there are ways to model sets of these effects and contain the scope of their impact so that code is less complex. Rewriting the above code using an effect model may look like this:

:::{.numberLines .nowrap}
```scala
def runPayroll(employeeId: Long): PayrollEffect[()] =
  for {
    employee <- employeeRepo.find(employee).sequenceA.getOr(fail(EmployeeMissing))
    paycheck <- payCalc.calculatePaycheck(employee).sequenceA.getOr(fail(PaycheckMissing))
    companyAcctNo <- getConfig("companyAccountNo")
    companyRoutingNo <- getConfig("companyRoutingNo")
    response <- achClient.depositPaycheck(companyAcctNo, companyRoutingNo, paycheck)
      .flatMap {
        case "SUCCESS" => pure(())
        case msg       => fail(DirectDepositError(msg))
      }
  } yield response
```
:::

This code looks similar, doesn't it? What hides between the lines here is a custom effect model `PayrollEffect` that abstracts away the effects of presence, aysnc IO, and implicit input and output. This code is thus unburdened of most complexity. 

You may notice that there are no return statements for error cases: the flow of execution through this code is not performed procedurally. Instead flow is controlled by declaring where errors occcur and the main operation short-circuits itself should any inner operation go awry. 

This abstraction of effects allows for safer code that better focuses on the business logic at-hand. But how are abstractions over effects created?

Previously I described function composition as a simple case of `h := g ∘ f`. Functors, Applicatives, and Monads address a particular kind of function composition, **composition of functional effects**, which I will demonstrate in the following sections.

## Contexts and effects

Let me start with a vague term: **Context**. What is a context? _A context is a setting where stuff exists under some circumstances._ Stuff such as instances of term `A` in the context of `F[A]`.

> Contexts in Scala may be neatly represented by a letter and brackets such as `F[_]` when the type of the term is unknown, and `F[A]` when the term is known to be of type `A`. Other letters work nicely of course, as do proper names, as in `Option[Int]`.

Each kind of context models a set of **effects**. Contexts thus represent a concrete, typed API describing how their terms may be produced. Names of contexts can hint at the effects they model, and with some intuition you may be able to figure out what each context’s effects may be.

### Common contexts and some of their effects

**Contexts representing presence:**

* `Option[A]`: Presence or absence of some instance of term `A`. Getting the term `A` when there is no instance causes a fault.
* `Either[X, A]`: Conventionally treated as term `A` if valid, term `X` if invalid. Getting the wrong term causes a fault.
* `List[A]`: Nondeterminism of sort, cardinality, and length of term `A`. There are zero to many many instances of term `A`, and trying get an `A` from an empty list causes a fault.

**Contexts representing side-effects:**

* `IO[A]`: External nondeterminism of an instance of term `A`.
    * The instance may be here already, eventually, or never.
    * The instance is acquired externally from the program via processes that rely on and are influenced by side effects.
    * Execution must await the instance before it may continue.
    * Faults may interrupt the acquisition of the instance.
    * Getting the instance causes a fault if acquisition has failed.
* `Future[A]`: Asynchronous and temporal nondeterminism of an instance of term `A`.
    * The instance may be here already, eventually, or never.
    * The instance is acquired externally from the program via processes that rely on and are influenced by side effects.
    * Execution may be suspended, awaited, or run in parallel with any number of other `Future[_]`'s. This allows for task management against limited computing resources.
    * Faults may interrupt the acquisition of the instance.
    * Getting the instance causes a fault if acquisition is incomplete or has failed.

**Contexts representing implicit input and output:**

* `ReaderT[F, A]`: Reading of implicit inputs in the context of `F[_]`.
    * Propagation of configuration usually leverages this effect by abstracting how instances of configuration values are acquired.
* `WriterT[F, A]`: Writing of implicit outputs in the context of `F[_]`.
    * Logging usually leverages this effect by abstracting how such output leaves the program.
* `StateT[F, A]`: Modifying implicit inputs and outputs in the context of `F[_]`.
    * Models state changing over time in an otherwise-immutable context.

Each of these contexts have two shared qualities in that they _produce_ some term `A` and that their effects dictate _how_ term `A` is produced. But with such a wide array of effects, and with so little overlap between each context, how can instances of term `A` be consumed in a manner unburdened of complexity?

In order to generalize contexts, the key differentiator between them must be abstracted: **effects**. By shedding effects as an _implementation detail_, the production of term `A` remains a shared characteristic that can be accessed by a general interface. By what interface would term `A` be consumed?

## Motivating Functors

**For any context `F[_]`, it produces some term `A`.** If you have a function `f: A => B`, how would you apply it to the term produced by the context `F[A]`? That would require extracting the term, right? Specifically, you can’t apply the following function directly to the context:

:::{.numberLines .nowrap}
```scala
// (pseudo code)
// given a context producing term A
val fa: F[A]
// given a function A => B
def f(x: A): B
// try to apply the function
f(fa) // compile error!
```
:::

Recall from the previous section, contexts share two qualities: that they produce a term, and that they have effects dictating how the term is produced. After abstracting effects, contexts do not expose an obvious shared interface to extract the term. Consider the following definitions for `Option[A]`, `Either[X, A]`, and `List[A]`:

**`Option[A]`** models the presence or absence of an instance of term `A`. It is the effect of _zero or one_ of term `A`:

:::{.numberLines .nowrap}
```scala
sealed trait Option[+A] {
  def get: A = throw new Exception()
  def isSome: Boolean = this != None
}
case class Some[+A](override val get: A) extends Option[A]
case object None extends Option[Nothing]
```
:::

**`Either[X, A]`** by convention models failure with term `X` and success with term `A`. It is the effect of being _either_ `Right`'s instance of term `A` when successful, or `Left`'s (or more appropriately, _Wrong_) instance of term `X` when failed:

:::{.numberLines .nowrap}
```scala
sealed trait Either[+X, +A] {
  def left: X = throw new Exception()
  def right: A = throw new Exception()
  def isRight: Boolean = this match {
    case _: Right => true
    case _ => false
  }
}
case class Left[+X, +A](override val left: X) extends Either[X, A]
case class Right[+X, +A](override val right: A) extends Either[X, A]
```
:::

**`List[A]`** models _zero to many_ instances of term `A`. It is the effect of an unknown number, sort, and cardinality of instances:

:::{.numberLines .nowrap}
```scala
sealed trait List[+A] {
  def head: A = throw new Exception()
  def tail: List[A] = throw new Exception()
  def isNil: Boolean = this == Nil
  def ::[B >: A](newHead: B): List[B] = newHead :: this
  def ++[B >: A](newTail: B): List[B] = this match {
    case h :: t => h :: (t ++ newTail)
    case Nil => newTail
  }
}
case class ::[A](override val head: A, override val tail: List[A]) extends List[A]
case object Nil extends List[Nothing]

object List {
  def apply[+A](values: A*): List[A] = values.foldRight(Nil)(_ :: _)
}
```
:::

### Extracting the instance of the term `A`

Both `Option[A]` and `Either[X, A]` have roughly the same shape in that there either is or isn’t an instance of the desired term `A`. Because of this, an operation `extract(): F[A] => A` is possible as it means the same thing between both of them: `extract()` either gets the existing instance of the term `A` or it faults. In object oriented programming, `Option[A]` and `Either[X, A]` might expose such an interface:

:::{.numberLines .nowrap}
```scala
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
:::

### Extracting an unknown length of instances

How do you `extract()` the term `A` from a `List[A]` such that it means the same thing as in `Option[A]` and `Either[X, A]`?

As in `Option[A]` and `Either[X, A]` there is a notion of the presence or absence of an instance of the term `A`, but presence in `List[A]` implies _one to many_ instances. A solution inspired by object oriented programming might change the interface thusly:

:::{.numberLines .nowrap}
```scala
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
:::

This interface however _is not coherent_. Faulting on absence is preserved as a behavior in `Option[A]` and `Either[X, A]`, but in `List[A]` however `extract()` could return an empty `Seq[A]` and whether that can be interpreted as absence is ambiguous. This requires client code to decide at the call site what absence means. This interface also imposes the effects of `List[A]`, specifically length, upon all client code using it. You would probably be very unhappy using this interface in your own code.

But what about implementing `extract()` for `Future[A]`? When applied to `Future[A]`, the `extract()` function is by its own signature a blocking call. You want your dependency on `A` to be properly asynchronous.

### Abstracting over effects

`Option[A]`, `Either[A]`, `List[A]`, `Future[A]`, and `IO[A]` all have different effects that dictate how term `A` is produced. To follow an axiom from object oriented programming: _abstract what changes_, therefore you have to shed effects as an implementation detail. How might that impact extracting the term `A`?

You may be unsatisfied by the answer: _extraction cannot be generalized_. All you know is that there is term `A`. You don't know whether an instance is present, how many of it there are, whether it's here already, or if it's arriving later. How do you consume term `A` when you know nothing about its instances' nature of existence? Functors solve this problem.

**Functors** are an abstraction that allow you to consume term `A` within the context of `F[A]`. A Functor is a simple structure, a single function called `map()`:

:::{.numberLines .nowrap}
```scala
def map(fa: F[A])(f: A => B): F[B]
```
:::

What `map()` does is _lift_ the function `f: A => B` into the context so that it becomes `F[A] => F[B]`, giving back `F[B]`.

This _lifting_ of functions that `map()` performs _is coherent_ across contexts. With `map()` you can apply `f: A => B` to any `List[A]` just as you can any `IO[A]`. The results of both operations are predictable: your `List[A]` maps to `List[B]` and your `IO[A]` maps to `IO[B]`.

How would you consume the term produced by `Future[A]` or `Option[A]`? You would also use a Functor.

What this enables is your function `f: A => B` to be used with any Functor regardless of its specific effects. Your function `f: A => B` is immediately reusable across all contexts and can be unit tested in isolation  of effects.

### Why does the Functor's `map()` function return `F[B]`?

Recall that contexts generally do not permit extracting terms. Think for a moment: what does extracting the term mean if you’re using a context like `Option[A]`? What about `Future[A]`? Would their effects change how extraction of the term would work?

Extracting _the_ term from `List[A]` flatly doesn't make sense as it has the effect of an unknowm number of instances.

Because there is no way to generalize extracting a term from a context, Functors don’t allow you to operate on contexts in such a way that an instance of the term can "escape" them. Extracting terms is implementation-specific, so this capability is not generalized.

Most importantly, by keeping all operations against terms within their context, the context’s specific effects remain abstracted. Asynchronous operations with `Future[A]` remain asynchronous, the length of `List[A]` remains unknown, and `Option[A]` may or may not be present.

Functors _preserve structure_ by keeping operations within the context. For example, applying `map()` on a `List[A]` or `BinaryTree[A]`:

:::{.nowrap .numberLines}
```markdown
[1, 2, 3, 4] -> map (*2) -> [1, 4, 6, 8]

      4                           8
    /   \                       /   \
   2     6   -> map (*2) ->   4       12
  / \   / \                  / \     /  \
 1   3 5   7                1   6  10    14
```
:::

The application of `map()` produces each a new and identifiable `List[B]` and `BinaryTree[B]`. The values internally may change, as they have been mapped over by a function, and `BinaryTree[B]` specifically may rebalance itself. What matters here is that the structures are coherent and identifiable, and also that our `map()`'ed function is usable between each.

Compare with iteration using a `for` loop:

:::{.nowrap .numberLines}
```markdown
[1, 2, 3, 4] -> for(x) -> x={1, 2, 3, 4}

      4
    /   \
   2     6   -> for(x) -> x={1, 2, 3, 4, 5, 6, 7}
  / \   / \
 1   3 5   7
```
:::

Iteration thus _destroys structure_. In order to get a `List[B]` back you would have to rebuild it yourself and any structural guarantees must be manually implemented following _procedural_ steps.

This isn't to say that functional programming is only about iteration and loops versus `map()`. Can you think of other operations that might destroy structure? For example, if you use an `await()` operation on a `Future[A]` you will destroy its asynchronous structure and potentially harm the performance of your program.

> Where the type of your context is known, it may make sense to pull the structure apart to extract the term. A common use case with `Option` is to extract the term if it is present and provide a default instance otherwise. Similarly, the runtime managing asynchronous `Future`s will create and destroy their structure as part of its normal operation. An example of destructive operations against `Option` appears later.

### Context `F[A]` must produce some term `A`

I stated above: _"For any context `F[_]`, it produces some term `A`."_ If a context were guaranteed to have an instance of a term `A` then you should be able to consume it with your function `f: A => B`, right?

But what if there’s nothing there, as in there are _zero instances_ of term `A`? Can you do anything? When a context has this kind of effect, a sort of "nothing here" or _void effect_, then the `map()` function above doesn’t do anything because there isn’t anything to do. If you try to `map()` a void `F[A]` with `f: A => B` then it returns a void `F[B]` as there’s "nothing here". It does this without having used `f: A => B` to get there.

This behavior is referred to as _short-circuiting_ and it is a key feature of all Functors, Applicatives, and Monads. It is exploited in particular to enable _control flow_ and _error handling_, which I will expand on later.

> `Option[A]` and `Either[X, A]` are two prime examples of short-circuiting in Functors. An `Option[A]` will only `map()` an instance of its term `A` if it is present, and an `Either[X, A]` will only `map()` if an instance of the desired term `A` is present.

### Becoming a Functor

Each context of course must provide its own implementation of `map()` in order for it to be used as a Functor. Functor implementations in Scala are provided via typeclasses. Any type that has the shape `F[_]` may become a Functor by implementing the following typeclass:

:::{.numberLines .nowrap}
```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
object Functor {
  def apply[F[_]: Functor[F]]: Functor[F] = implicitly[Functor[F]]
}
object FunctorSyntax {
  implicit class FunctorOps[F[_]](val fa: F[A]) extends AnyVal {
    def map[B](f: A => B)(implicit F: Functor[F]): F[B] = F.map(fa)(f)
  }
}
```
:::

Instances of the Functor typeclass simply extend the typeclass trait and make themselves available implicitly:

:::{.numberLines .nowrap}
```scala
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
        case Right(a) => Right(f(a)) // apply map
      }
  }
  implicit val listFunctor = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa match {
        case a :: at => f(a) :: map(at)(f) // apply map, recurse
        case Nil => Nil // void
      }
  }
}
```
:::

The three instances show remarkable similarities, and this isn’t uncommon across Functors for most data structures. Note in particular how `List[_]` is recursive, with the base case `Nil` representing void. Functor implementations are more complex in contexts such as `IO[_]` and `Future[_]` because they are managing side effects. The complexities imposed by each of these contexts is completely abstracted, allowing function `f: A => B` to operate unburdened by effects with focus on specific program logic.

Can you see how Functors enable control flow and short-circuiting? The void cases are the specific branches of logic that enable this. If there’s "nothing here", then they don’t do anything. In the specific case of `Either[X, _]`, `Left[X, _]` may be used to carry error state in its term `X`. This satisfies the "either" effect between `Left[X, A]` for failure and `Right[X, A]` for success.

> I like to think of `Right` as being "the right one I want." This pun is why `Either` is conventionally leveraged for the effect of correct vs. incorrect or success vs. failure.

#### Using `map()` as a general abstraction

Defining a `fizzBuzz()` function that uses a Functor looks like this:

:::{.numberLines .nowrap}
```scala
import Functor
import FunctorSyntax._
def fizzBuzz[F[_]: Functor](context: F[Int]): F[String] =
  context.map {
    case x if x % 3 == 0 && x % 5 == 0 => "fizzbuzz"
    case x if x % 3 == 0 => "fizz"
    case x if x % 3 == 0 => "buzz"
    case x => x.toString
  }
```
:::

And then `fizzBuzz()` may be used for all contexts implementing the Functor typeclass:

:::{.numberLines .nowrap}
```scala
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
:::

By using Functors, the `fizzBuzz()` function is free to focus on its specific program logic:

* Return "fizz" when `x` is divisible by `3`
* Return "buzz" when `x` is divisible by `5`
* Return "fizzbuzz" when `x` is divisible by both `3` and `5`
* Return `x` as a `String` otherwise

At no point is `fizzBuzz()` burdened by the effects of the context it executes against.

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
// pseudocode
val fa: F[A]
val fb: F[B]

def combine(x: A, y: B): C

map2(fa)(fb)(combine)
// => F[C]
```
:::

### Becoming an Applicative

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
