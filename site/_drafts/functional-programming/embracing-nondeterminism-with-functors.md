---
title: Embracing Nondeterminism with Functors
description: Embracing nondeterminism in functional programs using Functors
author: Logan McGrath
comments: false
date: 2022-01-24T17:14:03-0800
tags: functional programming, programming, scala, design patterns
layout: post
---

What’s a Functor? An Applicative? Or a Monad? The internet is teeming with articles that answer some facet of their character, but few provide a concrete motivation for why these structures exist or appear in the forms that they do.

<!--more-->

In this post I will:

* Demonstrate nondeterminism and complexity as the motivators for the use of Functors, Applicatives, and Monads as **design patterns** in functional programming.
* Provide **effects** as a model for characterizing nondeterminism and complexity in programs.
* Demonstrate the use of Functors as an elementary design pattern for abstracting complexity.

> The code that accompanies this post may be found [here]().

## Conventions

I will provide Scala code for concrete examples.

Terminology will leverage common functional programming vocabulary.

Where there is conceptual overlap with object oriented programming, I will leverage those terms to drive the intent behind abstractions.

### Terminology

**Expressions** are values that are described by some type `A`.

**Functions** are a _special case_ of expressions that map some type `A` to some type `B`. They are described by `f: A => B`, read as _f is A to B_.

**Terms** are identifiers naming unitary or indivisible variables and types.

  * For example, in the declaration:
      
      ```scala
      def map[A, B](fa: F[A])(f: A => B): F[B]
      ```
      
      The variable terms are `fa` and `f`, and the type terms are `A` and `B`.

**Lifting** describes injecting a term `A` into a context `F[_]` such that `lift: A => F[A]`, read as _A to F of A_. A **lifted** term or expression already has the form `F[A]`, or _F of A_.

**Lowering** describes extracting a term `A` from a context `F[A]` such that `lower: F[A] => A`.

**Composition** describes chaining the output of a function `f: A => B` to the input of function `g: B => C` such that a new function `h: A => C` may defined as `h := g ∘ f`, read as _h is g after f_.

* This alternative notation in Scala concretely defines `h` as an application of the function `g` _after_ `f` is applied to argument `x`.

    ```scala
    def f(a: A): B
    def g(b: B): C
    def h(x: A): C = g(f(x))
    ```

## Complexity in programming

Complexity is imposed by the _nondeterministic nature_ of programs in the real world.

**Nondeterminism** occurs when a function `f: A => B` maps to a different member of `B` for any number of times the same member of `A` has been applied. This means that `f: A => B` is driven by **side effects** that occur independent of the signature of the function.

An extreme example of a nondeterministic function is the random number generator `rng: () => Int` as it maps the solitary unit value `()` to all members of type `Int`. This mapping is influenced by some side effect _external_ to the function's signature.

Interactions with disk resources are affected by the state of resources on disk as the contents of these resources may change out of band. For example, reading the same file via `read: FilePath => String` is affected when the file is modified. Operations that read and write to sockets are intended to be side-effecting.

Interactions with any system over the network imply affecting and being affected by these systems’ states, as in databases, other services, and external API’s.

Side effects also include **faults** such as the _divide by zero error_, thrown exceptions, and panics as they constitute **implicit outputs** of a function. They impose an additional layer of protection to prevent or recover from them.

Exceptions and panics are fully nondeterministic as there is no single input that guarantees that an exception will never be thrown, as some **implicit input** may influence the outcome.

> In contrast with most exceptions and faults, a _divide by zero error_ only occurs if the input divisor is `0`; it normally is not treated as a side effect in practice for this reason.

Nondeterminism is not defined by disk IO and external state alone. Some operations may produce an unknown number of results relative to their input. A simple example is a function `toBits: Int => [Boolean]` where the known quantity of `Boolean` bits returned requires specific knowledge of the input argument. In practice, `toBits` is nondeterministic because of its results' unknown quantity.

Nondeterminism and unknowns require complexity in code. Side effects are what make our programs useful in the real world, which requires that we _embrace_ nondeterminism. _How might complexity in programs be reduced if they must also be nondeterministic?_

### Implied complexity

Given a function `f: A => B` and another `g: B => C`: a third function `h: A => C` may be composed of `h := g ∘ f` or _h is g after f_. Programs may be modeled as a function `prog: A => B`, where `prog` is composed of innumerable smaller functions, together in concert building the necessary mappings to generate the desired program output.

Functions in real world programs must internally interact with implicit inputs and outputs _not present_ in the program's signature of `prog: A => B`. An employee payroll system for example must make database queries and integrate with banks. These implicit inputs and outputs have **effects** that dictate how their associated functions produce their desired outputs. For example, database queries return nondeterministic responses of unknown length and an error might occur when performing a direct deposit. These effects determine how and whether payday is successfully produced.

Faults, errors, and unknowns as effects of these operations are opaque in functions modeled as simple input to output, as in `getUser: Int => User`. The signature of this function requires _tribal knowledge_ in order for you to be aware of what effects may dictate how a `User` is produced from it. For example:

* An associated `User` may not be found.
* The returned `User` may change between applications of the same `Int`.
* The database or network may fault and the function generates an exception that must be handled.

You might be thinking that these cases are a given when working with database code, and that _is_ tribal knowledge. These cases are **effects** that dictate the circumstances under which a `User` may be produced and can be modeled accordingly as part of the typed API of `getUser`. I will soon explain how this modeling works; first we will consider how to characterize complexity.

### Modeling complexity

Can you think of some program capabilities that necessitate complexity?

* Configuration
* Database queries
* External system integration
* Validating provided input and API responses
* Error handling
* Performance monitoring
* Feature flags, ramps, and A/B testing

How might supporting these capabilities cause complexity appear in code?

* When a program starts, it may read configuration from the environment, a database, or files. Configuration may also be a continuous process at runtime, which may affect the entire architecture of the program.
* Database queries are surrounded by code that handles exceptions and recovers from errors. Some languages encourage a hands-off approach to exception handling, leaving a minefield of potential errors. Rows returned by queries will have an unknown length, sort, and cardinality.
* API calls to external systems require async IO in order for programs to be performant. Async IO "infects" entire codebases requiring its capability.
* External API calls can fail for any reason. Different types of failures may dictate aborting the associated operation or retrying it. As in database queries, exception handling may be deferred to the minefield.
* External input and API responses are validated and transformed into domain objects. Sometimes the responses returned are in an unexpected format, requiring meticulous validation logic and recovery from malformed responses.
* Logging libraries are used to report errors and might require file system or network access. Logging may necessitate async IO itself so that the program remains performant.
* Metrics libraries may be used and require network access, possibly also async IO.
* Feature flags and ramps need to be queried in real-time. Error handling modes must be provided in the event that a behavior-modifying query fails. A/B testing requires deterministically persisting identities' sessions within their assigned variants.

_These complexities can be characterized in terms of **effects**._ Each of the following effects center on some dimension of nondeterminism:

* **Time and Async** as in asynchronous operations against disk access and network boundaries, such as API calls, database queries, or streaming from files.
* **IO** as in synchronous operations against disk access and network boundaries.
* **Presence** as some functions may not produce anything for some inputs.
* **Length** as database queries return zero or many rows, streaming data over the network implies infinitely many of "something", and long-running programs act as consumers of an infinite input.
* **Correctness** requiring sanitizing and validating program inputs or permissively-structured outputs from applied functions or network calls.
* **Implicit input** in the form of configuration, feature flags and ramps, A/B test assignment, or other inputs that aren't themselves explicitly provided as part of an API boundary or client interaction.
* **Implicit output** in the form of logging and metrics, as well as exceptions and other faults.
* **State** representing change over time and how it propagates across a program's architecture. Implicit inputs and outputs together constitute a form of state.

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
* **Correctness** where the `"SUCCESS"` response from the `achClient` is the only check performed. Other responses are simply not handled.
* **Implicit input** where the company's bank routing and account numbers are read from an external location.
* **Implicit output** where logging occurs but swallows errors, resulting in opaque `false` return cases. Exceptions also bubble up from database and network operations.

Fortunately there are ways to model sets of these effects and contain the scope of their impact so that code is less complex. Rewriting the above code using an effect model may look like this:

:::{.numberLines .nowrap}
```scala
def runPayroll(employeeId: Long): PayrollEffect[()] =
  for {
    employee <- employeeRepo.find(employee).flip.getOr(fail(EmployeeMissing))
    paycheck <- payCalc.calculatePaycheck(employee).flip.getOr(fail(PaycheckMissing))
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

This code looks similar, doesn't it? What hides between the lines here is a custom effect model `PayrollEffect` that abstracts away the effects of presence, async IO, and implicit input and output. This code is thus unburdened of most complexity.

You may notice that there are no return statements for error cases: the flow of execution through this code is not performed procedurally. Instead flow is controlled by declaring where errors occur and the main operation short-circuits itself should any inner operation fail.

This abstraction of effects allows for safer code that better focuses on the business logic at-hand. But how are abstractions over effects created?

Previously I described programs as a case of function composition: `h := g ∘ f`. Functors, Applicatives, and Monads address a special case of function composition, the **composition of functional effects**. In the following section I will demonstrate abstracting the elementary effects of presence.

## Contexts and effects

Let me start with an abstract concept: **Context**. What is a context? _A context is a setting where stuff exists under some circumstances._ Stuff such as instances of term `A` in the _context_ of `F[A]`.

> Contexts in Scala may be neatly represented by a letter and brackets such as `F[_]` when the type of the term is unknown, and `F[A]` when the term is known to be of type `A`. Other letters work nicely of course, as do proper names, as in `Option[Int]`.

Each kind of context models a set of **effects**. Contexts thus represent a concrete, typed API describing how their terms may be produced. Names of contexts can hint at the effects they model, and with some intuition you may be able to figure out what each context’s effects may be.

### Common contexts and some of their effects

**Elementary contexts representing dimensions of presence:**

* `Option[A]`: Presence, absence, or _optionality_ of some instance of term `A`. Getting the term `A` when there is no instance causes a fault.
* `Either[X, A]`: Conventionally treated as _either_ term `A` if valid _or_ term `X` if invalid. Getting the wrong term causes a fault.
* `List[A]`: _Unknown_ sort, cardinality, and length of term `A`. Trying get an `A` from an empty list or from beyond the end of it causes a fault.

**Contexts representing side-effects:**

* `IO[A]`: Externally-aqcuired term `A`.
    * The instance may be here already, eventually, or never. Execution awaits the instance until it arrives or a fault occurs.
    * The instance is acquired externally from the program via processes that rely on and are influenced by side effects.
    * Faults may interrupt the acquisition of the instance.
    * Getting the instance causes a fault if acquisition has failed.
* `Future[A]`: Eventually-acquired term `A`.
    * The instance may be here already, eventually, or never.
    * The instance may be acquired externally from the program via processes that rely on and are influenced by side effects.
    * The instance may be produced via long-running processes.
    * Acquisition of `A` be suspended, awaited, or run in parallel with any number of other `Future[_]`'s. This allows for task management against limited computing resources.
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

## Consuming terms produced by contexts

**For any context `F[_]`, it produces some term `A`.** If you have a function `f: A => B`, how would you apply it to the term produced by the context `F[A]`? That would require _lowering_ the term. Specifically, you can’t apply the following function directly to the context:

:::{.numberLines .nowrap}
```scala
// given a context producing term A
val fa: F[A]
// given a function A => B
def f(x: A): B
// try to apply the function
f(fa) 
// compile error!
```
:::

Recall from the previous section, contexts share two qualities: that they produce a term, and that they have effects dictating how the term is produced. After abstracting effects, contexts do not expose an obvious shared interface to extract the term. Consider the following definitions for `Option[A]`, `Either[X, A]`, and `List[A]`:

**`Option[A]`** models the presence, absence, or _optionality_ of an instance of term `A`. It is the effect of _zero or one_ of term `A`:

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

**`Either[X, A]`** by convention models failure with term `X` and success with term `A`. It is the effect of being _either_ `Right`'s term `A` when successful _or_ `Left`'s term `X` when failed:

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

**`List[A]`** models _zero to many_ instances of term `A`. It is the effect of an _unknown_ number, sort, and cardinality of instances:

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

Both `Option[A]` and `Either[X, A]` have roughly the same shape in that there either is or isn’t an instance of the desired term `A`. Because of this, a _lowering_ operation `extract(): F[A] => A` is possible as it means the same thing between both of them: `extract()` either gets the existing instance of the term `A` or it faults. In object oriented programming, `Option[A]` and `Either[X, A]` might expose such an interface:

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

### Extracting an unknown length of `A`

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

_This interface however is not coherent._ Faulting on absence is preserved as a behavior in `Option[A]` and `Either[X, A]`, but in `List[A]` however `extract()` could return an empty `Seq[A]`. Whether an empty `Seq[A]` can be interpreted as absence is ambiguous. This requires client code to decide at the call site what absence means. This interface also imposes the effects of `List[A]`, such as length, upon all client code using it. You would probably be very unhappy using this interface in your own code.

> Alternatively, absence can be preserved in `List[A]` by modifying `extract()` to return a `NonEmptyList[A]`. This has the effect of always having _at least one_ instance of term `A`. You're still stuck with an unknown length of instances, though.

What about implementing `extract()` for `Future[A]`? When applied to `Future[A]`, the `extract()` function is by its own signature a blocking call. You want your dependency on `A` to be properly asynchronous.

## Motivating Functors as a design pattern

`Option[A]`, `Either[A]`, `List[A]`, `Future[A]`, and `IO[A]` all have different effects that dictate how term `A` is produced. You must follow an axiom from object oriented programming: _abstract what changes_. Therefore you have to shed effects as an implementation detail. How might that impact lowering the term `A`?

You may be unsatisfied by the answer: _extraction cannot be generalized_. All you know is that there is term `A`. You don't know whether an instance is present, how many of it there are, whether it's here already, or if it's arriving later. How do you consume term `A` when you know nothing about its instances' nature of existence? Functors solve this problem.

**Functors** are an abstraction that allow you to consume term `A` within the context of `F[A]`. A Functor is a simple structure, a typeclass which provides a single function called `map()`:

:::{.numberLines .nowrap}
```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
object Functor {
  def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
}
```
:::

What `map()` does is _lift_ the function `f: A => B` into the context so that it behaves as `F[A] => F[B]`, giving back `F[B]`.

This _lifting_ of functions that `map()` performs is _coherent across contexts_. With `map()` you can apply `f: A => B` to any `List[A]` just as you can any `IO[A]`. The results of both operations are predictable: your `List[A]` maps to `List[B]` and your `IO[A]` maps to `IO[B]`.

How would you consume the term produced by `Future[A]` or `Option[A]`? You would also use a Functor.

What this enables is your function `f: A => B` to be used with any Functor regardless of its specific effects. Your function `f: A => B` is immediately reusable across all contexts and can be unit tested in isolation  of effects.

### Why does the Functor's `map()` function return `F[B]`?

Recall that contexts generally do not permit extracting terms. Think for a moment: what does extracting the term mean if you’re using a context like `Option[A]`? What about `Future[A]`? Would their effects change how extraction of the term would work?

Extracting _the_ term from `List[A]` flatly doesn't make sense as it has the effect of an unknown number of instances.

Because there is no way to generalize extracting a term from a context, Functors don’t allow you to operate on contexts in such a way that an instance of the term can "escape" them.

Most importantly, by keeping all operations against terms within their context, the context’s specific effects remain abstracted. Asynchronous operations with `Future[A]` remain asynchronous, the length of `List[A]` remains unknown, and `Option[A]` may or may not be present.

Functors thus _preserve structure_ by keeping operations within the context. For example, applying `map()` on a `List[A]` or `BinaryTree[A]`:

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

The application of `map()` produces two new and identifiable `List[B]` and `BinaryTree[B]`s. The values internally change, as they have been mapped-over by a function, and `BinaryTree[B]` specifically may re-balance itself. What matters here is that the structures are coherent and identifiable.

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

Iteration as a form of lowering _destroys structure_. In order to get a `List[B]` back you would have to rebuild it yourself and any structural guarantees must be manually implemented following _procedural_ steps.

This isn't to say that functional programming is only about iteration and loops versus `map()`. Can you think of other operations that might destroy structure? For example, if you use an `await()` operation on a `Future[A]` you will destroy its asynchronous structure and potentially harm the performance of your program.

> Where the type of your context is known, it may make sense to pull the structure apart to extract the term. A common use case with `Option` is to extract the term if it is present and provide a default instance otherwise.

### Context `F[A]` must produce some term `A`

Recall my statement from above: _"For any context `F[_]`, it produces some term `A`."_ If a context were guaranteed to have an instance of a term `A` then you should be able to consume it with your function `f: A => B`, right?

But what if there’s nothing there, as in there are _zero_ instances of term `A`? Can you do anything? When a context has this kind of effect, a sort of "nothing here" or _void_ effect, then the `map()` function above doesn’t do anything because there isn’t anything to do. If you try to `map()` a void `F[A]` with `f: A => B` then it returns a void `F[B]` as there’s "nothing here". It does this without having used `f: A => B` to get there.

This behavior is referred to as _short-circuiting_ and it is a key feature of Functors, Applicatives, and Monads that encode some notion of void. It is exploited in particular to enable _control flow_ and _error handling_, which I will expand on in later parts.

> `Option[A]` and `Either[X, A]` are two prime examples of short-circuiting in Functors. An `Option[A]` will only `map()` an instance of its term `A` if it is present, and an `Either[X, A]` will only `map()` if an instance of the desired term `A` is present.
>
> In contrast, the `Id[A]` context has the effect of the _identity_ of term `A`. To put it plainly, `Id[A]` is _the instance_ of term `A`. As the instance is always present, this context never short-circuits.

## Implementing a Functor

Each context of course must provide its own implementation of `map()` in order for it to be used as a Functor. Functor implementations in Scala are provided via typeclasses, and any type that has the shape `F[_]` may become a Functor by implementing the typeclass from above:

:::{.numberLines .nowrap}
```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
object Functor {
  def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]
}
```
:::

Instances of the Functor typeclass simply implement this trait and make themselves available implicitly:

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

These three instances for `Option[_]`, `Either[X, _]`, and `List[_]` show remarkable similarities, and this isn’t uncommon across Functors for most data structures. Note in particular how `List[_]` is recursive, with the base case `Nil` representing void. Functor implementations are more complex in contexts such as `IO[_]` and `Future[_]` because they are managing side effects. The complexities imposed by each of these contexts are completely abstracted, allowing function `f: A => B` to operate unburdened by effects with a focus on specific program logic.

Can you see how Functors enable control flow and short-circuiting? The void cases are the specific branches of logic that enable this. If there’s "nothing here", then they don’t do anything. In the specific case of `Either[X, _]`, `Left[X, _]` may be used to carry error state in its term `X`. This satisfies the _either_ effect between `Left[X, A]` for failure and `Right[X, A]` for success.

> You can think of `Right`'s term as being "the right instance you want" because it's "correct". _Right?_ This pun is why `Either` is conventionally leveraged for the effect of correct vs. incorrect or success vs. failure.

Contrasting with void-effects, here's what the Functor instance for `Id[_]` looks like:

:::{.numberLines .nowrap}
```scala
type Id[A] = A // see how sneaky the context definition is?

object FunctorInstances {
  implicit val idFunctor: Functor[Id] = new Functor[Id] {
    def map[A, B](fa: Id[A])(f: A => B): F[B] =
      f(fa) // map always applies!
  }
}
```
:::

### Using `map()` as a general abstraction

Defining a `fizzBuzz()` function that uses a Functor looks like this:

:::{.numberLines .nowrap}
```scala
import Functor
import FunctorSyntax._
def fizzBuzz[F[_]: Functor](context: F[Int]): F[String] =
  Functor[F].map(context) {
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

## Functors are universal

You might be thinking that lists and arrays in the wild already have a `map()` operation available. `Promise`s in JavaScript also have their own `map()`. You've probably been using Functors for a while and never realized!

Functors as a formal abstraction API find their use in cases where the specific kind of the context is unimportant. However, you might observe that the _shape_ of a Functor appears in many places without being _called_ a Functor. Using `map()` on a list conceptually performs the same operation as on both arrays and `Promise`s. Other structures defining `map()` operations are Functors because Functors arise from settings where stuff exists under some circumstances.

### Functor laws

The universality of Functors has a [formal definition](https://en.m.wikipedia.org/wiki/Functor) within the higher math of [category theory](https://en.m.wikipedia.org/wiki/Category_theory). In fact, this definition can be applied to any structure that has the shape of a Functor to assert that they are well-behaved.

In order to be a Functor, a context must satisfy two laws:

1. Preservation of identity functions:

    
    ```{.scala .nowrap}
    context.map(x => x) == context
    ```

2. Preservation of function composition:

    ```{.scala .nowrap}
    context.map(g ∘ f) == context.map(f).map(g)
    ```

Here are the two laws applied against Scala's builtin `List` type, which defines its own `map()`operation:

:::{.numberLines .nowrap}
```scala
def preservesIdentityFunctions(list: List[Int]): Unit = 
  assert(list.map(x => x) == list)
  
def preservesFunctionComposition(list: List[Int]): Unit = {
  val f: Int => Int = _ + 3
  val g: Int => Double = _ / 17.0
  assert(list.map(g compose f) == list.map(f).map(g))
}

val listOf3 = List(1, 2, 3)
preservesIdentityFunctions(listOf3)
preservesFunctionComposition(listOf3)

val nil = List()
preservesIdentityFunctions(nil)
preservesFunctionComposition(nil)
```
:::

Functors need obey only these two laws. These laws assert that Functors compose in the same manner as functions `f` and `g` do in `h := g ∘ f`. Functors thus _compose functional effects_ and may be universally regarded as the _context of effects_.

This means, ideally, that `map()` is the same regardless of context.

## Building upon Functors

In this post I introduced Functors as an elementary abstraction which permit you to consume term `A` within some context `F[A]` via `map()`:

:::{.numberLines .nowrap}
```scala
def map[A, B](fa: F[A])(f: A => B): F[B]
```
:::

This simple abstraction is enabling on its own, as it frees the logic in function `f` from the burden of complexity present in the context of `F[_]`. However this is only an _elementary_ abstraction. Consider for a moment: with a Functor you are able to work against the term produced by a single context. But what happens if you require terms produced from two or more contexts?

Take for example these two instances of the context `F[_]` and the function signature for `combine()`:

:::{.numberLines .nowrap}
```scala
val fa: F[A]
val fb: F[B]

def combine(x: A, y: B): C
```
:::

How do you apply `combine()` to the terms `A` and `B` produced by the contexts?

In my next post, we will explore how **Applicatives** enable working within two or more contexts at the same time, as well as the many ways that you will be able to exploit this capability.
