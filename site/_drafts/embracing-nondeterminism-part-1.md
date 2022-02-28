---
title: "Embracing Nondeterminism Part I: Contexts and Effects"
description: Abstracting nondeterminism and complexity by modeling effects as first class concepts in programs.
author: Logan McGrath
comments: true
date: 2022-01-24T17:14:03-0800
tags: functional programming, programming, scala, design patterns
layout: post
twitter:
  image: /images/tags/functional-programming/functional-grass-512x512.png
og:
  image:
    url: /images/tags/functional-programming/functional-grass-512x512.png
    alt: Abstracting nondeterminism and complexity by modeling effects as first class concepts in programs.
---

Have you ever received an unexpected `null` reference? Have you ever written a function to validate some input only to have it turn into spaghetti over time? How do you anticipate exceptions and protect against them at runtime?

<!--more-->

Significant portions of program logic exist to address special cases imposed by unknowns and nondeterminism. Have you ever written some code only to find out it does something unexpected when it's running in production? To protect against such errors you first have to be aware that an operation may return something unexpected, such as a `null` reference, invalid data, or throw an exception, and then write code that anticipates and recovers from such cases.

_[Defensive programming][]_ as a practice tries to protect against errors by preempting how they might occur. However anticipation of errors rests entirely on _tacit knowledge_ and imposes complex code to handle and recover from errors. _This complex code draws focus from writing the business logic that drives the value of programs_.

In this post I will provide **effects** as a model for characterizing nondeterminism and complexity in programs. I will introduce a **design pattern** to abstract complexity using this model.

> The code that accompanies this post may be found [here]().

## Conventions

_This post assumes familiarity with Scala code_. I will provide Scala code for concrete examples and note where they are different. Abstract examples will employ notation that looks like _"math"_.

Terminology will leverage common functional programming vocabulary.

Where there is conceptual overlap with object oriented programming, I will leverage those terms to drive the intent behind abstractions.

### How to read "math"

**A** reads as _"A"_ and means _"type of A"_. **B** reads as _"B"_ and means _"type of B"_. Capital letters and words starting with capital letters are a kind of _type_.

**()** a pair of parentheses, reads as _"unit"_ and means _"nothing"_ or _"void"_. It is technically a value, but the value isn't anything.

**=>** an equals and greater than sign, or right arrow, reads as _"to"_ or _"mapped to"_ and indicates _change_ or _transformation_.

**A => B** which read as _"A to B"_ and means _"function type of A mapped to B"_. Functions are a special kind of _type_ which represent any type _mapped to_ another type using a right arrow. Functions can map any type or function, as functions themselves are types.

**F[_]** reads as _"(context) F of underscore"_ or _"context F"_. Contexts are like constructors for types in that they take types as an argument: when given a type for their argument, _the underscore_, they are instantiated as a type.

**F[A]** reads as _"(context) F of A"_ and is an instantiation of `F[_]`.

**f** reads as _"f"_ and means _"function f"_ or _"variable f"_. Lower-case letters and words starting with a lower-case letter are functions or variables.

**:** a colon, reads as _"is"_ but means _"has type of"_.

**f: A => B** reads as _"f is A to B"_ or _"function f has type of A mapped to B"_.

**fa: F[A]** reads as _"fa is F of A"_ or _"variable fa has type of context F of A"_.

**:=** a colon and equals sign, reads as _"is"_ but means _"is defined as"_

**∘** a ring, reads as _"after"_ and represents the operation of _function composition_, which is defined in [Terminology](#terminology).

**h := g ∘ f** reads as _"h is g after f"_ or _"h is defined as function g after function f"_. This is described in [Terminology](#terminology).

### Terminology

**Functors** are a programming pattern explored in this post. Its relatives, **applicatives** and **monads**, receive honorable mentions and are addressed in later posts.

**Expressions** are values that are described by some type `A`.

**Functions** are a _special case_ of expressions that map some type `A` to some type `B`. They are described by `f: A => B`, read as _f is A to B_.

**Terms** are identifiers naming unitary or indivisible variables and types.

  * For example, in the declaration:

      ```{.scala .numberLines}
      def map[A, B](fa: F[A])(f: A => B): F[B]
      ```

      The variable terms are `fa` and `f`, and the type terms are `A` and `B`.

**Contexts** are like containers. They are noted using `F[_]`, read as _context F_ when their contents are unspecified, and `F[A]` or _F of A_ when their contents are known to be of type `A`. They are more concretely defined in later sections.

**Lifting** describes injecting a term `A` into a context `F[_]` such that `lift: A => F[A]`, read as _lift is A to F of A_. A **lifted** term or expression already has the form `F[A]`, or _F of A_.

**Lowering** describes extracting a term `A` from a context `F[A]` such that `lower: F[A] => A`, read as _lower is F of A to A_.

**Composition** describes chaining the output of a function `f: A => B` to the input of a function `g: B => C` such that a new function `h: A => C` may defined as `h := g ∘ f`, read as _h is g after f_.

* This alternative notation in Scala concretely defines `h` as an application of the function `g` _after_ `f` is applied to argument `x`.

    ```scala
    def f(a: A): B
    def g(b: B): C
    def h(x: A): C = g(f(x))
    ```

## Complexity in programming

Complexity is imposed by the _nondeterministic nature_ of programs in the real world. Any unknown quantity along some dimension requires specific handling in code, which creates complexity and draws engineering focus away from business logic.

Nondeterminism as a _dependence on factors other than initial state and input_ arises when a function `f: A => B` maps to a different member of `B` for any number of times the same member of `A` has been applied. This means that `f: A => B` is driven by **side effects** that occur independent of the signature of the function.

> An extreme example of a nondeterministic function is the random number generator `rng: () => Int` as it maps the solitary unit value `()` to all members of type `Int`. This mapping is influenced by some side effect _external_ to the function's signature.

Nondeterminism as _dimensions of unknown quantities_ arise in functions returning types such as lists or potentially `null` values. These outputs have unknown length and presence respectively, and require special handling. _This means that nondeterminism is not defined by disk IO and external state alone._

> A simple example is a function `toBits: Int => List[Boolean]` where the known quantity of `Boolean` bits returned requires specific knowledge of the input argument.
>
> You may find hashmap lookups more familiar: unless you have specific knowledge of the key used to lookup a value from a hashmap, you don't have any guarantee whether you will receive anything.

The dimension of **implicit outputs** includes **faults** such as the _divide by zero error_, panics, and thrown exceptions. They impose an additional layer of protection to prevent or recover from them. Panics and exceptions are fully nondeterministic as there is no single input that guarantees that an exception will never be thrown, as some **implicit input** may influence the outcome.

> In contrast with most faults, a _divide by zero error_ only occurs if the input divisor is `0`. The additional check for `0` that division sometimes requires is not considered complexity in practice.

_Nondeterminism creates complex code because it imposes special cases that must be managed._ Side effects however are what make our programs useful in the real world, which requires that we _embrace_ nondeterminism.

_How might complexity in programs be reduced if they must also be nondeterministic?_

### Implied complexity

Given a function `f: A => B` and another `g: B => C`: a third function `h: A => C` may be composed of `h := g ∘ f` or _h is g after f_. Programs may be modeled as a function `prog: A => B`, where `prog` is composed of innumerable smaller functions, together in concert building the necessary mappings to generate the desired program output.

Functions in real world programs must internally interact with implicit inputs and outputs _not present_ in the program's signature of `prog: A => B`. An employee payroll system for example must make database queries and integrate with banks. These implicit inputs and outputs have **effects** which determine how their associated functions produce their desired outputs. For example, database queries return nondeterministic responses of unknown length and an error might occur when performing a direct deposit. _These effects determine how and whether payday is successfully produced._

Faults, errors, and unknowns as effects of these operations are opaque in functions modeled as simple input to output, as in `getEmployee: Int => Employee`. The signature of this function requires _tacit knowledge_ in order for you to be aware of what effects may determine how an `Employee` is produced from it. For example:

* An associated `Employee` may not be found.
* The returned `Employee` may change between applications of the same `Int` employee ID.
* The database or network may fault and the function generates an exception that must be handled.

You might be thinking that these cases are a given when working with database code, but that knowledge only comes with experience. These cases are **effects** which describe the circumstances under which an `Employee` may be produced and can be modeled accordingly as part of the typed API of `getEmployee`. I will soon explain how this modeling works; first we will consider how to characterize complexity.

### Complex program capabilities

Can you think of some program capabilities that necessitate complexity? How might this complexity appear in code?

:::{.wide-list-items}
* When a program starts, it may **read configuration** from the environment, a database, or files. Configuration may also be a continuous process at runtime, which affects the entire architecture of the program.
* **Database queries** are surrounded by code that **handles exceptions and recovers from errors**. Some languages encourage a hands-off approach to exception handling, leaving a minefield of potential errors.
* Rows returned by database queries will have an **unknown length**, sort, and cardinality, which imposes special handling when you want _just one_ row returned.
* **External API calls require async IO** in order for programs to be performant. Async IO _infects_ entire codebases requiring its capability.
* **External API calls can fail for any reason.** Different types of failures may indicate aborting the associated operation or retrying it. As in database queries, exception handling may not be encouraged and leave open the possibility of unexpected errors at runtime.
* **External input and API responses** are validated and transformed into domain objects. Sometimes the responses returned are in an unexpected format, requiring **meticulous validation logic** and **recovery** from malformed responses.
* **Error handling** typically leverages exceptions. Each case where they occur may remain unknown until they're thrown at runtime.
* Logging libraries are used to **report errors** and might require file system or network access. Logging may necessitate async IO itself so that the program remains performant.
* **Metrics** are gathered with libraries that require network access, possibly also async IO.
* **Feature flags and ramps** need to be queried in real-time. Error handling modes must be provided in the event that a behavior-modifying query fails.
* **A/B testing** requires deterministically persisting identities' sessions within their assigned variants.
* **Retries** using exponential back-off must retain their previous retry interval and apply a random jitter in order to calculate their next one.
:::
### A model for characterizing complexity

Complexities can be characterized in terms of **effects**. Each of the following effects center on some dimension of nondeterminism:

:::{.wide-list-items}
* **Time and Async** as in asynchronous operations against disk access and network boundaries, such as API calls, database queries, or streaming from files. Long-running operations may be asynchronous without requiring IO.
* **IO** as in synchronous operations against disk access and network boundaries.
* **Presence** as some functions may not produce anything for some inputs.
* **Length** as database queries return zero or many rows, streaming data over the network implies infinitely many of "something", and long-running programs act as consumers of an infinite input.
* **Validation** requires sanitizing and validating program inputs or permissively-structured outputs from applied functions or network calls.
* **Implicit input** in the form of configuration, feature flags and ramps, A/B test assignment, or other inputs that aren't themselves explicitly provided as part of an API boundary or client interaction.
* **Implicit output** in the form of logging and metrics, as well as exceptions and other faults.
* **State** representing change over time and how it propagates across a program's architecture. Implicit inputs and outputs together constitute a form of state.
:::

The actual list of effects is innumerable, but these ones are common.

### Demonstrating effects in code

Take for example this Java code from a hypothetical payroll system:

:::{.numberLines}
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
* **Presence** as an `Employee` may not be found or their `Paycheck` not calculated.
* **Validation** where the `"SUCCESS"` response from the `achClient` is the only check performed. Other responses are simply not handled.
* **Implicit input** where the company's bank routing and account numbers are read from an external location.
* **Implicit output** where logging occurs but swallows errors, resulting in opaque `false` return cases. Exceptions also bubble up from database and network operations.

Fortunately there are ways to model sets of these effects and contain the scope of their impact so that code is less complex. Rewriting the above code using an effect model may look like this:

:::{.numberLines}
```scala
def runPayroll(employeeId: Long): PayrollEffect[()] =
  for {
    employee <- employeeRepo.find(employee).getOrFail(EmployeeMissing)
    paycheck <- payCalc.calculatePaycheck(employee).getOrFail(PaycheckMissing)
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

This code looks similar, doesn't it? What hides between the lines here is a custom effect model `PayrollEffect` that abstracts away the effects of presence, async IO, and implicit input and output. This code is thus unburdened of most complexity, and makes the rest of it easier to work with.

You may notice that there are no return statements for error cases: the flow of execution through this code is not performed procedurally. Instead flow is controlled by declaring where errors occur and the main operation short-circuits itself should any inner operation fail.

This abstraction of effects allows for safer code that better focuses on the business logic at-hand. _But how are abstractions over effects created?_

Previously I described programs as a case of function composition: `h := g ∘ f`. Functors, applicatives, and monads address a _special case_ of function composition, the **composition of functional effects**. In the following section I will demonstrate abstracting the elementary effects of presence.

## Contexts and effects

Let me start with an abstract concept: **Context**. What is a context? _A context is a setting where stuff exists under some circumstances._ Stuff such as instances of term `A` in the _context_ of `F[A]`.

> Contexts in Scala may be neatly represented by a letter and brackets such as `F[_]` read as _context F_ with an underscore when the type of the term is unspecified, and `F[A]` read as _F of A_ when the term is known to be of type `A`. Other letters work nicely of course, as do concrete names, as in `Option[Int]` or _Option of Int_.

Each kind of context models a set of **effects**. Contexts thus represent a concrete, typed API describing how their terms may be produced. Names of contexts can hint at the effects they model, and with some intuition you may be able to figure out what each context’s effects may be.

### Common contexts and some of their effects

**Elementary contexts representing dimensions of presence:**

* `Option[A]` or _Option of A_: Presence, absence, or _optionality_ of some instance of term `A`. Getting the term `A` when there is no instance causes a fault.
* `Either[X, A]` or _Either of X and A_: Conventionally treated as _either_ term `A` if valid _or_ term `X` if invalid. Getting the wrong term causes a fault.
* `List[A]` or _List of A_: _Unknown length_, sort, and cardinality of term `A`. Getting an `A` from an empty list or from beyond the end of it causes a fault.
* `NonEmptyList[A]` or _NonEmptyList of A_: _At least one_ of term `A` with an unkown sort and cardinality. The first `A` is guaranteed to be present.
* `Id[A]` or _Identity of A_: This _is_ `A` and is guaranteed to be present.
* `Set[A]` or _Set of A_: A set of _distinct instances_ of `A` whose size is unknown.
* _Many data structures are used to model different forms of presence._

**Contexts representing side-effects:**

* `IO[A]` or _IO of A_: Externally-acquired term `A`.
    * The instance may be here already, eventually, or never. Execution awaits the instance until it arrives or a fault occurs.
    * The instance is acquired externally from the program via processes that rely on and are influenced by side effects.
    * Faults may interrupt the acquisition of the instance.
    * Getting the instance causes a fault if acquisition has failed.
* `Future[A]` or _Future of A_: Eventually-acquired term `A`.
    * The instance may be here already, eventually, or never.
    * The instance may be acquired externally from the program via processes that rely on and are influenced by side effects.
    * The instance may be produced via long-running processes.
    * Acquisition of `A` be suspended, awaited, or run in parallel with any number of other `Future[_]`'s. This allows for task management against limited computing resources.
    * Faults may interrupt the acquisition of the instance.
    * Getting the instance causes a fault if acquisition is incomplete or has failed.

**Contexts representing implicit input and output:**

* `ReaderT[F, A]` or _Reader Transformer of context F and A_: Reading of implicit inputs in the context of `F[_]`.
    * Propagation of configuration usually leverages this effect by abstracting how instances of configuration values are acquired.
* `WriterT[F, A]` or _Writer Transformer of context F and A_: Writing of implicit outputs in the context of `F[_]`.
    * Logging usually leverages this effect by abstracting how such output leaves the program.
* `StateT[F, A]` or _State Transformer of context F and A_: Modifying implicit inputs and outputs in the context of `F[_]`.
    * Models state changing over time in an otherwise-immutable context.
* _These contexts are higher-order in the term of `F`. They are listed here for illustration but won't be explored in this post._

Each of these contexts have two shared qualities in that they _produce_ some term `A` and that their effects determine _how_ term `A` is produced. But with such a wide array of effects, and with so little overlap between each context, how can instances of term `A` be consumed in a manner unburdened of complexity?

In order to generalize contexts, the key differentiator between them must be abstracted: **effects**. By shedding effects as an _implementation detail_, the production of term `A` remains a shared characteristic that can be accessed by a general interface. By what interface would term `A` be consumed?

## Consuming terms produced by contexts

**For any context `F[_]`, it produces some term `A`.** If you have a function `f: A => B`, how would you apply it to the term produced by the context `F[A]`? That would require _lowering_ the term. Specifically, you can’t apply the following function directly to the context:

:::{.numberLines}
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

Recall from the previous section, contexts share two qualities: that they produce a term, and that they have effects which determine how the term is produced. After abstracting effects, contexts do not expose an obvious shared interface to extract the term. Consider the following definitions for `Option[A]`, `Either[X, A]`, and `List[A]`:

**`Option[A]`** is the effect of presence, absence, or _optionality_ of an instance of term `A`:

:::{.numberLines}
```scala
sealed trait Option[+A] {
  def get: A = throw new Exception()
  def isSome: Boolean = this != None
}
case class Some[+A](override val get: A) extends Option[A]
case object None extends Option[Nothing]
```
:::

**`Either[X, A]`** by convention is the effect of _either_ success with term `A` _or_ failure with term `X`:

:::{.numberLines}
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

**`List[A]`** is the effect of _unknown length_, sort, and cardinality of instances of term `A`:

:::{.numberLines}
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

Both `Option[A]` and `Either[X, A]` have roughly the same shape in that there either is or isn’t an instance of the desired term `A`. Because of this, a _lowering_ operation `extract: F[A] => A` is possible as it means the same thing between both of them: `extract` either gets the existing instance of the term `A` or it faults. In object oriented programming, `Option[A]` and `Either[X, A]` might expose such an interface:

:::{.numberLines}
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

How do you `extract` the term `A` from a `List[A]` such that it means the same thing as in `Option[A]` and `Either[X, A]`?

As in `Option[A]` and `Either[X, A]` there is a notion of the presence or absence of an instance of the term `A`, but presence in `List[A]` implies _one to many_ instances. A solution inspired by object oriented programming might change the interface thusly:

:::{.numberLines}
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

_This interface however is not coherent._ Faulting on absence is preserved as a behavior in `Option[A]` and `Either[X, A]`, but in `List[A]` however `extract` could return an empty `Seq[A]`. Absence can be preserved in `List[A]` by modifying `extract` to return a `NonEmptyList[A]` instead, as this has the effect of always having _at least one_ instance of term `A`. You're still stuck with an unknown length of instances, though.

This interface essentially transforms these contexts into `NonEmptyList[A]` and imposes its specific complexity on all of your code. You would probably be very unhappy using it.

What about implementing `extract` for `Future[A]`? When applied to `Future[A]`, the `extract` function is by its own signature a blocking call. You want your dependency on `A` to be properly asynchronous.

## Motivating functors as a design pattern

`Option[A]`, `Either[A]`, `List[A]`, `Future[A]`, and `IO[A]` all have different effects that determine how term `A` is produced. You must follow an axiom from object oriented programming: _abstract what changes_. Therefore you have to shed effects as an implementation detail. How might that impact lowering the term `A`?

You may be unsatisfied by the answer: _extraction cannot be generalized_. All you know is that there is term `A`. You don't know whether an instance is present, how many of it there are, whether it's here already, or if it's arriving later. How do you consume term `A` when you know nothing about its instances' nature of existence? Functors solve this problem.

**Functors** are abstractions that allow you to consume term `A` within the context of `F[A]`. A functor is a simple structure: a single function `map: F[A] => (A => B) => F[B]`. Functors in Scala may be formally defined using the `Functor` typeclass:

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

What `map` does is _lift_ the function `f: A => B` into the context so that it behaves as `F[A] => F[B]`, giving back `F[B]`.

This _lifting_ of functions that `map` performs is _coherent across contexts_. With `map` you can apply `f: A => B` to any `List[A]` just as you can any `IO[A]`. The results of both operations are predictable: your `List[A]` maps to `List[B]` and your `IO[A]` maps to `IO[B]`.

How would you consume the term produced by `Future[A]` or `Option[A]`? You would also use a functor.

What this enables is your function `f: A => B` to be used with any functor regardless of its specific effects. Your function `f: A => B` is immediately reusable across all contexts and can be unit tested in isolation  of effects.

### Why does the `map` function return `F[B]`?

Recall that contexts generally do not permit extracting terms. Think for a moment: what does extracting the term mean if you’re using a context like `Option[A]`? What about `Future[A]`? _Would their effects change how extraction of the term would work?_

Extracting _the_ term from `List[A]` flatly doesn't make sense as it has the effect of an unknown number of instances.

Because there is no way to generalize extracting a term from a context, functors don’t allow you to operate on contexts in such a way that an instance of the term can "escape" them.

Most importantly, by keeping all operations against terms within their context, the context’s specific effects remain abstracted. Asynchronous operations with `Future[A]` remain asynchronous, the length of `List[A]` remains unknown, and `Option[A]` may or may not be present.

Functors thus _preserve structure_ by keeping operations within the context. For example, applying `map` on a `List[A]` or `BinaryTree[A]`:

```{.nowrap .numberLines}
def f(n) = n * 2

[1, 2, 3, 4] -> map f -> [1, 4, 6, 8]

      4                        8
    /   \                    /   \
   2     6   -> map f ->   4       12
  / \   / \               / \     /  \
 1   3 5   7             1   6  10    14
```

The application of `map` produces two new and identifiable `List[B]` and `BinaryTree[B]`s. The values internally change, as they have been mapped-over by a function, and `BinaryTree[B]` specifically may re-balance itself. What matters here is that the structures are coherent and identifiable.

Compare with iteration using a `for` loop:

```{.nowrap .numberLines}
[1, 2, 3, 4] -> for(x) -> x={1, 2, 3, 4, 5, 6, 7}

      4
    /   \
   2     6   -> for(x) -> x={1, 2, 3, 4, 5, 6, 7}
  / \   / \
 1   3 5   7
```

Iteration as a form of lowering _destroys structure_. In order to get a `List[B]` back you would have to rebuild it yourself and any structural guarantees must be manually implemented following _procedural_ steps.

This isn't to say that functional programming is only about iteration and loops versus `map`. Can you think of other operations that might destroy structure? For example, _if you use an `await` operation on a `Future[A]` you will destroy its asynchronous structure_ and potentially harm the performance of your program.

> Where the type of your context is known, it may make sense to pull the structure apart to extract the term. A common use case with `Option` is to extract the term if it is present and provide a default instance otherwise.

### Context `F[A]` must produce some term `A`

Recall my statement from above: _"For any context `F[_]`, it produces some term `A`."_ If a context were guaranteed to have an instance of a term `A` then you should be able to consume it with your function `f: A => B`, right?

But what if there’s nothing there, as in there are _zero_ instances of term `A`? Can you do anything? When a context has this kind of effect, a sort of "nothing here" or _void_ effect, then the `map` function above doesn’t do anything because there isn’t anything to do. If you try to `map` a void `F[A]` with `f: A => B` then it returns a void `F[B]` as there’s "nothing here". _It does this without having used `f: A => B` to get there._

This behavior is referred to as _short-circuiting_ and it is a key feature of functors, applicatives, and monads that encode some notion of void. It is exploited in particular to enable _control flow_ and _error handling_, which I will expand on in later parts.

> `Option[A]` and `Either[X, A]` are two prime examples of short-circuiting in functors. An `Option[A]` will only `map` an instance of its term `A` if it is present, and an `Either[X, A]` will only `map` if an instance of the desired term `A` is present.
>
> In contrast, the `Id[A]` context has the effect of the _identity_ of term `A`. To put it plainly, `Id[A]` _is_ the instance of term `A`. As the instance is always present, this context never short-circuits.

## Implementing a functor in Scala

Each context of course must provide its own implementation of `map` in order for it to be used as a functor. Functor implementations in Scala are provided via typeclasses, and any type that has the shape `F[_]` may become a functor by implementing the `Functor` typeclass from above:

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

Instances of the `Functor` typeclass simply implement this trait and make themselves available implicitly:

:::{.numberLines}
```scala
object FunctorInstances {
  implicit val optionFunctor = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa match {
        case Some(x) => Some(f(x)) // apply map
        case None    => None // void
      }
  }
  implicit def eitherFunctor[X] = new Functor[Either[X, *]] {
    def map[A, B](fa: Either[X, A])(f: A => B): Either[X, B] =
      fa match {
        case Left(x)  => Left(x) // void
        case Right(a) => Right(f(a)) // apply map
      }
  }
  implicit val listFunctor = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa match {
        case a :: at => f(a) :: map(at)(f) // apply map, recurse
        case Nil     => Nil // void
      }
  }
}
```
:::

These three instances for `Option[_]`, `Either[X, _]`, and `List[_]` show remarkable similarities, and this isn’t uncommon across functors for most data structures. Note in particular how `List[_]` is recursive, with the base case `Nil` representing void. Implementations of `Functor` are more complex in contexts such as `IO[_]` and `Future[_]` because they are managing side effects. _What is key is that the complexities imposed by each of these contexts are completely abstracted_, allowing function `f: A => B` to operate unburdened by effects with a focus on specific program logic.

Can you see how functors enable control flow and short-circuiting? The void cases are the specific branches of logic that enable this. If there’s "nothing here", then they don’t do anything. In the specific case of `Either[X, _]`, `Left[X, _]` may be used to carry some error state in its term `X`. This satisfies the effect of _either_ `A` for success _or_ `X` for failure.

> You can think of `Right`'s term as being "the right instance you want" because it's "correct". _Right?_ This pun is why `Either` is conventionally leveraged for the effect of correct vs. incorrect or success vs. failure.

Contrasting with effects that encode some notion of void, here's what the `Functor` instance for `Id[_]` looks like:

:::{.numberLines}
```scala
type Id[A] = A

object FunctorInstances {
  implicit val idFunctor: Functor[Id] = new Functor[Id] {
    def map[A, B](fa: Id[A])(f: A => B): F[B] =
      f(fa) // map always applies!
  }
}
```
:::

### Using functors as a general abstraction

Defining a `fizzBuzz` function that uses a functor looks like this:

:::{.numberLines}
```scala
import Functor

def fizzBuzz[F[_]: Functor](context: F[Int]): F[String] =
  Functor[F].map(context) {
    case x if x % 3 == 0 && x % 5 == 0 => "fizzbuzz"
    case x if x % 3 == 0 => "fizz"
    case x if x % 3 == 0 => "buzz"
    case x => x.toString
  }
```
:::

And then `fizzBuzz` may be used for all contexts implementing the `Functor` typeclass:

:::{.numberLines}
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

By using functors, the `fizzBuzz` function is free to focus on its specific program logic:

* Produce "fizz" when `x` is divisible by `3`
* Produce "buzz" when `x` is divisible by `5`
* Produce "fizzbuzz" when `x` is divisible by both `3` and `5`
* Produce `x` as a `String` otherwise

At no point is `fizzBuzz` burdened by the effects of the context it executes against.

## Functors are universal

You might be thinking that lists and arrays in the wild already have a `map` operation available. `Promise`s in JavaScript also have their own `map`. You've been using functors for a while and never realized!

Functors as a formal abstraction API, such as in the Scala `Functor` typeclass, find their strongest use in cases where the concrete type of the context is unimportant. However, you might observe that the _shape_ of functors appears in many places without being _called_ a functor. Using `map` on a list conceptually performs the same operation as on both arrays and `Promise`s. Other structures defining `map` operations may be functors because _functors arise from settings where stuff exists under some circumstances_.

### Functor laws

That so many functors appear in the wild is no coincidence. Functors even have a [formal definition](https://en.m.wikipedia.org/wiki/Functor) within the higher math of [category theory](https://en.m.wikipedia.org/wiki/Category_theory). This definition can be applied to any structures that have the shape of a functor to assert that they behave as functors.

In order to be a functor, a context defining a `map` function must satisfy two laws:

1. Preservation of identity functions:

    ```{.scala .numberLines}
    context.map(id) == id(context)
    ```

2. Preservation of function composition:

    ```{.scala .numberLines}
    context.map(g ∘ f) == context.map(f).map(g)
    ```

Here are the two laws applied against Scala's builtin `List` type, which defines its own `map` operation:

:::{.numberLines}
```scala
def preservesIdentityFunctions(list: List[Int]): Unit =
  assert(list.map(identity(_)) == identity(list))

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

Functors need only obey these two laws. These laws assert that functors compose in the same manner as functions `f` and `g` do in `h := g ∘ f`. Functors thus _compose functional effects_ and may be abstractly regarded as the _context of effects_.

Because of this rigorous definition, functors as a design pattern represent a concept that _transcends_ codebases and languages. In contrast, design patterns as they are realized in object-oriented programming are mere idioms to be relearned between codebases written even in the same language.

What this means, ideally, is that `map` is the same regardless of context.

## Building upon functors

In this post I introduced functors as an elementary abstraction which permit you to consume term `A` within some context `F[A]` via `map`:

:::{.numberLines}
```scala
def map[A, B](fa: F[A])(f: A => B): F[B]
```
:::

This simple abstraction is enabling on its own, as it frees the logic in function `f` from the burden of complexity present in the context of `F[_]`. However this is only an _elementary_ abstraction. Consider for a moment: with a functor you are able to work against the term produced by a single context. But what happens if you require terms produced from two or more contexts?

Take for example these two instances of the context `F[_]` and the function signature for `combine`:

:::{.numberLines}
```scala
val fa: F[A]
val fb: F[B]

def combine(x: A, y: B): C
```
:::

How do you apply `combine` to the terms `A` and `B` produced by the contexts?

In my next post, we will explore how **applicatives** enable working within two or more contexts at the same time, as well as the many ways that you will be able to exploit this capability.

[Defensive programming]: https://en.wikipedia.org/wiki/Defensive_programming
