---
title: "Embracing Nondeterminism Part I: Contexts and Effects"
description: Abstracting nondeterminism and complexity by modeling effects as first class concepts in programs.
author: Logan McGrath
comments: true
date: 2022-01-24T17:14:03-0800
published: 2022-03-15T09:20:00-0700
tags: functional programming, programming, scala, design patterns
layout: post
thumbnail: /images/tags/functional-programming/functional-grass-128x128.png
twitter:
  image: /images/tags/functional-programming/functional-grass-512x512.png
og:
  image:
    url: /images/tags/functional-programming/functional-grass-512x512.png
    alt: Abstracting nondeterminism and complexity by modeling effects as first class concepts in programs.
stylesheets:
  - css/posts/embracing-nondeterminism-part-1.scss
---

Have you ever received an unexpected `null` reference? Have you ever written a function to validate some input only to have it turn into spaghetti over time? How do you anticipate exceptions and protect against them at runtime?

<!--more-->

{{add "scrollShadowsXSelectors" "#img-tree-functor .content"}}

Significant portions of program logic exist to address specific cases imposed by nondeterminism and unknown quantities. Have you ever written some code only to find out it does something unexpected when it's running in production? To protect against unexpected behavior you first have to be aware that an operation may return something unexpected, such as a `null` reference, invalid data, or throw an exception, and then write code that anticipates and recovers from such cases.

_[Defensive programming][]_ as a practice tries to protect against errors and unknowns by preempting how they might occur. However anticipation of errors and unknowns rests entirely on _[tacit knowledge][]_ and imposes complex code to handle and recover from them. _This complex code draws focus from writing the business logic that drives the value of programs_.

In this post I will provide **effects** as a model for characterizing nondeterminism and unknowns in programs. I will introduce a **design pattern** to abstract complexity using this model.

## Conventions

**This post assumes familiarity with Scala code**. I will provide Scala code for concrete examples and note where they are different. Abstract examples will employ notation that looks like **"math"**.

Terminology will leverage common functional programming vocabulary.

Where there is conceptual overlap with object oriented programming, I will leverage those terms to drive the intent behind abstractions.

### How to read "math"

**Uppercase letters** and words starting with uppercase letters are names of _types_. For example **A** reads as _"A"_ and means _"type of A"_.

**Lowercase letters** and words starting with a lowercase letter are functions or variables. **f** reads as _"f"_ and means _"function f"_ or _"variable f"_.

**A => B** reads as _"A to B"_ and means _"function type of input type A mapped to output type B"_. Functions types can map any kind of input type to any other kind of output type, including function types.

**F[_]** reads as _"context F of underscore"_ or _"context F"_. Contexts are a type constructor[^hkt] that take types as an argument and produce another type. They become proper types when their _underscore_ is replaced by another type, as in **F[A]** or _"context F of A"_.

**()** a pair of parentheses, reads as _"unit"_ and means _"nothing"_ or _"void"_. It is both a type and a solitary value.

**f : A => B** reads as _"f is A to B"_ or _"function f has type of A mapped to B"_.

**fa : F[A]** reads as _"fa is F of A"_ or _"variable fa has type of context F of A"_.

**h = g ∘ f** reads as _"h is g after f"_ or _"h is defined as function g composed after function f"_. Composition is described in [Terminology](#terminology).

### Terminology

**Functors** are a programming pattern explored in this post. Think of them as an analog of a **design pattern** found in object-oriented programming.

**Expressions** are values that are described by some type `A`.

**Functions** are a _special case_ of expressions that map some type `A` to some type `B`. They are described by `A => B`.

**Terms** are identifiers naming unitary or indivisible variables and types.

* For example, in the declaration:

    ```.scala
    def map[A, B](fa: F[A])(f: A => B): F[B]
    ```

    The variable terms are `fa` and `f`, and the type terms are `A` and `B`.

**Contexts** describe circumstances within which their contents are found. They are noted using `F[_]` when their contents are unspecified, and `F[A]` when their contents are known to be of type `A`. They are more concretely defined in later sections.

**Lifting** describes injecting a term `A` into a context `F[_]` such that `lift : A => F[A]`.

A **Lifted** term or expression already has the form `F[A]`.

**Lowering** describes extracting a term `A` from a context `F[A]` such that `lower : F[A] => A`.

**Composition** describes chaining the output of a function `f : A => B` to the input of a function `g : B => C` such that a new function `h : A => C` may defined as `h = g ∘ f`.

* This algebraic notation demonstrates how function `g` is applied _after_ function `f` is applied to the argument `x`:

  ```
  h(x) = g(f(x))
  ```

* Compare with this definition in Scala which declares the types of the functions:

  ```scala
  val f: A => B = _
  val g: B => C = _
  val h: A => C = x => g(f(x))
  ```

## Complexity in programming

### Pure and impure functions

Programming broadly consists of two categories of functions:

  1. **Pure functions** which produce the same result for the same argument, for all arguments. They are _deterministic_.
  2. **Impure functions** which produce different results for the same argument, for any argument. They are _nondeterministic_.

**Nondeterminism** arises from outputs dependent on factors other than input to a function. These factors are referred to as **side effects** as **implicit input**. In addition, functions may produce **side effects** as **implicit output**.

Both categories of functions may produce their results in **unknown quantities** along any measurable dimension, such as presence, length, or validity of their result. These quantities require specific knowledge of a given input in order to be known with certainty in the result. Unknown quantities are nondeterministic in impure functions as they are particularly influenced by side effects.

### Manifesting complexity

#### Nondeterminism

Nondeterminism as a dependence on factors other than initial state and input arises when a function `f : A => B` maps to a different member of `B` for any number of times `f` has been applied to the same member of `A`. This means that `f` is influenced by **side effects** that occur independent of its signature.

An extreme example of a nondeterministic function is the random number generator `rng : () => Int` as it maps the solitary unit value `()` to all members of type `Int`. This mapping is influenced by some side effect or _implicit input_ which is external to the function's signature of `() => Int`:

:::{.numberLines}
```scala
println(rng())
// 1729
println(rng())
// 87539319
println(rng())
// -2147483648
println(rng())
// 1337
println(rng())
// 42
```
:::

Nondeterminism is significant in that operations may be unpredictable, and that no operation in particular may be reproducible.

#### Unknown quantities

Unknown quantities along measurable dimensions arise in functions returning types such as lists, potentially `null` references, or validation results. These outputs have unknown length, presence, and validity respectively, and require specific handling for undesirable cases. _Even pure functions produce results having unknown quantities._

* A simple example is a function `toBits : Int => List[Boolean]` where the known quantity of `Boolean` bits returned requires specific knowledge of the input argument.
* Hashmap lookups may or may not return a value associated to a given key. Unless you have specific knowledge of the key used to lookup a value, you don't have any guarantee whether the value actually exists.

Both of these operations are pure functions and are deterministic, but their results are contextualized by length and presence. Any unknown quantity along some measurable dimension requires specific handling in code. This means that in addition to writing code that handles the desired case of an operation, code must be specifically written for each dimension that exhibits unknown quantities.

Side effects as **implicit output** include **faults** such as the _divide by zero_ error and thrown exceptions. They impose an additional layer of protection to prevent or recover from them. Exceptions are fully nondeterministic as there is no single input that guarantees that an exception will never be thrown, as some side effect as an **implicit input** may influence the outcome.

> In contrast with most faults, a _divide by zero_ error only occurs if the input divisor is `0`. The additional check for `0` that division sometimes requires is not considered complexity in practice.
>
> Running out of memory will throw an exception even in pure functions. Exceptions are truly nondeterministic and you must choose when and how to handle their cases. Hopefully you know ahead of time where you will need to do so.

In addition to being side effects, exceptions may be reasoned about as a _dimension of success or failure_ in an operation. This quantity is unknowable ahead of time and highly dependent upon implicit input.

Concurrency and asynchronous operations are driven entirely by side effects. Asynchronous operations have an unknown temporal quantity that imposes costly specific handling, as execution must wait for operations to complete. Support for asynchronous operations requires runtimes to manage limited computing resources and scheduling of tasks, forming an entire system within a program.

#### Relating nondeterminism and unknown quantities

Side effects enable nondeterminism which influences unknown quantities in the results of operations. Undesired cases along dimensions such as length, presence, validity, success, and time require specific handling in addition to the code to handle the desired output case of operations. This specific handling creates complexity and draws engineering focus away from business logic. Yet side effects drive the business value of programs in the real world, which requires that we embrace nondeterminism and unknown quantities.

_How might complexity in programs be reduced if they must also be driven by side effects?_

### Implied complexity

Given a function `f : A => B` and another `g : B => C`: a third function `h : A => C` may be composed of `h = g ∘ f` or _h is g after f_. Programs may be modeled as a function `program : Input => Output`, where `program` is composed of innumerable smaller functions, together in concert building the necessary mappings to generate the desired program output. However composition is not so simple in programs as the results of some operations may not produce the desired input for subsequent operations.

Functions in real world programs must internally interact with implicit inputs and outputs _not present_ in the program's signature of `program : Input => Output`. An employee payroll system for example must make database queries and integrate with banks. These implicit inputs and outputs have **effects** which determine how their associated functions produce their desired outputs. For example, database queries return nondeterministic responses of unknown length and an error might occur when performing a direct deposit. _These effects determine how and whether payday is successfully produced._

Errors and unknown quantities as **effects** of these operations are opaque in functions modeled as simple input to output, as in `getEmployee : Int => Employee`. The signature of this function requires _[tacit knowledge][]_ of what effects may determine how an `Employee` is produced from it. For example:

1. An associated `Employee` may not be found.
2. The returned `Employee` may change between applications of the same `Int` employee ID.
3. The database or network may fault and the function generates an exception that must be handled.

You might be thinking that these cases are a given when working with database code, but that knowledge only comes with experience. These cases are **effects** which describe the circumstances under which an `Employee` may be produced and can be modeled accordingly as part of the typed API of `getEmployee`. I will soon explain how this modeling works; first we will consider how to characterize complexity.

### Operations producing undesired cases

Can you think of some program operations that produce undesired cases in addition to their desired output? How might these cases cause code to become complex?

:::{.wide-list-items}
* When a program starts, it may **read configuration** from the environment, a database, or files. Reading configuration values may be blocking or asynchronous, and some configuration keys may not have associated values.
* **Database queries** produce an unknown length of rows, and may fail due to incorrect syntax, a database error, or network fault. Receiving an unknown length of rows imposes specific handling if all you want is _just one row_ and any guarantees that you're retrieving the correct one must be manually enforced.
* **API calls** may be blocking or require async IO, they may fail for any reason, and the data they return may or may not conform to an expected structure.
* **Task management** of long-running operations requires async IO and managing limited computing resources. Tasks may fail for any reason or they may never complete. You have to wait for the result to be available in order to use it.
* **Error handling** when an operation fails may require aborting the operation and returning a default case, or retrying it. Exceptions are easy to rethrow or bubble-up the stack causing unexpected errors at runtime.
* **Retries** using an [exponential back-off][] strategy must retain their previous retry interval and apply a random jitter in order to calculate their next interval.
* **User-provided input**, **server requests**, and **API responses** must be validated to assert that they conform to an expected structure and then validated for semantic content. Invalid data must be handled.
* Some operations **produce logs and metrics** as a secondary output. All components requiring logs and metrics carry an extra dependency in order to support them, and their associated operations may be blocking or require async IO.
* **Feature flags and ramps** need to be queried in real-time. These queries may be blocking or asynchronous, and they may fail for any reason. Presentation of default variants in the case of failure must be reliably persisted to the current session, yet this operation may fail as well.
* **A/B testing** requires reliably determining eligibility of users' sessions per variant and persisting the associated assignments. Either of these operations may fail and presenting a default variant itself requires persistence. Retrieving assigned variants must also be reliable.
:::

### A model for characterizing complexity

Complexities can be characterized in terms of **effects**. The operations listed above impose complexity because they feature the effects of:

Presence
  : Some configuration keys may not have an associated value.
  : Some database queries expect one row to be returned, but instead no rows may be found.

Length
  : Database queries may return zero or many rows.
  : Collections don't have a totally guaranteed or fixed size.

Validity
  : User-provided input, server requests, and API responses all require validation before they may be used.

Success
  : Some operations fail with an exception, which can hide potential error cases.
  : Some operations may be aborted.

IO
  : Operations are dependent on external systems' state as **implicit input**
  : Operations can affect external systems' state as **implicit output**.
  : Operations may be concurrent, paused, or interrupted.
  : Operations may block execution of the calling operation.
  : Interacting with concurrency primitives may block execution and produce nondeterministic outputs.

Time
  : Task management makes no guarantees how long any particular task will take to run.
  : API calls require an indeterminate amount of time.
  : Database queries take time to run and return rows as they are found.
  : Operations dependent on async output must await the result.

State
  : Retry strategies must track previous state in order to calculate their next retry periods.

The actual list of effects is innumerable, but these ones are common.

### Demonstrating effects in code

Take for example this Java code from a hypothetical payroll system:

:::{.numberLines}
```java
class PayrollRunner {

  EmployeeRepo employeeRepo;
  BankAchClient achClient;
  PayCalculator payCalc;

  boolean runPayroll(long employeeId) {
    try {
      Employee employee = employeeRepo.find(employeeId)
      if (employee == null) {
        Logger.error("Missing employee " + employeeId);
        return false;
      }
      Paycheck paycheck = payCalc.calculatePaycheck(employee);
      if (paycheck == null) {
        Logger.error("No paycheck for " + employeeId);
        return false;
      }
      String companyAcctNo = PayrollConfig.get("companyAcctNo");
      String companyRoutingNo = PayrollConfig.get("companyRoutingNo");
      String response = achClient.depositPaycheck(companyAcctNo, companyRoutingNo, paycheck);
      return response.equals("SUCCESS");
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
}
```
:::

The code above demonstrates effects in the dimensions of:

* **IO** as database queries and network calls are implicitly synchronous. Configuration acts as an implicit input.
* **Presence** as an `Employee` may not be found or their `Paycheck` not calculated. Configured banking information may not have associated values.
* **Validity** where the `"SUCCESS"` response from the `achClient` is the only check performed. Other responses are simply not handled.
* **Success** where logging occurs but swallows errors, resulting in opaque `false` return cases. Exceptions also bubble up from database and network operations.

_These effects are not obvious from the code above_. I have rewritten it to handle all undesirable cases to highlight where complexity exists:

:::{.numberLines}
```java
class PayrollRunner {

  EmployeeRepo employeeRepo;
  BankAchClient achClient;
  PayCalculator payCalc;

  void runPayroll(long employeeId) throws PayrollException, MissingConfigException {
    Employee employee;
    try {
      employee = employeeRepo.find(employeeId);
      if (employee == null) {
        Logger.error("Missing employee " + employeeId);
        throw new MissingEmployeeException(employeeId);
      }
    } catch (SQLException exception) {
      throw new PayrollException("Error looking up employee " + employeeId, exception);
    }

    Paycheck paycheck = payCalc.calculatePaycheck(employee);
    if (paycheck == null) {
      Logger.error("No paycheck for " + employeeId);
      throw new PayrollMissingException(employeeId);
    }

    String companyAcctNo = PayrollConfig.get("companyAcctNo");
    if (companyAcctNo == null) {
      throw new MissingConfigException("companyAcctNo");
    }

    String companyRoutingNo = PayrollConfig.get("companyRoutingNo");
    if (companyRoutingNo == null) {
      throw new MissingConfigException("companyRoutingNo");
    }

    String response;
    try {
      response = achClient.depositPaycheck(companyAcctNo, companyRoutingNo, paycheck);
    } catch (AchException exception) {
      throw new PayrollException("Failed to deposit paycheck for employee " + employee, exception);
    }
    if (!response.equals("SUCCESS")) {
      throw new DepositPaycheckException("Received error response when depositing paycheck for employee " + employeeId + ": " + response);
    }
  }
}
```
:::

There's a large amount of complexity in this code due to various effects. In order to make clear all effects and where they occur, I leveraged checked exceptions and all exceptions that are known to be thrown by other functions are translated into exceptions representing the domain of this operation. As all false cases effectively communicated no information I replaced them with exceptions typed according to the reason for failure. The operation now returns void as it is side-effecting and a specific return for success would be superfluous.

There are a number of checks along the dimension of presence. There's several along the dimension of success. Each operation is dependent upon the success of the operation preceding it, following a _procedurally validated_ imperative flow.

There's a lot of effects just in this code. Fortunately there are ways to model sets of these effects and contain the scope of their impact so that code is less complex. Rewriting the above code using an effect model may look like this:

:::{.numberLines}
```scala
def runPayroll(employeeId: Long): PayrollEffect[()] =
  for {
    employee <- employeeRepo.find(employee).getOrFail(EmployeeMissing(employeeId))
    paycheck <- payCalc.calculatePaycheck(employee).getOrFail(PaycheckMissing(employeeId))
    companyAcctNo <- getConfig("companyAccountNo")
    companyRoutingNo <- getConfig("companyRoutingNo")
    response <- achClient.depositPaycheck(companyAcctNo, companyRoutingNo, paycheck)
      .flatMap {
        case "SUCCESS" => pure(())
        case msg       => fail(DirectDepositError(employeeId, msg))
      }
  } yield response
```
:::

This code looks similar, doesn't it? What hides between the lines here is a custom effect model `PayrollEffect` that abstracts away the effects of presence, async IO, success, and implicit input and output. This code is thus unburdened of most complexity, and makes the rest of it easier to work with.

You may notice that there are no return statements for error cases: the flow of execution through this code is not performed procedurally. Instead flow is controlled by declaring where errors occur and the main operation short-circuits itself should any inner operation fail.

_This abstraction of effects allows for safer code that better focuses on the business logic at-hand._

Previously I described programs as a case of function composition: `h = g ∘ f`. Functors address a _special case_ of function composition, the **composition of functional effects**. In the following sections I will describe how effects are abstracted in order to demonstrate how they compose later.

## Contexts and effects

What is a **context**? _A context is a setting where stuff exists under some circumstances._ Stuff such as instances of term `A` in the _context_ of `F[A]`.

> Contexts in Scala may be neatly represented by a letter and brackets such as `F[_]` read as _context F_ with an underscore when the type of the term is unspecified, and `F[A]` read as _F of A_ when the term is known to be of type `A`. Other letters work nicely of course, as do concrete names, as in `Option[Int]` or _Option of Int_.

Each kind of context models a set of **effects**. Contexts thus represent a concrete, typed API describing how their terms may be produced. Names of contexts can hint at the effects they model, and with some intuition you may be able to figure out what each context’s effects may be.

### Common contexts and some of their effects

**Elementary contexts representing dimensions of presence:**

* `Option[A]` or _Option of A_: Presence, absence, or _optionality_ of some instance of term `A`. Getting the term `A` when there is no instance causes a fault.
* `Either[X, A]` or _Either of X and A_: Conventionally treated as _either_ term `A` if valid _or_ term `X` if invalid. Getting the wrong term causes a fault.
* `List[A]` or _List of A_: _Unknown length_, sort, and cardinality of term `A`. Getting an `A` from an empty list or from beyond the end of it causes a fault.
* `NonEmptyList[A]` or _NonEmptyList of A_: _At least one_ of term `A` with an unknown sort and cardinality. The first `A` is guaranteed to be present.
* `Id[A]` or _Identity of A_: This _is_ `A` and is guaranteed to be present.
* `Set[A]` or _Set of A_: A set of _distinct instances_ of `A` whose size is unknown.

> Many data structures are used to model different forms of presence.

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

> These contexts are higher-order in the term of `F`. They are listed here for illustration but won't be explored in this post.

Each of these contexts have two shared characteristics in that they _produce_ some term `A` and that their effects determine _how_ term `A` is produced. But with such a wide array of effects, and with so little overlap between each context, how can instances of term `A` be consumed in a manner unburdened of complexity?

In order to generalize contexts, the key differentiator between them must be abstracted: **effects**. By shedding effects as an _implementation detail_, the production of term `A` remains a shared characteristic. This opens an opportunity to create a **seam** between an impure function _producing_ the context itself and a pure function _consuming_ instances of the term `A` contained within.

_How do you create this seam?_

## A design pattern for contexts

**For any context `F[_]`, it produces some term `A`.** If you have a function `f : A => B`, how would you apply it to the term produced by the context `F[A]`? That would require _lowering_ the term. Specifically, you can’t apply the following function directly to the context:

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

Recall from the previous section, contexts share two characteristics: that they produce a term, and that they have effects which determine how the term is produced. After abstracting effects, contexts do not expose an obvious shared interface to extract the term. Consider the following definitions for `Option[A]`, `Either[X, A]`, and `List[A]`:

**`Option[A]`** is the effect of presence, absence, or _optionality_ of an instance of term `A`:

:::{.numberLines}
```scala
sealed trait Option[+A] {
  def get: A = throw new Exception()
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
}
case class ::[+A](override val head: A, override val tail: List[A]) extends List[A]
case object Nil extends List[Nothing]
```
:::

### An object-oriented approach to contexts

Both `Option[A]` and `Either[X, A]` have roughly the same shape in that there either is or isn’t an instance of the desired term `A`. Because of this, a _lowering_ operation `extract : F[A] => A` is possible as it means the same thing between both of them: `extract` either gets the existing instance of the term `A` or it faults. In object oriented programming, `Option[A]` and `Either[X, A]` might expose such an interface:

:::{.numberLines}
```scala
trait Extractable[A] {
  def extract(): A
}
sealed trait Option[A] extends Extractable[A] {
  def get: A = throw new Exception()
  override def extract(): A = get
}
sealed trait Either[X, A] extends Extractable[A] {
  def left: X = throw new Exception()
  def right: A = throw new Exception()
  override def extract(): A = right
}
```
:::

#### Generalizing for an unknown length of term `A`

How do you `extract` the term `A` from a `List[A]` such that it means the same thing as in `Option[A]` and `Either[X, A]`?

As in `Option[A]` and `Either[X, A]` there is a notion of the presence or absence of an instance of the term `A`, but presence in `List[A]` implies _one to many_ instances. A solution inspired by object oriented programming might change the interface thusly:

:::{.numberLines}
```scala
trait Extractable[A] {
  def extract(): Seq[A]
}
sealed trait Option[A] extends Extractable[A] {
  def get: A = throw new Exception()
  override def extract(): Seq[A] = Seq(get)
}
sealed trait Either[X, A] extends Extractable[A] {
  def left: X = throw new Exception()
  def right: A = throw new Exception()
  override def extract(): Seq[A] = Seq(right)
}
sealed trait List[A] extends Extractable[A] {
  def head: A = throw new Exception()
  def tail: List[A] = throw new Exception()
  override def extract(): Seq[A] = ???
}
```
:::

_This interface however is not coherent._ Faulting on absence is preserved as a behavior in `Option[A]` and `Either[X, A]`, but `Seq[A]` is allowed to be empty per its definition. Allowing `List[A]` to be empty implies that it should be allowed to return an empty `Seq[A]` from `extract`. In order to preserve faulting on absence, `extract` must return a `NonEmptyList[A]` instead, as this has the effect of always having _at least one_ instance of term `A`. You're still stuck with an unknown length of instances, though.

This interface essentially transforms these contexts into `NonEmptyList[A]` and imposes its specific complexity on all of your code. You would probably be very unhappy using it.

What about implementing `extract` for `Future[A]`? When applied to `Future[A]`, the `extract` function is by its own signature a blocking call. You want your dependency on `A` to be properly asynchronous.

This interface does not generalize for more than the contexts of `Option` and `Either`. A better interface exists.

### Motivating functors as a design pattern

`Option[A]`, `Either[A]`, `List[A]`, `Future[A]`, and `IO[A]` each have different effects that determine how term `A` is produced. You must follow an axiom from object oriented programming: _abstract what changes_. Therefore you have to shed effects as an implementation detail. How might that impact lowering the term `A`?

You may be unsatisfied by the answer: _extraction cannot be generalized_. All you know is that there is term `A`. You don't know whether an instance is present, how many of it there are, whether it's here already, or if it's arriving later. How do you consume term `A` when you know nothing about its instances' nature of existence? Functors solve this problem.

**Functors** are abstractions that allow you to consume term `A` within the context of `F[A]`. Functors are a class of types for which they have defined a single function `map : F[A] => (A => B) => F[B]`. Functors in Scala may be formally declared using the `Functor` typeclass:

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

What `map` does is _lift_ the function `f : A => B` into the context so that it behaves as `F[A] => F[B]`, giving back `F[B]`.

This _lifting_ of functions that `map` performs is _coherent across contexts_. With `map` you can apply `f : A => B` to any `List[A]` just as you can any `IO[A]`. The results of both operations are predictable: your `List[A]` maps to `List[B]` and your `IO[A]` maps to `IO[B]`.

How would you consume the term produced by `Future[A]` or `Option[A]`? You would also use a functor.

What this enables is your function `f : A => B` to be used with any functor regardless of its specific effects. Your function `f : A => B` is immediately reusable across all contexts and can be unit tested in isolation  of effects.

#### Why does the `map` function return `F[B]`?

Recall that contexts generally do not permit extracting terms. Think for a moment: what does extracting the term mean if you’re using a context like `Option[A]`? What about `Future[A]`? _Would their effects change how extraction of the term would work?_

Extracting _the_ term from `List[A]` flatly doesn't make sense as it has the effect of an unknown number of instances.

Because there is no way to generalize extracting a term from a context, functors don’t allow you to operate on contexts in such a way that an instance of the term can "escape" them.

Most importantly, by keeping all operations against terms within their context, the context’s specific effects remain abstracted. Asynchronous operations with `Future[A]` remain asynchronous, the length of `List[A]` remains unknown, and `Option[A]` may or may not be present.

Functors thus _preserve structure_ by keeping operations within the context. For example, applying `map` on a `List[A]` or `BinaryTree[A]`.

:::{#functor-diagrams}
{{imageFigure id: "img-list-functor",
              src: getUrl "images/embracing-nondeterminism/list-functor-512x512.png",
              title: "Applying <code>map</code> to a <code>List[Int]</code>."}}
{{imageFigure id: "img-tree-functor",
              src: getUrl "images/embracing-nondeterminism/binary-tree-functor-1024x340.png",
              title: "Applying <code>map</code> to a <code>BinaryTree[Int]</code>."}}
:::

The application of `map` produces two new and identifiable `List[B]` and `BinaryTree[B]`s. The values internally change, as a function has been applied to them, and `BinaryTree[B]` may re-balance itself accordingly. What matters here is that the structures are coherent and identifiable. Both `List[B]` and `BinaryTree[B]` are created from this operation and the originating `List[A]` and `BinaryTree[A]` still exist in their original state.

Compare with using iteration using a `for` loop in this JavaScript code:

:::{.numberLines}
```javascript
const list = [1, 2, 3, 4, 5, 6, 7]
const square = x => x * x

for (const x in list) {
  const x2 = square(x) // apply function to the current value
  results.push(x2) // append to new list
}
```
:::

Iteration as a form of lowering _destroys structure_. In order to get a `List[B]` back you have to rebuild it yourself and any structural guarantees must be manually implemented following _procedural_ steps.

This isn't to say that functional programming is only about iteration and loops versus `map`. Can you think of other operations that might destroy structure? For example, if you use an `await` operation on a `Future[A]` you will destroy its _asynchronous structure_ and potentially harm the performance of your program.

> Where the type of your context is known, it may make sense to pull the structure apart to extract the term. A common use case with `Option` is to extract the term if it is present and provide a default instance otherwise:
>
> :::{.numberLines}
> ```scala
> val myLookup = map.find("myKey").getOrElse("myDefault")
> ```
> :::

#### Context `F[A]` must produce some term `A`

Recall my statement from above: _"For any context `F[_]`, it produces some term `A`."_ If a context were guaranteed to have an instance of a term `A` then you should be able to consume it with your function `f : A => B`, right?

But what if there’s nothing there, as in there are _zero_ instances of term `A`? Can you do anything? When a context has this kind of effect, a sort of "nothing here" or _void_ effect, then the `map` function above doesn’t do anything because there isn’t anything to do. If you try to `map` a void `F[A]` with `f : A => B` then it returns a void `F[B]` as there’s "nothing here". _It does this without having used `f : A => B` to get there._

This behavior is referred to as _short-circuiting_ and it is a key feature of contexts that encode some notion of void. It is exploited in particular to enable two key features of _imperative programming_, control flow and error handling, which I will expand on in later parts.

> `Option[A]` and `Either[X, A]` are two prime examples of short-circuiting in functors. An `Option[A]` will only `map` an instance of its term `A` if it is present, and an `Either[X, A]` will only `map` if an instance of the desired term `A` is present.
>
> In contrast, the `Id[A]` context has the effect of the _identity_ of term `A`. To put it plainly, `Id[A]` _is_ the instance of term `A`. As the instance is always present, this context never short-circuits.

### Implementing functors in Scala

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

These three instances for `Option[_]`, `Either[X, _]`, and `List[_]` show remarkable similarities, and this isn’t uncommon across functors for most data structures. Note in particular how `List[_]` is recursive, with the base case `Nil` representing void. Implementations of `Functor` are more complex in contexts such as `IO[_]` and `Future[_]` because they are managing side effects. _What is key is that the complexities imposed by each of these contexts are completely abstracted_, allowing function `f : A => B` to operate unburdened by effects with a focus on specific program logic.

Can you see how functors enable control flow and short-circuiting? The void cases are the specific branches of logic that enable this. If there’s "nothing here", then they don’t do anything. In the specific case of `Either[X, _]`, `Left` may be used to carry some error state in its term `X`. This satisfies the effect of _either_ `A` for success _or_ `X` for failure.

> You can think of `Right`'s term as being "the right instance you want" because it's "correct". _Right?_ This pun is why `Either` is conventionally leveraged for the effect of correct vs. incorrect or success vs. failure.

Contrasting with contexts that encode some notion of void, the `Functor` instance for `Id[_]` will always apply the function in `map`:

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

#### Using Scala functors as a general abstraction

Defining a `fizzBuzz : F[Int] => F[String]` function that uses a functor looks like this:

:::{.numberLines}
```scala
def fizzBuzz[F[_]: Functor](context: F[Int]): F[String] =
  Functor[F].map(context) { x =>
    val isFizz = x % 3 == 0
    val isBuzz = x % 5 == 0
    (isFizz, isBuzz) match {
      case (true, true) => "fizzbuzz"
      case (true, _) => "fizz"
      case (_, true) => "buzz"
      case _ => x.toString
    }
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

At no point is `fizzBuzz` burdened by the effects of the context it executes against. Given a `Functor` instance for them, it's usable against `IO[Int]` and `Future[Int]` as well!

### Functors are universal

You might be thinking that lists and arrays in the wild already have a `map` operation available. `Promise`s in JavaScript also have their own `map` operation named [`then`][]. You've been using functors for a while and never realized!

Functors as a formal abstraction API, such as in the Scala `Functor` typeclass, find their strongest use in cases where the concrete type of the context is unimportant. However, you might observe that the _shape_ of functors appears in many places without being _called_ a functor. Using `map` on a list conceptually performs the same operation as on both arrays and `Promise`s. Other structures defining `map` operations may be functors because _functors arise from settings where stuff exists under some circumstances_.

#### Composition of functional effects

That so many functors appear in the wild is no coincidence. Functors even have a [formal definition][] within the higher math of [category theory][]. This definition can be applied to any structures that have the shape of a functor to assert that they behave as functors.

In order to be a functor, a context defining a `map` function must satisfy two laws:

1. Preservation of identity functions:

    ```scala
    context.map(id) == id(context)
    ```

2. Preservation of function composition:

    ```scala
    context.map(g ∘ f) == context.map(f).map(g)
    ```

Here are the two laws applied against Scala's builtin `List` type, which defines its own `map` operation:

:::{.numberLines}
```scala
def preservesIdentityFunctions(list: List[Int]): Unit =
  assert(list.map(identity) == identity(list))

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

These laws assert that functors preserve the behavior of functions `f` and `g` as if they were applied in sequence and also if they were composed independent of `map`. Functors thus _compose functional effects_ because this property of composition is retained within the context of their effects.

Because of this rigorous definition, functors as a design pattern represent a concept that _transcends_ codebases and languages. In contrast, design patterns as they are realized in object-oriented programming form idioms that must be relearned between codebases written even in the same language.

Functors may be universally regarded as a _context of effects_. What this means is that ideally, and _provably_, `map` is the same regardless of specific context.

## Building upon functors

In this post I introduced functors as an elementary abstraction which permit you to separate business logic in function `f` from complex operations producing instances of term `A` in context `F[A]`:

:::{.numberLines}
```scala
def map[A, B](fa: F[A])(f: A => B): F[B]
```
:::

This abstraction is enabling on its own, as it frees the logic in function `f` from the burden of complexity present in the context of `F[_]`. However this is only an _elementary_ abstraction as it simply provides a mechanism for _composing functions_ in serial within the context of effects. Our goal eventually is to abstract effects in _imperative_ code as in the custom effect model from the above `runPayroll` function:

:::{.numberLines}
```scala
def runPayroll(employeeId: Long): PayrollEffect[()] =
  for {
    employee <- employeeRepo.find(employee).getOrFail(EmployeeMissing(employeeId))
    paycheck <- payCalc.calculatePaycheck(employee).getOrFail(PaycheckMissing(employeeId))
    companyAcctNo <- getConfig("companyAccountNo")
    companyRoutingNo <- getConfig("companyRoutingNo")
    response <- achClient.depositPaycheck(companyAcctNo, companyRoutingNo, paycheck)
      .flatMap {
        case "SUCCESS" => pure(())
        case msg       => fail(DirectDepositError(employeeId, msg))
      }
  } yield response
```
:::

Consider for a moment: with a functor you are able to work against the _desired_ case of a result. You can chain any number of `map` operations against a functor, or `map` any number of composed functions against it. At the end, you still have a functor that is in the _desired_ case. In order to write _imperative_ code, you have to be able to force an _undesirable_ case so that subsequent operations are skipped.

Working against _two or more contexts at once_ opens opportunities to introduce _undesirable_ cases. Take for example these two instances of the context `F[_]` and the function signature for `combine`:

:::{.numberLines}
```scala
val fa: F[A]
val fb: F[B]

def combine(a: A, b: B): C
```
:::

How do you apply `combine` to the terms `A` and `B` produced by the contexts? What happens if one of the contexts is in an undesirable case? At first blush it appears that `map` might work, but `combine` takes two arguments. You need a specialized functor in order to apply `combine`!

In my next post, we will explore how **applicatives** enable working within two or more contexts at the same time, as well as the many ways that you will be able to exploit this capability in your programs.

> **Acknowledgements**
>
> Many of my professional network, colleagues, and friends helped me develop this material and proof read my many drafts. I would like to especially thank:
>
> * [Angi Shen](https://www.linkedin.com/in/angishen/), my team mate at Credit Karma, for her many questions about functors and related patterns and forcing me to learn how to teach these subjects.
> * [Ivy Huang](https://www.linkedin.com/in/shushu-huang-b2123324/), my team mate at Credit Karma, for providing the initial impetus to write this piece, and for helping me develop this material both in this format and for use in technical training.
> * [Rex Fenley](https://www.linkedin.com/in/rexmas/), my category theory study buddy, for helping me refine concepts and develop vocabulary to carry the subject.
> * [Rodrigo Manubens](https://www.linkedin.com/in/rodrigo-manubens-7a862260/), my friend and colleague at Credit Karma, for highlighting the hard parts and praising the good parts.
> * [Mindy Or](https://www.linkedin.com/in/mindy-or-2658857a/), my friend and former ThoughtWorks colleague, for providing perspectives for those new to this subject matter and forcing me to think about what points I was actually trying to make.
> * [Aaron Erickson](https://www.linkedin.com/in/aaronerickson/), my former ThoughtWorks colleague and seasoned functional programmer, for proof reading and providing criticism.

[Defensive programming]: https://en.wikipedia.org/wiki/Defensive_programming
[tacit knowledge]: https://en.wikipedia.org/wiki/Tacit_knowledge
[exponential back-off]: https://en.wikipedia.org/wiki/Exponential_backoff
[formal definition]: https://en.m.wikipedia.org/wiki/Functor
[category theory]: https://en.m.wikipedia.org/wiki/Category_theory
[`then`]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise#chained_promises

[^hkt]: `F[_]` specifically is a [higher-kinded type](https://danso.ca/blog/higher-kinded-types/).
