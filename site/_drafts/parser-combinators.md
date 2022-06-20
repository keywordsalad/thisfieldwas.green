---
title: "Parser Combinators"
description: Combining functions to build a parser
author: Logan McGrath
comments: true
date: 2022-06-20T10:37:30-0700
tags: functional programming, programming, scala, design patterns, combinators, parsing
layout: post
code_repo: https://bitsof.thisfieldwas.green/keywordsalad/parser-combinators/src/branch/main
---

In my post {{linkedTitle "_posts/2022-06-16-set-function.md"}} I introduced function combinators as a pattern for building complex logic. I added the caveat that combinators should not be used to define sets, which I feel undersells the value of combinators as a general programming pattern. In this post we will explore combinators as an applied solution to a common problem: _parsing text_.

<!--more-->

> The code that accompanies this post may be found [here]({{code_repo}}).

## The `parse()` function

Parsers begin as a simple signature, a function called `parse()` from an input `Int` offset and `String` to either a failed offset or matched offset.

:::{.numberLines}
```scala
trait Parse extends (((Int, String)) => Either[Int, (Int, String)])
```
:::

**This signature enables two capabilities:**

1. On failure the offset where the `parse()` function failed to recognize the input is returned.
2. On success the offset where the end of the recognized input is returned along with the input, which can be used to extract the matched input and also be passed to the next `parse()` function as its starting point.

_Below we will introduce specialized `parse()` functions within the `Parse` companion object._

### Implementing the first `parse()` function

The first `parse()` function we can implement is `satisfy()`. This function works by using a _predicate_ to match the `Char` at the current offset within the input.

:::{.numberLines}
```scala
def satisfy(predicate: Char => Boolean): Parse = {
  case (offset, input) if predicate(input(offset)) => Right(offset + 1, input)
  case (offset, _)                                 => Left(offset)
}
```
:::

This function is very simple; it can match single `Char`s easily.

:::{.numberLines}
```scala
"satisfy" can {
  "recognize a single character if it matches" in {
    val parse = satisfy(_ == 'a')
    parse((0, "abc")) shouldBe Right((1, "abc"))
  }
  "reject a character that doesn't match" in {
    val parse = satisfy(_ == 'a')
    parse((0, "xyz")) shouldBe Left(0)
  }
}
```
:::

With a little more work, it can be made to match Unicode `Char`s!

:::{.numberLines}
```scala
"recognize Unicode characters" in {
  val parse = {
    val pattern = Pattern.compile("\\\\p{Ll}")
    satisfy(c => pattern.matcher(CharBuffer.wrap(ArrayCharSequence(Array(c)))).matches())
  }
  parse((0, "π is pi")) shouldBe Right((1, "π is pi"))
  parse((0, "Π is uppercase pi")) shouldBe Left(0)
}
```
:::

Thus `satisfy()` is sufficiently versatile to meet a wide range of needs, though it requires that you be creative in how you create your predicates.

### Specializing the `satisfy()` parse function

Based on the previous example, recognizing a character with a regular expression is really cumbersome. Let's define a `parse()` function that makes this kind of recognizer easier to define.

:::{.numberLines}
```scala
def satisfyRegex(pattern: String): Parse = {
  val pattern = Pattern.compile("\\p{Ll}")
  satisfy(c => pattern.matcher(CharBuffer.wrap(ArrayCharSequence(Array(c)))).matches())
}
```
:::

Now a regular expression can be provided without creating the boilerplate to support the predicate directly. The test to show that the `satisfyRegex()` function is identical to the original, as well.

:::{.numberLines}
```scala
"satisfyRegex" can {
  "recognize a character using a regex" in {
    val parse = satisfyRegex("\\\\p{Ll}")
    parse((0, "π is lowercase pi")) shouldBe Right((1, "π is lowercase pi"))
    parse((0, "Π is uppercase pi")) shouldBe Left(0)
  }
}
```
:::

## Parser combinators

So far we have two `parse()` functions: `satisfy()` and `satisfyRegex()`. Neither of these do more than recognize single `Char`s and advance the offset of the input. We want to be able to recognize sections of input.

Let's start first by recognizing two letters, one after the other, with our first combinator: the `&` combinator.

**What is a combinator?** A combinator is a function that builds a function from other functions. In this case, we will be building a new `parse()` function that _combines_ the capabilities of two other `parse()` functions.

Here is how we define the combinator on the `Parse` trait:

:::{.numberLines}
```scala
def &(second: Parse): Parse =
  input => this(input).flatMap(nextInput => second(nextInput))
```
:::

By using the `&` combinator we can define new `parse()` functions from two existing ones, allowing us to recognize a length of input. Here is a short example using a test:

:::{.numberLines}
```scala
"&" which {
  val parseFirst = satisfy(_ == 'a')
  val parseSecond = satisfy(_ == 'b')
  val parse = parseFirst & parseSecond
  "recognizes the first letter and then the second" in {
    parse((0, "abc")) shouldBe Right((2, "abc"))
  }
  "recognizes the first letter but rejects the second" in {
    parse((0, "acc")) shouldBe Left(1)
  }
  "rejects the first letter" in {
    parse((0, "ccc")) shouldBe Left(0)
  }
}
```
:::

### Recognizing alternatives with a second `parse()` function

Given the possibility that a `parse()` function may fail to recognize the input, is there a way that we could fall back to an alternative `parse()` function that would be able to recognize it? There is!

The next combinator is `|`. This combinator tries to recognize the input with the first `parse()` function and will use the second `parse()` function if it fails.

Here is how we define the combinator on the `Parse` trait:

:::{.numberLines}
```scala
def |(second: Parse): Parse =
  input => this(input).left.flatMap(_ => second(input))
```
:::

This combinator allows us to choose between possibilities of input. A simple example is demonstrated in its test:

:::{.numberLines}
```scala
"|" which {
  val parseFirst = satisfy(_ == 'a')
  val parseSecond = satisfy(_ == 'b')
  val parse = parseFirst | parseSecond
  "recognizes using the first parse function" in {
    parse((0, "abc")) shouldBe Right((1, "abc"))
  }
  "recognizes using the second parse function" in {
    parse((0, "bcd")) shouldBe Right((1, "bcd"))
  }
  "rejects if neither parser recognizes" in {
    parse((0, "cde")) shouldBe Left(0)
  }
}
```
:::

## Putting together what we have so far

Now that we have sequential and alternative recognizers, we can start defining more complex `parse()` functions. Let's start with a term recognizer.

:::{.numberLines}
```scala
def term(value: String): Parse =
  value.tail.foldLeft(satisfy(_ == value.head))((p, c) => p & satisfy(_ == c))
```
:::

By using the classic functional programming idiom `foldLeft()`, we can build a term recognizer using only the functions and combinators we have defined so far. Slick!

Here is the test to show that our `term()` function works, including one using the `|` combinator with a second term:

:::{.numberLines}
```scala
"term" which {
  val parse = term("banana")
  "recognizes a word" in {
    parse((0, "banana apple")) shouldBe Right((6, "banana apple"))
  }
  "rejects input that doesn't match" in {
    parse((0, "banapple")) shouldBe Left(4)
  }
  "alternate with a second term" in {
    val combined = parse | term("apple")
    combined((0, "banana orange")) shouldBe Right((6, "banana orange"))
    combined((0, "apple orange")) shouldBe Right((5, "apple orange"))
  }
}
```
:::

## How do we get the matched input?

There's a small problem with our `parse()` function. It only gives the end offset of the recognized input and there isn't any way to collect or modify the recognized input. This means we have to change the signature of the `parse()` function.

### Modeling input

First, we need to consider what our input needs to model. Our input is characterized by two things, the text being parsed and the offset that the current `parse()` function is operating from. This defines a _cursor_:

:::{.numberLines}
```scala
case class InputCursor(offset: Int, input: String) {

  def peek: Char = input(offset)

  def +(amount: Int): InputCursor = copy(offset = offset + amount)

  def -(amount: Int): InputCursor = copy(offset = offset - amount)
}
```
:::

A cursor allows us to hold a reference to the input itself, unmodified, but increment or decrement the position we are at within the input. As our `parse()` functions recognize input, they increment the offset so that the following functions have a place to start.

### Modeling output

The output of the `parse()` function changes as well. We want to be able to return the recognized input. This changes the signature of the `parse()` function:

:::{.numberLines}
```scala
trait Parse extends (InputCursor => Either[InputCursor, (???, InputCursor)])
```
:::

But what should be returned isn't entirely clear. If the recognizer matches a `Char`, then the `parse()` function should return a `Char`. But what about the `term()` recognizer? That should return a `String`. What if we're using a function that recognizes integers?

This means that the `parse()` function is generic in its return type:

:::{.numberLines}
```scala
trait Parse[+A] extends (InputCursor => Either[InputCursor, (+A, InputCursor)])
```
:::

The overall return type is also becoming verbose by using the general-purpose `Either` class. So let's define something specialized:

:::{.numberLines}
```scala
sealed trait ParseResult[+A]

case class ParseFailure(cursor: InputCursor) extends ParseResult[Nothing]

case class ParseSuccess[+A](result: A, cursor: InputCursor) extends ParseResult
```
:::

_**But before we use it this means that all of our function signatures have to change!**_

## The new shape of the `parse()` function

Our `parse()` function will soon become generic its return type, which makes it _covariant_ in that `parse()` can be adapted to produce anything we want. What the `parse()` function produces is contextualized by whether it is recognized in the input, and as such there is a chance that nothing is produced because the input wasn't recognized. This means the the `parse()` function forms a specific structure: a _context_ with the shape of a _functor_.

> I have written about contexts, functors, and related structures extensively. Start from the following article if these terms are unfamiliar to you: **{{linkedTitle "_posts/2022-03-15-contexts-and-effects.md"}}**

Take a look at what the `parse()` function signature will look like after we start using `ParseResult`:

:::{.numberLines}
```scala
trait Parse[+A] extends (InputCursor => ParseResult[A])
```
:::

Let's compare that with the shape of a _functor_: `Parse[A]` and `F[A]`.

In fact, `ParseResult[A]` has the same shape!

To save some time, I've added the [`cats`][] library to leverage the functor typeclass abstraction. First, let's implement the `Functor` typeclass for `ParseResult[A]`:

:::{.numberLines}
```scala
object ParseResult {

  implicit val parseResultFunctor: Functor[ParseResult] = new Functor[ParseResult] {

    override def map[A, B](fa: ParseResult[A])(f: A => B): ParseResult[B] =
      fa match {
        case ParseFailure(failedCursor)        => ParseFailure(failedCursor)
        case ParseSuccess(matched, nextCursor) => ParseSuccess(f(matched), nextCursor)
      }
  }
}
```
:::

This allows us to transform the contents of the `ParseResult` at-will.

### Changing the contents of the `parse()` function itself

The `parse()` function itself is a functor. But that doesn't quite make sense: it doesn't contain anything because it's a function!

However, it still has the _shape_ of a functor. Recall the new signature that it will have `InputCursor => ParseResult[A]`. It's covariant in `A`, giving it the shape of `F[A]`. So how do we treat it as a functor? Let's change the signature first and see how the code breaks.

_**And all of the `parse()` functions break!**_ So we will step through each and get them working again.

1. **`satisfy()`** only requires that we change the return:

    ```scala
    def satisfy(predicate: Char => Boolean): Parse[Char] = cursor =>
      if (predicate(cursor.peek)) {
        ParseSuccess(cursor.peek, cursor + 1)
      } else {
        ParseFailure(cursor)
      }
    ```

2. **The `&` combinator** has a `flatMap()` screaming to be abstracted:

    ```scala
    def &[B](second: Parse[B]): Parse[(A, B)] =
      input =>
        this(input) match {
          case ParseFailure(failedCursor) => ParseFailure(failedCursor)
          case ParseSuccess(firstMatched, nextCursor) =>
            second(nextCursor).map(secondMatched => (firstMatched, secondMatched))
        }
    ```

3. **The `|` combinator** requires an abstraction to act on failure:

    ```scala
    def |[B >: A](second: Parse[B]): Parse[B] =
      input =>
        this(input) match {
          case _: ParseFailure    => second(input)
          case s: ParseSuccess[_] => s
        }
    ```

4. **term()** is affected the most, and the rewrite requires threading a cursor through a lot of `parse()` functions and handling the results:

    ```scala
    def term(value: String): Parse[String] = {
      val parsers = value.map(c => satisfy(_ == c))
      cursor =>
        parsers
          .foldLeft[Parse[mutable.StringBuilder]](cursor => stringBuilder()(cursor)) { (sbp, cp) => cursor =>
            (sbp & cp)(cursor).map { case (sb, c) => sb.append(c) }
          }(cursor)
          .map(_.toString())
    ```

**What happens if we leverage the Functor typeclass?**

[`cats`]: https://typelevel.org/cats/
