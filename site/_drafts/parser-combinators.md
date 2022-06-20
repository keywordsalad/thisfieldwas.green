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

## Putting the combinators together

Now that we have sequential and alternative recognizers, we can start defining more complex `parse()` functions. Let's start with a keyword recognizer.

:::{.numberLines}
```scala
def keyword(value: String): Parse =
  value.tail.foldLeft(satisfy(_ == value.head))((p, c) => p & satisfy(_ == c))
```
:::

By using the classic functional programming idiom `foldLeft()`, we can build a keyword recognizer using only the functions and combinators we have defined so far. Slick!

Here is the test to show that our `keyword()` function works, including one using the `|` combinator with a second keyword:

:::{.numberLines}
```scala
"keyword" which {
  val parse = keyword("banana")
  "recognizes a word" in {
    parse((0, "banana apple")) shouldBe Right((6, "banana apple"))
  }
  "rejects input that doesn't match" in {
    parse((0, "banapple")) shouldBe Left(4)
  }
  "alternate with a second keyword" in {
    val combined = parse | keyword("apple")
    combined((0, "banana orange")) shouldBe Right((6, "banana orange"))
    combined((0, "apple orange")) shouldBe Right((5, "apple orange"))
  }
}
```
:::
