---
title: Lessons from Sterling
author: Logan McGrath
date: 2013-08-05T09:37:00-07:00
comments: false
tags: Sterling, Functional Programming, Language Design
layout: post
---

I've spent the last seven months developing a language called [Sterling][].
Sterling was intended to be an untyped functional scripting language, something
like lazily-evaluated, immutable JavaScript. Last week I decided to shelve
Sterling.

<!--more-->

## How Sterling Worked

Sterling's evaluation model is very simple and I felt it held a lot of promise
because it made the language very flexible. Everything in Sterling is an
expression. Some expressions accept a single argument--these were called
*lambdas*. All expressions also contain sub-expressions, which could be accessed
as *attributes*. With a little sugar, a bag of attributes could be made
self-referencing and thus become an *object*.

```haskell
// An assortment of basic expression types

// a constant expression which takes no arguments
anExpression = 2 + 2

// lambda expressions take only 1 argument
aLambda = (x) -> 2 + x

// function expressions take more than 1 argument
aFunction = (x y) -> x * y

// an object expression with constructor
anObject = (constructorArg) -> object {
    madeWith: constructorArg,
}

// an object expression that behaves like a lambda after constructed
invokableObject = (constructorArg) -> object {
    madeWith: constructorArg,
    invoke: (arg) -> "Made with #{self.madeWith} and invoked with #{arg}",
}
```

Expressions could be built up to carry a high amount of capability. Because
Sterling is untyped, decoration and ducktyping are used heavily to compose ever
more features into expressions.

Sterling was directly inspired by [Lambda Calculus][]. This had an enormous
impact on the design of the language, the largest of which was how the language
executed at runtime. Expressions in Sterling are represented as trees and
leaves. Top-level expressions have names, and they could be inserted into other
expressions by referencing those names.

```haskell
// A recursive named expression looks like this:
fibonacci = (n) -> if n <= 1 then
                       n
                   else
                       fibonacci (n - 1) + fibonacci (n - 2)
                   end
```

Because each expression was a tree, no expression needed to be executed until
its result was absolutely needed. This lazy execution model allows for very
large, complex expressions to be built in one function then returned to the
outside world to be further processed and executed. Functions could be created
inline and passed as arguments to other functions, or constructed within
functions and returned.

Sterling's tree-based structure naturally supported a prototype-based object
model. To modify an expression tree, the tree needed to create a copy of itself
with any changes to it. All expressions, thus, were effective prototypes. This
also had the benefit of directly supporting immutability and helped to enforce a
 functional programming paradigm.

## What Could Have Been

I intended Sterling to be a functional scripting language. In some ways, I was
looking to create a JavaScript reboot that clung closer to JavaScript's
functional roots and would be used for general-purpose scripting.

Sterling's syntax was designed to be very terse, readable, and orthogonal. By
that I mean everything in Sterling should be an expression that can be used
[virtually anywhere for anything][]. Because Sterling was based on lambdas, this
worked particularly well for arguments expressions because arguments could fold
into the function call result on the left:

```haskell
// Consing a list by folding arguments, left-to-write
[] 1 2 3 4
> [1] 2 3 4
> [1, 2] 3 4
> [1, 2, 3] 4
> [1, 2, 3, 4]
```

This folding capability meant that Sterling could support very expressive
programming styles. Any function could be returned as the result of another
function call and continue chaining against arguments. Sterling's terse syntax
also made defining functions very easy:

```haskell
// Some basic functions in Sterling
identity = (x) -> x
selfApply = (x) -> x x
apply = (x y) -> x y
selectFirst = (x y) -> x
selectSecond = (x y) -> y
conditional = (condition) -> if condition.true? then selectFirst else selectSecond end
friday? = say $ conditional (today.is :friday) 'Yay Friday!' 'Awww...'
```

Because Sterling was intended to be immutable, objects would be used to
represent state and carry behavior to return new state resulting from an
operation:

```haskell
// Printing arguments from an immutable list iterator
main = (args) ->
    print args.iterator // gets an Iterator

print = (iterator) ->
    say unless iterator.empty? then
        printNext iterator 0
    else
        'Empty iterator'
    end

printNext = (iterator index) ->
    unless iterator.empty? then
        "arg #{index} => #{iterator.current}\n" + printNext iterator.tail index.up
    end

Iterator = (elements position) -> object {
    empty?: position >= elements.length,
    head: Iterator elements 0,
    current: elements[position],
    tail: iterator elements position.up,
}
```

Paul Hammant at one point suggested baking dependency injection
[directly into a language][], and even offered I do this in Sterling. This drove
development of a metadata system in Sterling that could be used to support
metaprogramming and eventually dependency injection.

```haskell
// Meta attributes on expressions
@component { uses: [ :productionDb ] }
@useWhen (runtime -> runtime.env is :production)
Inventory = (db) -> object {
    numberOfItems: db.asInt $ db.scalarQuery "SELECT COUNT(*) FROM thingies",
    priceCheck: (thingy) -> db.asMoney $ db.scalarQuery "SELECT price FROM
    thingies WHERE id = :id" { id: thingy.id },
}

@provides :productionDb
createDb = ...

@fake? true
@component { name: :Inventory }
@useWhen (runtime -> runtime.env is :development)
FakeInventory = object -> {
    numberOfItems: 0,
    priceCheck: (thingy) -> thingy.price,
}
```

The metadata system was very flexible and could support arbitrary meta
annotations. The above metadata translates to the following map structures at
runtime:

```haskell
// What meta attributes look like if they were JavaScript
Inventory.meta = {
    "component": {
        "uses": [ "productionDb" ]
    },
    "useWhen": {
        "value": function (runtime) {
            return runtime["env"] == "production";
        }
    }
};

createDb.meta = {
    "provides": {
        "value": "productionDb",
    }
};

FakeInventory.meta = {
    "fake?": {
        "value": true
    },
    "component": {
        "name": "Inventory"
    },
    "useWhen": {
        "value": function (runtime) {
            return runtime["env"] == "development";
        }
    }
};
```

I felt these functional features and expressive syntax would make for an
enjoyable and productive programming experience. The meta system in particular I
felt could become quite powerful especially for customizing load-time behavior
of Sterling programs. However, some of my goals came with a few problems.

## The Problems

### Speed

Sterling is amazingly slow. A natural consequence of a tree-based language is
that trees must be copied and modified for many operations, no matter how
"trivial" they may be (integer arithmetic, for example.) Recursive functions
like the `fibonacci` expression above had a particularly nasty characteristic of
building enormous trees that took a lot of time to reduce to single values.

The speed issues in Sterling were partially mitigated using [memoization][].

### Memoization: Blessing But Possibly A Curse

Memoization increased the possibility for static state to hang around in an
application. Applying arguments to an object constructor, for instance, would
return a previously-constructed object. I'm not entirely sure what the total
impact of the "object constructor problem" could have been, as objects are not
mutable, but I didn't like this charateristic nonetheless. Immutability,
however, wasn't entirely true (see "Escaping The Matrix" below).

Named expressions are persistent in memory. If a named expression took a large
argument, or returned a large result, then the total memory cost of a memoizing
expression could become quite high over time.

### The Impacts Of Typelessness

Types are actually quite nice to have, and I began to miss them quite a bit the
more I worked on Sterling. While Sterling is very flexible (because it has no
types) it also has very poor support for polymorphism (because it has no types).
Want to do something else if you receive an `Asteroid` object rather than a
`Spaceship` object?

The na&iuml;ve solution is to implement an if-case for each expected type:

```haskell
Spaceship = object {
    collideWith: (other) ->
        if other.meta.name is 'Asteroid' then
            say 'Spaceship collided with an asteroid!'
        else if other.meta.name is 'Spaceship' then
            say 'Spaceships collide!'
        end
}

Asteroid = object {
    collideWith: (other) ->
        if other.meta.name is 'Asteroid' then
            say 'Asteroids collide!'
        else if other.meta.name is 'Spaceship' then
            say 'Asteroid collided with a spaceship!'
        end
}
```

This is fragile, though, and the code is complex. What's worse, is there's no
way to ensure that a method is receiving an `Asteroid` and not another object
that simply implements its API.  A better solution is to let the colliding
object select the proper method from the object it's colliding with:

```haskell
Spaceship = object {
    collideWith: (other) -> other.collidedWithSpaceship self,
    collideWithSpaceship: (spaceship) -> say 'Spaceships collide!',
    collideWithAsteroid: (asteroid) -> say 'Spaceship collided with an asteroid!',
}

Asteroid = object {
    collideWith: (other) -> other.collideWithAsteroid self,
    collideWithSpaceship: (spaceship) -> 'Asteroid collided with a spaceship!',
    collideWithAsteroid: (asteroid) -> 'Asteroids collide!',
}
```

This solution is better. It's also similar to implementing [visitor pattern] in
Java. I still don't like it because there's no type safety and adding support
for more types requires violating the [open/closed principle][]. For instance,
in order for a `Bunny` to be correctly collided-with, a `collidedWithBunny`
method must be added to both `Spaceship` and `Asteroid`. Developers may find it
easier instead to allow the `Bunny` to masquerade as an asteroid:

```haskell
// Spaceship-eating Bunny
Bunny = object {
    collideWith: (other) -> other.collideWithAsteroid self, // muahaha I'm an asteroid!
    collidedWithSpaceship: (spaceship) -> say 'NOM NOM NOM NOM!',
    collidedWithAsteroid: (asteroid) -> ...
}
```

This [single-dispatch behavior][] means that for any argument applied to a
method name, the same method will be dispatched. In the case of Java, this is
determined by the type of a method's arguments at compile time. Adding new
methods for similarly-typed arguments requires all client code be recompiled.
While Sterling may not have typing, it is still single-dispatch.

The lack of types became particularly painful when implementing arithmetic
operations and compile-time analysis was nearly impossible without collecting a
great deal of superfluous metadata.

### Escaping The Matrix

As I worked on Sterling, I required functionality that wasn't yet directly
supportable in the language itself. I solved this problem using the "glue"
expression that could tie into a Java-based expression:

```haskell
EmptyIterator = glue 'sterling.lang.builtin.EmptyIterator'
List = glue 'sterling.lang.builtin.ListConstructor'
Set = glue 'sterling.lang.builtin.SetConstructor'
Tuple = glue 'sterling.lang.builtin.TupleConstructor'
Map = glue 'sterling.lang.builtin.MapConstructor'
```

For short-term problems, this option isn't too bad, but it allows the programmer
to escape the immutable "Matrix" of Sterling. For example, I implemented
Sterling's collections as thin wrappers around Java collections, and allowed t
hem to be mutable. Actually, a lot of things in Sterling were mutable:

* Method collections on expressions
* Object methods
* Maps
* Lists

This, coupled with memoization, could cause a lot of issues with static state
and had the potential to enable a lot of bad design decisions for programs
written in Sterling.

## The Good Parts

Despite the baggage, there's a few takeaways!

Sterling's syntax is very small and terse. I particularly enjoyed not having to
type a lot of parentheses, braces, commas, and semicolons. Separating arguments
by spaces allowed the language read like a book.

Most expressions can be delimited with whitespace alone, and because everything
is an expression, objects could be created inline and if-cases could be used as
arguments.

Operators are just methods. Any object or expression can define a "+" operator
and customize what it does. With polymorphism supported with multi-methods, this
can become an incredibly powerful feature.

Sterling also has the ability to define arbitrary metadata on any named
expression. This metadata is gathered into a `meta` attribute and can be
inspected at runtime to support a sort of meta programming.

## What I'm Carrying Forward

I'm now working on a new language project that will be borrowing Sterling's
syntax. This time, however, I will be using types. Algebraic data types hold a
certain fascination for me, and I'm interested in seeing what I can do with
them. At the very least, I do intend on using multi-methods for better
polymorphism support.

I don't think I like declaring scope. It's verbose. Or declaring types. That
should be restricted to locations where it affects interfaces, like function
signatures.

While Sterling's meta system didn't really go anywhere, I do intend on carrying
it forward as a supplement to algebraic types. I may even still bake in
dependency injection because I hate all the typing required to tie together an
application.

I don't believe I will carry forward mandatory immutability, though I may
support some form of "immutability by default".

Sterling's lazy evaluation caused a lot of headaches more than a few times.
I'll probably not make any successor language lazily evaluated because
memoization becomes a near requirement in order to make lazy evaluation useful.

## My Holy Grail

* A language that is interpreted and optionally compiled either AOT or JIT
* [Inferred typing][] as opposed to [nominal typing][]
* At least psuedo-declarative
* Dynamic to some degree
* Easy to write, easy to read
* Highly composable
* Simple closures
* First-class functions, if not first-class everything

[Sterling]: https://github.com/lmcgrath/sterling
[Lambda Calculus]: http://en.wikipedia.org/wiki/Lambda_calculus
[Inferred typing]: http://en.wikipedia.org/wiki/Type_inference
[nominal typing]: http://en.wikipedia.org/wiki/Nominative_type_system
[virtually anywhere for anything]: http://brandonbyars.com/2008/07/21/orthogonality/
[directly into a language]: http://paulhammant.com/blog/crazy-bob-and-type-safety-for-dependency-injection.html/
[open/closed principle]: http://en.wikipedia.org/wiki/Open/closed_principle
[memoization]: {{route '_drafts/sterling-with-memoization.md'}}
[visitor pattern]: http://en.wikipedia.org/wiki/Visitor_pattern#Java_example
[single dispatch]: http://en.wikipedia.org/wiki/Multiple_dispatch#Java
