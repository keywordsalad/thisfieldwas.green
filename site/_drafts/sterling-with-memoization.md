---
title: Sterling With Memoization
author: Logan McGrath
date: 2013-06-17T04:26:00-07:00
comments: false
tags: Sterling, language design
layout: post
---

In my [last post][] I wrote about performance in the [Sterling][] programming
language with a basic benchmark. Today I'm ticking off one **@TODO** item:
[Memoization][].

<!--more-->

Sterling now stores the results of each function/argument pair, returning
respective results rather than forcing a recalculation of an already-known
value. I've leveraged the benchmark from the previous post, and the difference
in execution speed is very pronounced:

```bash
Java Benchmark
--------------
Iteration 0: executions = 100; elapsed = 6 milliseconds
Iteration 1: executions = 100; elapsed = 4 milliseconds
Iteration 2: executions = 100; elapsed = 4 milliseconds
Iteration 3: executions = 100; elapsed = 4 milliseconds
Iteration 4: executions = 100; elapsed = 4 milliseconds
Iteration 5: executions = 100; elapsed = 4 milliseconds
Iteration 6: executions = 100; elapsed = 4 milliseconds
Iteration 7: executions = 100; elapsed = 4 milliseconds
Iteration 8: executions = 100; elapsed = 4 milliseconds
Iteration 9: executions = 100; elapsed = 4 milliseconds
--------------
Average for 10 iterations X 100 executions: 4 milliseconds

Sterling Benchmark
------------------
Iteration 0: executions = 100; elapsed = 648 milliseconds
Iteration 1: executions = 100; elapsed = 0 milliseconds
Iteration 2: executions = 100; elapsed = 1 milliseconds
Iteration 3: executions = 100; elapsed = 0 milliseconds
Iteration 4: executions = 100; elapsed = 0 milliseconds
Iteration 5: executions = 100; elapsed = 0 milliseconds
Iteration 6: executions = 100; elapsed = 0 milliseconds
Iteration 7: executions = 100; elapsed = 0 milliseconds
Iteration 8: executions = 100; elapsed = 0 milliseconds
Iteration 9: executions = 100; elapsed = 0 milliseconds
------------------
Average for 10 iterations X 100 executions: 64 milliseconds
```

Sterling without memoization required on average 0.079 seconds to calculate the
20th member of the Fibonacci sequence, but with memoization, the amount of time
shrinks to 0.006 seconds. The time penalty only applies the first time the
function is executed for a given argument, so call times become
near-instantaneous.

## Sterling is faster than Java!

**Not really.** But it is if I fiddle with the benchmark variables a bit (:

By changing the benchmark to execute the Fibonacci function 1000 times for 100
iterations, something interesting happens:

```bash
Java Benchmark
--------------
Iteration 0: executions = 1000; elapsed = 42 milliseconds
Iteration 1: executions = 1000; elapsed = 39 milliseconds
Iteration 2: executions = 1000; elapsed = 38 milliseconds
Iteration 3: executions = 1000; elapsed = 39 milliseconds
Iteration 4: executions = 1000; elapsed = 39 milliseconds
Iteration 5: executions = 1000; elapsed = 39 milliseconds
Iteration 6: executions = 1000; elapsed = 41 milliseconds
Iteration 7: executions = 1000; elapsed = 40 milliseconds
Iteration 8: executions = 1000; elapsed = 38 milliseconds
Iteration 9: executions = 1000; elapsed = 38 milliseconds
...
Iteration 99: executions = 1000; elapsed = 39 milliseconds
--------------
Average for 100 iterations X 1000 executions: 39 milliseconds

Sterling Benchmark
------------------
Iteration 0: executions = 1000; elapsed = 629 milliseconds
Iteration 1: executions = 1000; elapsed = 0 milliseconds
Iteration 2: executions = 1000; elapsed = 0 milliseconds
Iteration 3: executions = 1000; elapsed = 0 milliseconds
Iteration 4: executions = 1000; elapsed = 0 milliseconds
Iteration 5: executions = 1000; elapsed = 0 milliseconds
Iteration 6: executions = 1000; elapsed = 0 milliseconds
Iteration 7: executions = 1000; elapsed = 0 milliseconds
Iteration 8: executions = 1000; elapsed = 1 milliseconds
Iteration 9: executions = 1000; elapsed = 0 milliseconds
...
Iteration 99: executions = 1000; elapsed = 0 milliseconds
------------------
Average for 100 iterations X 1000 executions: 6 milliseconds
```

### This benchmark smells funny

Yes, the performance in this benchmark is very contrived. But this does present
an interesting potential property of applications written in Sterling: If an
application performs a great deal of repeated calculations, it will run faster
over time. A quick glance at the second bench mark will show that Java is
performing the calculation every single time it is called, whereas Sterling only
requires the first call and then it stores the result. This suggests **O(1)**
vs. **O(n)** time complexity in Sterling's favor.

You won't get this sort of performance for a web application because of their
side effect-driven nature, but for number crunching Sterling may very well be a
good idea.

## @TODO

### How does memoization impact memory?

Obviously, those calculated values get stored somewhere, and somewhere means
memory is being used. I should perform another benchmark comparing memory
requirements of the Fibonacci algorithm between pure Java and Sterling.

### What if I don't want memoization for a particular function?

There may be some cases where you want to recalculate a value for a known
argument. For example, if I query a database I shouldn't necessarily expect the
same result each time. Sterling should give an easy way of signalling that a
function should not leverage memoization.

## Links

* [Commit containing memoization changes][]
* [Benchmark showing O(1) complexity][]

[last post]: {{route '_drafts/sterling-benchmarks.md'}}
[Sterling]: https://github.com/lmcgrath/sterling
[Memoization]: https://en.wikipedia.org/wiki/Memoization
[Commit containing memoization changes]: https://github.com/lmcgrath/sterling/commit/7d69d49a911d2d916701fa973e02ffabe82afe9d
[Benchmark showing O(1) complexity]: https://github.com/lmcgrath/sterling/blob/5c879ece28194fdbc36ed5dff2a760d6a38a4033/src/test/java/sterling/math/FibonacciBenchmarkTest.java
