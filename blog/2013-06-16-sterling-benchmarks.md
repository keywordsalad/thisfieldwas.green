---
layout: post
title: "Sterling Benchmarks"
author: "Logan McGrath"
date: 2013-06-16 21:12
comments: false
published: false
tags:
  - Functional Programming
  - Sterling
  - Language Design
---

Since [mid January][], I’ve been developing a functional scripting language I 
call [Sterling][]. In the past few weeks, Sterling has become nearly usable, but
it doesn’t seem to be very fast. So this weekend, I’ve taking the time to create 
a simple (read: na&iuml;ve) benchmark.

<!--more-->

The benchmark uses a [recursive algorithm][] to calculate the Nth member of the 
[Fibonacci sequence][]. I’ve implemented both Sterling and Java versions of the 
algorithm and I will be benchmarking each for comparison.

```haskell
// Sterling Implementation
fibonacci = n -> if n = 0 then 0
                 else if n = 1 then 1
                 else fibonacci (n - 1) + fibonacci (n - 2)
```

```java
// Java Implementation
static int fibonacci(int n) {
    if (n == 0) {
        return 0;
    } else if (n == 1) {
        return 1;
    } else {
        return fibonacci(n - 1) + fibonacci(n - 2);
    }
}
```

### Why was the Fibonacci sequence chosen for the benchmark?

The algorithm for calculating the Nth member of the Fibonacci sequence has two 
key traits:

* It’s recursive
* It has O(2<sup>n</sup>) complexity

Sterling as of right now performs zero optimizations, so I’m assuming this 
algorithm will bring out Sterling’s worst performance characteristics 
(muahahaha).

## The benchmark execution plan

I’m using a very basic benchmark excluding Sterling’s compilation overhead and 
comparing the results to native Java. I will execute the Fibonacci algorithm 100 
times for 10 iterations, providing an average of the time elapsed for each 
iteration.

```java
// Benchmark, psuedo-Java
Expression input = IntegerConstant(20);
Expression sterlingFibonacci = load("sterling/math/fibonacci");

void javaBenchmark() {
    List<Interval> intervals;
    int value = input.getValue();
    for (int i : iterations) {
        long startTime = currentTimeMillis();
        for (int j : executions) {
            fibonacci(value);
        }
        intervals.add(currentTimeMillis() - startTime);
        printIteration(i, intervals.last());
    }
    printAverage(intervals);
}

void sterlingBenchmark() {
    List<Interval> intervals;
    for (int i : iterations) {
        long startTime = currentTimeMillis();
        for (int j : executions) {
            sterlingFibonacci.apply(input).evaluate();
        }
        intervals.add(currentTimeMillis() - startTime);
        printIteration(i, intervals.last());
    }
    printAverage(intervals);
}
```

## The benchmark results

```bash
Java Benchmark
--------------
Iteration 0: executions = 100; elapsed = 4 milliseconds
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
Iteration 0: executions = 100; elapsed = 8,152 milliseconds
Iteration 1: executions = 100; elapsed = 7,834 milliseconds
Iteration 2: executions = 100; elapsed = 7,873 milliseconds
Iteration 3: executions = 100; elapsed = 7,873 milliseconds
Iteration 4: executions = 100; elapsed = 7,910 milliseconds
Iteration 5: executions = 100; elapsed = 7,973 milliseconds
Iteration 6: executions = 100; elapsed = 7,927 milliseconds
Iteration 7: executions = 100; elapsed = 7,793 milliseconds
Iteration 8: executions = 100; elapsed = 7,912 milliseconds
Iteration 9: executions = 100; elapsed = 7,986 milliseconds
------------------
Average for 10 iterations X 100 executions: 7,923 milliseconds
```

### Immediate conclusions:

Sterling is _**REALLY**_ slow!

Sterling executes directly against an abstract syntax tree representing 
operations and data. This tree is generally immutable, so the execution is 
performed by effectively rewriting the tree to reduce each node into an “atomic” 
expression, such as an integer constant or lambda (which can’t be further 
reduced without an applied argument).

References to functions are inserted into the tree by copying the function’s 
tree into the reference’s node. The function is then evaluated with a given 
argument to reduce the tree to a single node. These copy-and-reduce operations
are very costly and are a likely reason for Sterling’s poor performance.

## @TODO

### Memoization

Copying and reducing a function tree for an argument is expensive. These 
operations should not need to be performed more than once for any function and 
argument pair.

### Bytecode perhaps?

Given the shear amount of recursion and method calls being performed to execute 
Sterling, does it make sense to compile the syntax tree into a bytecode that can 
be executed in a loop?

## Links

* [Sterling GitHub Project][]
* [Benchmark Code][]
* [Sterling Fibonacci Implementation][]

[mid January]: https://github.com/lmcgrath/sterling/tree/8b58ce4d4b080b353f7870ec0c0c30639fb2fa7b
[Sterling]: https://github.com/lmcgrath/sterling
[recursive algorithm]: http://en.wikipedia.org/wiki/Dynamic_programming#Fibonacci_sequence
[Fibonacci sequence]: http://en.wikipedia.org/wiki/Fibonacci_sequence
[Sterling GitHub Project]: https://github.com/lmcgrath/sterling
[Benchmark Code]: https://github.com/lmcgrath/sterling/blob/post_20130616_sterling_benchmark/src/test/java/sterling/math/FibonacciBenchmarkTest.java
[Sterling Fibonacci Implementation]: https://github.com/lmcgrath/sterling/blob/post_20130616_sterling_benchmark/src/main/resources/sterling/math/_base.ag
