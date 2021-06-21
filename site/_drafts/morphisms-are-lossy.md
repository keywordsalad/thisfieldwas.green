---
layout: post
title: "Morphisms are Lossy"
author: "Logan McGrath"
date: 2021-06-18T18:02:00-07:00
comments: false
published: false
tags:
  - Category Theory
  - Scala
  - Cats
---

Outline:
* Two things, a layman's introduction to:
  * Natural transformations
  * Isomorphisms
* Observation of loss of information in natural transformation as applied in Lists and Options using Scala Cats
* A crash course in natural transformations as defined in category theory
* Generalizing information loss, loss all the way down
  * Applied to products
  * Applied to projections
  * Applied to functions between sets
  * Applied to isomorphisms aka no information is lost, what gives?
