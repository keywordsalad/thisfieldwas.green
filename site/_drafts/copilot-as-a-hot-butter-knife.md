---
title: GitHub Copilot as a hot butter knife
author: Logan McGrath
comments: true
date: 2021-12-13T14:49:08-0800
tags: copilot, programming
layout: post
---

In my [last post]({{getUrl '_posts/2021-12-11-reasons-why-my-website-is-offline.md'}}), I vaguely recall a review of GitHub Copilot along the lines of _"cuts through boilerplate like a hot knife through butter"_.

While reviewing drafts of my post, my brother, [Ian McGrath](https://www.ians.tech/), pointed out to me that he couldn't tell on his phone whether the heading of the current section belonged under the previous heading or introduced a new section. I feel that font size alone is a poor differentiator between heading levels, and I think that I ought to be able to use `css` to automatically number them and make their relationships explicit. Given `html`'s six header elements, there should be enough boilerplate for Copilot to demonstrate whether it saves me time.

<!--more-->

_In this post I will demonstrate in Copilot:_

* Numbered headers in `scss`
* Unprompted suggestions in `scss`
* Infinitely numbered headers in `scss`
* Conflicting weight of terms used in imports in `scss`
* Speaking highly of itself in `scss`
* When and when not to include `css/` as prefix to file names in `haskell`

## Numbering headers

<div id="numbered-headers">

{{imageFigure id: "headers-without-numbers",
      src: "/images/copilot-as-a-hot-butter-knife/headers/01-headers-without-numbers.png",
      title: "My minimalist design where h2 and h3 headers are nearly the same size"}}

{{imageFigure id: "headers-with-numbers",
      src: "/images/copilot-as-a-hot-butter-knife/headers/02-headers-with-numbers.png",
      title: "By adding header numbering, I maintain the minimalism, and make the subheading relationship explicit"}}

</div>
