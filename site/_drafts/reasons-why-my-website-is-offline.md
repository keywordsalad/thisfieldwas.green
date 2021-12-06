---
title: Reasons why my website is offline
author: Logan McGrath
comments: false
date: 2021-12-06T04:54:32-08:00
tags: self host, configuration management
layout: post
---

My website is hosted from a 2007 HP Pavilion tower that my best friend, `[REDACTED]`, a privacy buff, found next to the garbage chute at his condo building. It must have been someone’s gaming rig in its prior life as it had a decent graphics card in it and 8gb of RAM. Its demise appeared to be an upgrade to Windows 10, as upon boot the login screen was so unresponsive that I could barely get the mouse to register movement and indicate for the computer to shut down again.

Per recommendation by `[REDACTED]`, I breathed new life into this computer with an Arch Linux install. I began a project for myself: to see how many of my third party services I could host on my own.

<!--more-->

## Why not just use the cloud?

**Tinfoil hat time!**

This sudden urge to self host was impulsed by the advent of GitHub Copilot. The concept of it sure spooked the hell out of me. Not the idea that my professional role had become so fungible that I could be replaced by a machine, I feel that is inevitable regardless, but rather that I unknowingly, and in true _the product is free_ fashion, was myself the source of the data for which corporations would be paying untold sums of money to _another_ corporation rather than to _people_ in order to manifest technical solutions. My labor having been willingly consented to grotesquely subsidize widening profit margins in a very real embodiment of late stage capitalism. Thus I was very motivated to get as much of my stuff out of third party services as I could because it was clear, as this also coincided with Apple’s neural hashing of photos, that third parties could not be trusted with my data _even if they were already making money off of it_.

I had complained to `[REDACTED]` loudly about this whole GitHub thing and that whole Apple thing, and just a few days later he found this computer next to the garbage chute and with much excitement called me and announced, “It turns on!” What luck!

## Foundations

The computer sat in many pieces in my living room for about a month as I removed dust and prepared to install an array of disks to host my media. HP does not make cases amenable to many changes or even provide much in the way of expansion bays, so as a consequence I now have a drive array that is free-floating within the case. However, the drives are each `4TB` and so heavy that they are held in place by friction induced by their own mass and density alone. The computer for a short period held the hostname `gravwell` because of the density these drives gave it.

After I arranged the disks within and successfully had an Arch installation that could be bootstrapped in a repeatable manner, I closed up the case and moved the computer into the bedroom closet where I could plug it directly into the router.

I spent a good number of days iterating on this computer with an [Ansible playbook](https://bitsof.thisfieldwas.green/keywordsalad/ansibled/src/commit/2f8b5c99c51adeb2226d2e9e51cead6766448559/servers.yml#L1-L23) to get the configuration just right. I have the drive array set up so that if I nuke the computer then the array will be rebuilt, re-encrypting the drives, though the data will be lost. My goal with this was to be able to take a second computer, because this one will at some point approach end-of-life, and be able to spin it up with the same configuration before copying everything over from the old one. Configuration managemenent is Ansible's value prop, it does this very well, and I now use it for my current three Linux-based systems. I have very much nuked and enjoyed rebuilding my Arch Linux-powered _thonkpad_ with just a few keystrokes several times now.

## Logistics

The closet computer now hosts a small set of services:

* My website using `nginx`, which also terminates `ssl`
* My source code using [`gitea`](https://gitea.io/)
* A big `16TB`-usable _just a bunch of disks_ array, redundancy provided by [`snapraid`](https://www.snapraid.it/), unioned with [`mergerfs`](https://github.com/trapexit/mergerfs). Functionally it looks like one big fat disk and it's very nice to use.
* `pihole`... I thought this would mean less ads, but it's mostly been more headache.

I’m using docker containers for `gitea` and `postgres` (which supports `gitea`), and `nginx` routes everything from web either to static `html` or to `gitea`.

I mentioned above that this computer sits in the closet. I refer to this computer at home simply as _the closet computer_. This location comes with some problems, however. For example the closet does not have a dedicated power outlet and I have to run an extension cable from an outlet in the middle of the room and under the bed.

## Reasons why my website is offline

Here is a short list of reasons that my website has been offline:

* My ISP had an outage.
* The power was out.
* I closed the sliding closet door too quickly and it unplugged the router.
* My husband pushed a box of shoes under the bed and pushed the extension cable out of its plug.
* Instead of unplugging the router itself to turn it off and then on again, I flipped the switch on the power strip and forgot to turn the closet computer back on.
* I can’t get into the `bios` to configure the closet computer to power back on when power returns.
* The computer came up before the router and `autossh` stops trying to open the tunnels to the outside network after a period of time because systemd being what it is simply gives up.
* The `pihole` service seems to not be working after powering on and my router refuses to route any traffic whatsoever. Not even IP addresses!

I don’t have a static IP. My ISP’s network infrastructure simply doesn’t allow for it. To get around this, I have a [`linode`](https://linode.com) hosted in Fremont, CA whose IP is pointed to by all of my domains, and I use `ssh` tunnels for ports `22`, `80`, and `443` on the closet computer into which a an `nginx` streaming server forwards traffix coming in from the `linode`'s same ports. If the `ssh` tunnels aren’t up, the sites don’t serve and I can’t even get an error page out. It’s really kinda crap. Being what it is, `systemd` will stop `autossh` from retrying connections to the `linode` after a certain period and this is a major problem if there’s a hiccup in the internet connection. Sometimes my internet is out for five minutes, but that’s kind of the trade off of using a cheaper mesh network ISP over Comcast's cartoon villain contract bundles.

I probably need to use something other than `systemd` to manage my `ssh` tunnels as it hasn’t been reliable _at all_ and probably one of bigger points of failure because it cascades after any failures the router experiences.

The closet receives too much human traffic for the closet computer or really any hardware to be held there. We live in a century craftsman whose modern infrastructure has been monkey-patched together and I conveniently piggybacked on existing connections made by the previous homeowners without putting thought into any consequences therein. Now we have a closet acting as a server cabinet instead of a closet. We are of course using it as both, with frequent access throughout the day and cables running everywhere as if a panicked octopus were dropped on the floor and then frozen in place.

## Duct tape and rainbows

Retaking my source code has felt very empowering, as if I’ve also reclaimed a little of the internet back for myself. For that alone I want to see what more I can do. As a whole the internet has become clustered around a lot of centralizing services, and I wish for things to become more loose as they once were 20 years ago when everyone and their dog had their own website and regularly attended a handful of niche forms from a disparate, fuzzy set. Now there's Facebook. GitHub. Google. Name a _thing_, and there's effectively a single company that one goes to for that _thing_.

I’ve had a lot of problems hosting my website and source code from my closet computer in terms of uptime. Mostly related to tripping over the power cable, though in other ways the setup isn’t resilient on its own and I still need to do some work to improve how it performs. But it’s been a really rewarding experience nonetheless. I’ve been able to work in a part of tech that I use all the time but have never myself had to maintain.

Third parties are invaluable for uptime. I took this for granted rather severely, and it’s making me rethink what total cost actually looks like, not only in terms of dollars and _rights_ but on softer metrics such as _was the hiring manager able to see that writeup I referred her to?_

I still use GitHub professionally and for open source projects hosted there. I may see the light one day and realize that being a principled curmudgeon means that I’m only that. Naturally I acknowledge a sort of cognitive dissonance in regards to contributing to open source works. Regardless, I have given myself a learning opportunity and the worst thing that happened is I don’t use GitHub as a consumer for most things now. I suppose that was my goal after all.

On the lighter side, being professionally out and telling people that my website is coming out of the closet gives me a certain subversive glee I never realized I needed. I might even give Copilot a go soon. I'm reading some good things about how it cuts through boilerplate like a hot knife through butter. I'm seeing how it might fit into the world of a professional software engineer now that I've had time to step back, and I would be over the moon to have an intelligent assist for the boring parts of my job.
