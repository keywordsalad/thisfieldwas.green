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

### Tinfoil hat time!

This sudden urge to self host was impulsed by the advent of GitHub Copilot. The concept of it sure spooked the hell out of me. Not the idea that my professional role had become so fungible that I could be replaced by a machine, I feel that is inevitable regardless, but rather that I unknowingly, and in true _the product is free_ fashion, was myself the source of the data for which _corporations_ would be paying untold sums of money to _another corporation_ rather than to _people_ in order to manifest technical solutions; my labor having been willingly consented to subsidize widening profit margins in what felt to me a very real embodiment of late stage capitalism. Thus I felt very motivated to get as much of my stuff out of third party services as I could because it was clear, as this also coincided with Apple’s neural hashing of photos, that third parties could not be trusted with my data _even if they were already making money off of it_. I went so far as to disconnect myself from cloud server hosting.

I had complained to `[REDACTED]` loudly about this whole GitHub thing and that whole Apple thing, and just a few days later he had found this computer next to the garbage chute and with much excitement called me and announced, _“It turns on!”_ What luck!

## Foundations

The computer sat in many pieces in my living room for about a month as I removed dust and prepared to install an array of disks to host my media. HP did not make this case amenable to changes or even provide much in the way of expansion bays, so as a consequence I now have a drive array that is free-floating within the case. However, the drives are each `4TB` and so heavy that they are held in place by friction induced by their own mass and density alone. The computer for a short period held the hostname `gravwell` because of the density these drives gave it.

After I arranged the disks within and successfully had an Arch installation that could be bootstrapped in a repeatable manner, I closed up the case and moved the computer into the bedroom closet where I could plug it directly into the router.

I spent a good number of days iterating on this computer with an [Ansible playbook](https://bitsof.thisfieldwas.green/keywordsalad/ansibled/src/commit/2f8b5c99c51adeb2226d2e9e51cead6766448559/servers.yml#L1-L23) to get the configuration just right. I have the drive array set up so that if I nuke the computer then the array will be rebuilt and re-encrypt the drives, though the data will be lost. My goal with this was to be able to take a second computer, because this one will at some point cease to work, and be able to spin the new computer up with the same configuration before plugging the drive array into it. Configuration management is Ansible's primary domain, it does this very well, and I now use it for my current three Linux-based systems. I have very much nuked and enjoyed rebuilding my Arch Linux-powered _thonkpad_ with just a few keystrokes several times now, and I manage all changes to it through its playbook.

## Logistics

The closet computer now hosts a small set of services:

* My website uses `nginx`, which also terminates `ssl`.
* My source code using [`gitea`](https://gitea.io/).
* A big `16TB` of usable space, _just a bunch of disks_ array, redundancy provided by [`snapraid`](https://www.snapraid.it/), and unioned with [`mergerfs`](https://github.com/trapexit/mergerfs). Functionally it looks like one big fat disk and it's very nice to use.
* `pihole`... I thought this would mean less ads, but it's mostly been more headaches.

I’m using docker containers for `gitea` and `postgres` (which supports `gitea`), and `nginx` routes everything from the web either to static `html` or to `gitea`.

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
* Hubris.

I don’t have a static IP. My ISP’s network infrastructure simply doesn’t allow for it. To get around this, I had to swallow my pride and purchase a cloud server from [`linode`](https://linode.com), whose IP is pointed to by all of my domains. I use `autossh` to maintain `ssh` tunnels for ports `22`, `80`, and `443` on the closet computer into which an `nginx` streaming server on the `linode` forwards traffic coming in on the same ports. It’s ok that I’m using a cloud server for this, I tell myself, because none of my data is stored on it.

If the `ssh` tunnels aren’t up, the sites can’t be served and I can’t even get an error page out. It’s really kinda crap like that. Being what it is, `systemd` on the closet computer will stop `autossh` from retrying connections to the `linode` after a certain period and this is a major problem if there’s a hiccup in the home internet connection. Sometimes my internet is out for five minutes, but that’s a tradeoff made when using a cheaper, locally-based ISP over Comcast's cartoon villain bundled contracts.

I absolutely need to use something other than `systemd` to manage my `ssh` tunnels as it hasn’t been reliable _at all_ and probably one of the bigger points of failure because it cascades after any failures the router experiences.

The closet receives too much human traffic for the closet computer or really any hardware to be held there. We live in a century craftsman whose modern infrastructure has been monkey-patched together and I conveniently piggybacked on existing connections made by the previous homeowners without putting thought into any consequences therein. Now we have a closet acting as a server cabinet instead of a closet. We are of course using it as both, with frequent access throughout the day and cables running everywhere as if a panicked octopus were dropped on the floor and then frozen in place.

## Duct tape and rainbows

I’ve had a lot of problems hosting my website and source code from my closet computer in terms of uptime. Mostly related to tripping over the power cable, though in other ways the setup isn’t resilient and I still need to do some work to improve how it performs. But it’s been a really rewarding experience nonetheless. I’ve been able to work in a part of tech that I use all the time but have never myself had to engineer or maintain.

Retaking my source code and hosting my website myself has felt very empowering, as if I’ve also reclaimed a little piece of the internet. For that alone I want to see what more I can self host. As a whole the internet has become clustered around a lot of centralizing services, and I pine for the days when everyone and their dog had their own small, weird websites and frequented a handful of small, niche forums with just enough users to be interesting. Now there's Facebook. GitHub. Google... Name a _thing_, and there's effectively a single company that one goes to for that _thing_, and it's regressed towards the mean and mediocre.

Despite this disdain I'm expressing, third parties are invaluable for uptime. I took this for granted rather severely, and it’s making me rethink what total cost actually looks like, not only in terms of dollars and _rights_ but even simple booleans such as _will the hiring manager able to see that writeup I referred her to?_ I would absolutely benefit from hosting my website from my `linode`, perhaps after I reread the service agreements to allay my confounding mind.

I still use GitHub professionally and for open source projects hosted there. I may see the light one day and realize that being a principled curmudgeon means that I’m only that. Obviously there exists a sort of cognitive dissonance in regards to contributing to open source works in the context of labor. Regardless, I have given myself a learning opportunity and thus I don’t use GitHub as a consumer for most things now. I suppose that was my goal after all.

I might even give Copilot a go soon. I'm reading some good things about how it cuts through boilerplate like a hot knife through butter. Reviews such as this have helped me conceptualize how it might fit into my own workflow especially now that I've had time to step back. I would be over the moon to have an intelligent assist for the boring parts of my job. I suppose that was GitHub's goal, too.

On the lighter side, being professionally out and telling people that my website is coming out of the closet has given me a certain subversive glee that I never realized I needed. It's a great lead into how the computer got there.
