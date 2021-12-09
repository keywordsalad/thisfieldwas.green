---
title: Reasons why my website is offline
author: Logan McGrath
comments: false
date: 2021-12-06T04:54:32-08:00
tags: self host, configuration management, tinfoil hat
layout: post
---

My website is hosted from a 2007 HP Pavilion tower that my best friend, `[REDACTED]`, a privacy buff, found next to the garbage chute at his condo building. It appears to have been someone’s gaming rig in its prior life, as it has a decent graphics card in it and `8GB` of RAM. Its demise was likely an upgrade to Windows 10, as upon boot, the login screen was so unresponsive that I could barely get the mouse to register movement and indicate for the computer to shut down again.

`[REDACTED]` recommended to me that I breathe a new life into this computer using Arch Linux. Thus began my journey to self host my website.

<!--more-->

## Tinfoil hat time!

This sudden urge to self host is impulsed by the advent of [GitHub Copilot](https://copilot.github.com/). The concept of it sure spooks the hell out of me. Not the idea that my professional role has now become so fungible that I could be replaced by a machine, I feel that is inevitable regardless.

### My revulsion is more nuanced than replacement

My use of GitHub, a long-standing freemium product, has been in the form of a paid private-tier subscription. I pay GitHub for this private tier so that my code remains under my control, and this code is hosted in this context as I am paid to develop it privately, or it is a personal project that I am prototyping. Despite this, if I make this code publicly available it feeds into Copilot without my express knowledge. This code is analyzed and transformed into data for which _corporations_ will be paying untold sums of money to _another, single corporation_ in order to manifest their technical means of production, _I assume_. My passion project energies, many contributions to open source, _my labor_ has been willingly consented to subsidize widening profit margins powering the visceral push behind late stage capitalism's twisting knife as it lances each artery through which our capital collectively channels.

I am very motivated to get as much of my stuff out of third party services as I can because to generalize based on GitHub, it's likely that no third party can be trusted with my data _even if they are already making money off of it_. I resolve to divest Apple and Google of my cloud data, GitHub of my source code, and `linode` of my miscellaneous. In this post I will focus roughly on how my website and source code are now hosted.

Because I complain to `[REDACTED]` about this GitHub Copilot thing, he offers to help me find a new option for hosting my data. Just a few days later he finds this computer next to the garbage chute and with much excitement calls me and gleefully announces, _“It turns on!”_ What luck!

## Keeping my data in the closet

The computer sits in many pieces in my living room for about a month as I remove dust and prepare to install an array of disk to host my media. HP did not make this case amenable to changes or even provide much in the way of expansion bays, so as a consequence I now have a drive array that is free-floating within the case. No matter though, the drives are each `4TB` and so heavy that they are held in place by friction induced by their own mass and density alone. Later for a short period the computer carries the hostname `gravwell` because of the density these drives give it.

After I carefully arrange the disks within and have an Arch installation that can be reproducibly bootstrapped, I close the case and move the computer into the bedroom closet where I can plug it directly into the router.

I spend a good number of days iterating on this computer with an [Ansible playbook](https://bitsof.thisfieldwas.green/keywordsalad/ansibled/src/commit/2f8b5c99c51adeb2226d2e9e51cead6766448559/servers.yml#L1-L23) to get the configuration just right. The drive array is set up so that if I nuke the computer then the array can be rebuilt and the drives encrypted again, though the data will be lost if this happens. As long as the array remains intact, I can add new disks to it incrementally.

My goal with this playbook is to be able to reproduce the configuration of the _closet computer_ on a replacement if it should ever cease to work. Happy path is a matter of a few keystrokes, I grab a coffee while the playbook runs, and then I power off the computer and plug the drive array into it before turning it back on. Huzzah. Configuration management is Ansible's primary domain, it does this very well, and I now use it for my current three Linux-based systems.

I nuke and enjoy rebuilding my Arch Linux-powered _thonkpad_ on occasion. All changes I make to it are managed through its associated playbook, including an exploration of Minecraft launchers as authentication into the game broke and Microsoft required me to migrate my Mojang/Minecraft account.

## Logistics

The closet computer hosts a small set of services:

* My website using [`nginx`](https://www.nginx.com/), which also terminates `ssl`.
* My source code using [`gitea`](https://gitea.io/).
* My data array with `16TB` usable storage:
  * [Just a Bunch of Disks](https://en.wikipedia.org/wiki/Non-RAID_drive_architectures#JBOD) non-RAID architecture with redundancy using [`snapraid`](https://www.snapraid.it/)
  * Magic using [`mergerfs`](https://github.com/trapexit/mergerfs) so that the array looks like one big fat disk.
  * It’s very nice to use, a choice recommendation by `[REDACTED]`.
* [`pi-hole`](https://pi-hole.net/)... I thought this would mean less ads, but it hasn't really helped much.

I’m using docker containers for `gitea` and [`postgres`](https://www.postgresql.org/) (supports only `gitea`, not listed above). `nginx` runs natively and provides routing for its configured domains.

I mentioned above that this computer sits in the closet. I refer to this computer at home simply as the _closet computer_. This location comes with some problems, however. For example the closet does not have a dedicated power outlet and I have to run an extension cable from an outlet in the middle of the room and under the bed.

## Reasons why my website is offline

As I can't speculate about my uptime in terms of a magnitude of `9`'s, let me paint a picture instead with a short and incomplete list of reasons for which my website is offline:

* My ISP had an outage.
* The power was out.
* I closed the sliding closet door too quickly and it unplugged the router.
* My husband pushed a box of shoes under the bed and pushed the extension cable out of its plug.
* Instead of unplugging the router itself to turn it off and then on again, I flipped the switch on the power strip and forgot to turn the _closet computer_ back on.
* I can’t get into the `bios` to configure the _closet computer_ to power back on when power returns.
* The computer came up before the router and `autossh` stopped trying to open the tunnels to the outside network after a period of time because `systemd` flakes.
* My router refuses to route any traffic whatsoever until I point DNS away from the `pi-hole`.
* Hubris.

## A hosting solution held together by glue and popsicle sticks

I don’t have a static IP. My ISP’s network infrastructure simply doesn’t allow for it, as the infrastructure is based on a mesh of wireless signal propagated between large, unsightly antennas installed on top of each house using their service. To get around this, I swallow my pride and purchase again a cloud server from `linode`, I call this one my _bastion server_, and with this as a sort of static IP address I then configure my domains to point to.

As I still can't reach from the outside into my closet, I have to perform a little magic...

Using `ssh`, I can open a [remote tunnel](https://www.ssh.com/academy/ssh/tunneling/example#remote-forwarding) from the _closet computer_ to the _bastion server_ with this command:

```bash
ssh -R 10080:localhost:80 bastion.oflogan.xyz
```

This command binds a connection to the _bastion server_'s `localhost:10080` port and forwards it to the _closet computer_'s `*:80` port. This means I can forward any HTTP traffic the _bastion server_ receives on port `*:80` to `localhost:10080` to the _closet computer_, where I am now hosting my website.

The remote tunnel binds `localhost:10080` on the _bastion server_, which means that the tunnel is not accessible publicly, and my website is still stuck in the closet. In order for my website to come out, I use an `nginx` [reverse proxy](https://docs.nginx.com/nginx/admin-guide/load-balancer/tcp-udp-load-balancer/) to forward the public port `*:80` to private port `localhost:10080`.

I leverage `autossh` to maintain persistent remote tunnels from the _closet computer_ to the _bastion server_ for ports `22`, `80`, and `443`. As the `nginx` reverse proxy to the `ssh` tunnels simply pass bits from point `A` to `B` to `C`, to an outside observer it's as if my website is coming from the _bastion server_, when in fact it's coming out of the closet.

**This is how it looks:**

```{.txt .nowrap}
         +-BASTION-SERVER--------------\
         | <=proxy=> | <=====ssh tunnel+===> +-CLOSET-COMPUTER---\
SSH ---> | *:22----> | localhost:10022 | --> | *:22 SSH          |
HTTP --> | *:80----> | localhost:10080 | --> | *:80 goto 443 duh |
HTTPS -> | *:443---> | localhost:10443 | --> | *:443 my website  |
         `-----------+-----------------+     `-------------------+
```

It’s ok that I’m using a cloud server for this, I tell myself, because none of my data is stored on it. But also, I think I pulled off something kinda cool.

This is a really fragile architecture, however. If the `ssh` tunnels aren’t up, the site can’t be served and I can’t even get an error page out as there doesn't seem to be a way to query for whether the reverse proxies are passing any data through. This setup is really kinda crap like that.

Being what it is, `systemd` on the _closet computer_ will stop `autossh` from retrying connections to the _bastion server_ after a certain period and this is a major problem if there’s a hiccup in the home internet connection. Sometimes my internet is out for five minutes, but that’s a tradeoff made when using a cheaper, locally-based ISP over Comcast's cartoon villain bundled contracts. At a minimum I need to use something other than `systemd` to manage my `ssh` tunnels as it hasn’t been reliable _at all_ and is probably one of the bigger points of failure because it cascades with any failures at the router.

Physically the closet receives too much human traffic for the _closet computer_ or really any hardware to be held there. We live in a modest century Craftsman whose modern infrastructure has been monkey-patched together and I piggybacked on the convenient, pre-existing connections made by the previous homeowners putting no thought into potential consequences therein. Now we have a closet acting as a server cabinet instead of a closet. We are of course using it as both, with frequent access throughout the day and cables running everywhere as if a panicked octopus were dropped on the floor and frozen in place.

## Duct tape and rainbows

I have a lot of problems hosting my website and source code from my _closet computer_ in terms of uptime. Mostly related to tripping over the power cable, though in other ways the setup isn’t resilient and I still need to do some work to improve how it performs. But it’s been a really rewarding experience nonetheless. I’m working with tech that I use all the time as a consumer but have never myself had to engineer or maintain. It's a fun puzzle.

Hosting my website and source code myself feels very empowering, as if I reclaimed a little piece of the internet. For that alone I want to see what more I can self host. The internet today clusters around an increasingly centralizing oligopoly of services, and I pine for the days when every odd person had their own small, weird website, and frequented a handful of small, niche forums, each with just enough users to be interesting. Now there's Facebook. GitHub. Google... If I name a _thing_: there's likely a single company that I would go to for that _thing_, and that _thing_ has likely regressed in quality towards the mean and mediocre. And when I go to that company for that _thing_, I can't tell whether I've found the _thing_ or an ad.

{{img id: "closet-computer-in-context",
      title: "The closet computer, in context. Corey asked me to dust first.",
      src: "/images/reasons-my-website-is-offline/coming-out-of-the-closet-1024.png"}}

I severely took for granted the uptime that a third party service could give me. It’s making me rethink what the total cost of what third party hosting actually looks like, not only in terms of dollars and nebulous ideals, but more importantly in terms of peace of mind. Terms such as _will the promotion committee be able to see that writeup I referred them to?_ I would absolutely benefit from hosting my website from my _bastion server_ instead, but perhaps after I reread `linode`'s service agreements to allay my confounding mind.

Regarding nebulous ideals, Copilot makes a very strong case for treating [data as a form of labor](https://www.brookings.edu/blog/techtank/2018/02/21/should-we-treat-data-as-labor-lets-open-up-the-discussion/). At the time of this writing, Wed Dec 8, 2021, Copilot is in technical review and accessible via waitlist. I can't find any mention of licensing cost or whether there will be paid vs community editions. If Copilot is available as an exclusively free service then I believe it stands as an excellent reinvestment back into the community and profession as a whole. If someone is paying for it then we are collectively exploited within some legally-accepted gray area because we agreed to be.[^tos]

## Stepping back

I still use GitHub professionally and for open source projects hosted there. I realize that being a principled curmudgeon sometimes means that I’m only that. Obviously there also exists a sort of cognitive dissonance in regards to contributing to open source works in the context of labor. Regardless, I gave myself a learning opportunity and I consequently don’t use GitHub as a personal consumer now. I suppose that was my goal after all.

I might give Copilot a go soon. I'm reading some good things about how it cuts through boilerplate like a hot knife through butter. Reviews such as this help me conceptualize how it might fit into my own workflow especially now that I've had time to step back. I would be over the moon to have an intelligent assist for the boring parts of my job. I suppose that was GitHub's goal, too.

On the lighter side, being professionally out and telling people that my website is coming out of the closet gives me a certain subversive glee that I never realized I needed. It's a great lead into how the computer got there.

For now, I host my source code at [bitsof.thisfieldwas.green](https://bitsof.thisfieldwas.green). My website source repository is directly accessible from [keywordsalad/thisfieldwas.green](https://bitsof.thisfieldwas.green/keywordsalad/thisfieldwas.green).

[^tos]: There exists an episode of South Park where Kyle accepts the Apple iTunes user agreement [without reading it](https://www.imdb.com/title/tt1884035/). It's worth a watch but the writers apply a heavy-handed story telling device to get their point across. I liked it, but you may not.
