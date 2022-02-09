---
title: Highly Available SSH Tunnels
author: Logan McGrath
comments: false
date: 2022-02-08T14:27:57-0800
published: 2022-02-08T14:27:57-0800
tags: self host, duct tape
layout: post
---

In my previous post, {{linkedTitle "_posts/2021-12-11-reasons-why-my-website-is-offline.md"}}, I complained about `systemd` giving up when it fails to maintain SSH tunnels. In this post, I complain about `systemd` a bit more and how I gave up and stopped using it for managing my SSH tunnels.

<!--more-->

## Recall from my last post

1. My website is hosted from a computer that sits in my closet
2. I have no static IP address
3. I open a remote SSH tunnel from the _closet computer_ to my _bastion server_ hosted on a `linode`
4. My host names' DNS point to the IP address of the _bastion server_
5. Traffic received by the _bastion server_ is forwarded through SSH tunnels to the _closet computer_
6. The _closet computer_ handles the traffic it receives through the SSH tunnels

**This is how it looks:**

```{.txt .nowrap}
         +-BASTION-SERVER--------------\
         | <=proxy=> | <===remote tunnel===> +-CLOSET-COMPUTER---\
SSH ---> | *:22----> | localhost:10022 | --> | *:22 SSH          |
HTTP --> | *:80----> | localhost:10080 | --> | *:80 goto 443 duh |
HTTPS -> | *:443---> | localhost:10443 | --> | *:443 my website  |
         `-----------+-----------------+     `-------------------+
```

## Failure, `systemd`, and duct tape

When my home internet connection is down, the SSH tunnels into the _closet computer_ fail to reopen. I have been using `systemd` to manage these tunnels, and `systemd` will eventually stop trying to open the tunnels if the network remains inaccessible. This design is to safeguard against situations such as the [thundering herd problem](https://en.wikipedia.org/wiki/Thundering_herd_problem) and [this Stackoverflow question](https://unix.stackexchange.com/questions/289629/systemd-restart-always-is-not-honored) highlights the specific mechanism by which `systemd` decides to stop trying. To paraphrase, `systemd` tries a bit to recover and then gives up.

I can't make any guarantees about uptime regarding my home internet connection, and I am not at risk of causing a thundering herd against my own infrastructure. As a solution, `systemd` does not make sense for managing my SSH tunnels.

As a better solution, one held together by duct tape and per recommendation by [Vlad](https://internetwebsite.ofvlad.xyz), I have instead created a cronjob to manage my SSH tunnels:

```{.txt .nowrap}
*/5 * * * * ssh closet.thisfieldwas.green true || ssh -fN bastion
```

And added the following SSH configuration:

```{.txt .nowrap}
Host bastion
    HostName bastion.thisfieldwas.green
    Port 22
    RemoteForward 10022 localhost:22
    RemoteForward 10080 localhost:80
    RemoteForward 10443 localhost:443

Host closet.thisfieldwas.green
    ProxyCommand ssh jump_user@bastion.thisfieldwas.green -W 127.0.0.1:10022
```

This simple setup works by testing every five minutes whether the SSH tunnel to _closet computer_ is open by SSH'ing in by its external hostname. There is an account named `jump_user` that is allowed to SSH into the tunnels open on the _bastion server_. If an SSH connection cannot be made, then the remote tunnels are opened. This setup works when I reboot the _closet computer_, if I unplug my router, or if my internet service is out completely.

I don't have to worry about my husband Corey accidentally tripping over the power cable because I know that he will plug it back in. When the _closet computer_ has powered back on, the tunnels open shortly after, and I'm offline for about five minutes for a small hiccup. For a self-hosted webserver, I think this is acceptable.

## @TODO

While this is a step towards higher availability for my website, I still have some distance to cover before I have true resilience. Last Sunday, Feb 6 at 12pm PST, a 3.1 earthquake occurred, its epicenter barely a mile from my home, and it could have cut my internet connection!

I could simply host my website from the _bastion server_, but physically it's located in San Jose, CA, which isn't much further from that particular earthquake's epicenter than my house is.

As an exercise, I want to duct tape my own high availability together while still serving my website from my _closet computer_ as primary. It is fun to tell people that my professional site is coming out of the closet, after all.
