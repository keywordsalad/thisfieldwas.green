---
title: High availability for closet colocation
author: Logan McGrath
comments: false
date: 2021-12-27T11:14:21-08:00
tags: tinfoil hat, configuration management, self host
layout: post
---
My website is coming out of the closet. [No, really]({{route "_posts/2021-12-11-reasons-why-my-website-is-offline.md"}}), it’s hosted from an old computer tower that I keep in my bedroom closet; an unconventional deployment architecture indeed. It’s particularly burdened by any number of reasons for poor uptime.

_In this post, I will explore options through which I aim to achieve high availability even while keeping my website in the closet._

## The problem space

Recall from my last post: _my website comes out of the closet because I feel that I can’t trust third party providers with my data_. I contemplate to my navel the effort I expend to keep my data secure, considering the nature of my data.

### My navel, too, wears a tinfoil hat

With time, my navel meets my gaze and wisely intuits that bits, regardless of the device that exchanges them, should be considered read and even copied when exchanged. Bits therefore must be encrypted before they are exchanged.

Without encryption, I cannot trust any device to exchange them securely unless I control the device with complete confidence--and I only half-know what I’m doing, so I can’t smoke test for cracks. I also cannot trust what happens when my data reaches the other end of the pipe, as it may be decrypted at any point by the third party. Therefore my calculus regarding data security becomes guided by risk assessment rather than strict avoidance.

## Coming out of the closet as a risk assessment

The bastion server in the current configuration simply passes bits that are encrypted via SSL from its port 443 to SSH tunnels that terminate at port 443 of the closet computer. This means that the bastion is nothing more than a pass-through: it doesn’t know what requests are coming in for my website or what responses are being returned. All traffic that it receives are encrypted. This is by design, of course, as I don’t want linode having access to my data. 

Port 80 is also forwarded the same way, even though its traffic is unencrypted. The closet computer redirects all requests on port 80 to port 443 regardless. Port 22 is also forwarded, and the traffic is completely opaque to the bastion.

This configuration doesn’t allow the bastion to cache requests on port 443 because they are encrypted: at the bastion I don’t know what’s being requested so that I can cache the response. As a risk assessment, these requests are for pages from a site that hosts a static blog whose content pines for a reader, therefore there is no risk associated with exchanging these bits with linode.

As the site in its entirety can be held in cache on the bastion, then this presents an obvious and very simple option:

### The bastion as the host

I even called this out in my last post as the obvious, better way to keep my website up. 

But I don’t like this option.

## Knitting a sweater

If I want a sweater that’s made using a nice wool, specific stitch, and have a particular fit, then I’m probably going to knit the sweater myself. The sweater may require that I learn about or perform a great deal of things. Like shaving a yak for wool so that I can spin the yarn I need to knit the sweater

Obviously I could purchase wool yarn, but I would deprive myself of shaving the yak. The _yak shave_ is related to the IKEA effect in that the outputs carry an artificially high value. I have shaved many yaks, as, unlike assembling IKEA furniture, I enjoy shaving yaks. I cherish my yaks, especially the hairier ones, and I pain to part with them.

Getting my website to come out of the closet was an accomplishment for me, and I want to keep that piece of architecture in place. The next yak I will need to shave is caching the responses on the bastion while keeping traffic on either side of the server encrypted.

### The bastion as the cache

In order for the bastion to act as a cache, it will have to terminate SSL. This is pretty easy. I’m a little annoyed that my bastion server now has to be configured for every domain I host where before it was a simple case of shuffling everything from one port to another, though, but at least I can automate the configuration through ansible.

There are then two options for securing the traffic between the bastion and the closet computer.

Terminate SSL then SSH tunnel binds port 80 to localhost 

Port 443 no longer needs to be served as the SSL cert for the closet computer is no longer publicly accessible anyway. Port 22 is already secure. Port 80, however, is serving everything that was once on port 443 so it needs to be restricted to localhost only. This allows the SSH tunnel to the bastion to bind to localhost:80, which the web server will be listening on, without allowing every device on the home network to be able to see the traffic.










— TODO verify processes on localhost also can’t see port 80 traffic 

SSL handoff to closet computer 

— TODO how to SSL the closet to the bastion
