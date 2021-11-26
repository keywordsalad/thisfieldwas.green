---
title: App-Config-App in Action
author: Logan McGrath
date: 2012-11-20T17:00:00-06:00
published: 2012-11-20T17:00:00-06:00
comments: false
tags: AngularJS, Perforce, SCM, Sinatra, Configuration Management
layout: post
changefreq: yearly
---

Paul Hammant found this cool [Server-Side Piano][] and I've modified it to be
configurable from a running App-Config-App. Because the sound is generated at
the server, you're able to see (hear) the Server-Side Piano change its
configuration without reloading the UI.

<!--more-->

{{youtube video: "hZbQhF6fsEo", id: "app-config-demo"}}

## Making it work for yourself

I've updated the [App-Config-App][] with additional configuration to support
choosing which instrument the Server-Side Piano will play. A clean install of
App-Config-App using `setup_examples.rb` will provide everything needed to run
the Server-Side Piano.

The application's configuration URL and credentials are located in `web.xml`.
Additional details may be found in the application's [README][].

[Server-Side Piano]: https://github.com/lmcgrath/angular-java-server-midi
[App-Config-App]: https://github.com/lmcgrath/app-config-app
[README]: https://github.com/lmcgrath/angular-java-server-midi/blob/master/README.markdown
