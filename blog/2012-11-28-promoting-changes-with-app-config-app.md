---
layout: post
title: "Promoting changes with App-Config-App"
author: "Logan McGrath"
date: 2012-11-28T13:04:00-05:00
published: 2012-11-28T13:04:00-05:00
comments: false
tags: AngularJS, Perforce, SCM, Sinatra, Configuration Management
---

The App-Config-App now lets you promote changes between environments!

<!--more-->

## How does it work?

Perforce lets you create mappings to define the relationship between two
diverging code branches. This allows for easy integration of changes between the
two branches by referencing the name of the mapping.

> See [Perforce's documentation][] for more details on the how and why of
> branch mappings.

The App-Config-App reads these branch mappings in order to create paths for
promotion between environments.

## Promoting changes with App-Config-App

The App-Config-App `setup_example.rb` creates four branches with the following
mappings:

``` markdown
Mapping        Source    Destination
------------------------------------
dev-qa         dev       qa
qa-staging     qa        staging
staging-prod   staging   prod
```

If you login to App-Config-App and go to "Promote Changes," you get an interface
showing these relationships:

$img("/images/app-config3/promote_changes.png")$

Changes between environments can be promoted in either direction along a mapping
configuration. The receiving environment accepts all changes (developers would
know this as an 'accept-theirs' resolution) and you are then allowed to review
the changes by clicking on the "Pending Changes" link.

For example, I've promoted changes from "qa" to "dev":

$img("/images/app-config3/promote_result.png")$

I can then review the changes by clicking on "Pending Changes":

$img("/images/app-config3/pending_changes.png")$

Changes may be edited or reverted before committing them.

## Promoting changes using P4V

[P4V][] is the
Perforce visual client. Using P4V, you have much greater control over how
changes get promoted, but it requires a little more work.

I've connected P4V to my App-Config-App user workspace to perform the same
promotion from "qa" to "dev":

$img("/images/app-config3/p4v.png")$

Select the "qa" folder, then from the menu bar go to "Actions" >
"Merge/Integrate". This will bring up a wizard for performing the integration.

Select the following:

``` markdown
Merge method: "Use branch mapping"
Branch mapping: "dev-qa"
Automatically resolve files after merging: checked
Resolve option: "Accept source"
```

And ensure the direction of integration is "Target" < "Source":

$img("/images/app-config3/p4v_integrate.png")$

Finally, click "Merge". If you expand the "dev" folder, you can see the where
the changes are:

$img("/images/app-config3/p4v_integrate_result.png")$

You are now free to modify the files further before finally committing the
changes.

### How it compares

You get greater options when using P4V to promote changes, but producing the
same result as App-Config-App's default behavior is fairly involved. If you
aren't paying attention or don't know what you're doing, you might break
something :(

## @TODO

### More options for resolving changes

When you promote changes in App-Config-App, the source changes will overwrite
the destination. This behavior reduces the chance for a conflict to happen, but
it means you really have to pay attention to what's changed in the destination
config and possibly edit the config further before finally committing it.

### Conflict resolution

If a conflict occurs after promoting changes, a screen should be available for
viewing and editing the conflicting changes.

### Better error reporting if promotion fails due to permissions

Users with read-only access to multiple environments will still be able to
promote changes between them. The promotion doesn't actually occur (the files
remain unchanged) but the application doesn't report any errors when this
happens.

[Perforce's documentation]: http://www.perforce.com/perforce/doc.current/manuals/p4v/Managing_branch_specifications.html
[P4V]: http://www.perforce.com/product/components/perforce_visual_client
