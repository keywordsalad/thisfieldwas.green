---
layout: post
title: "Using Perforce Chronicle for application configuration"
author: "Logan McGrath"
created: 2012-11-07T13:54:00-05:00
published: 2012-11-07T13:54:00-05:00
tags: Perforce, Configuration Management
---

Following Paul Hammant's post [App-config workflow using SCM][] and subsequent
[proof of concept][] backed by Git, I will show that an app-config application
backed by Perforce is possible using [Perforce Chronicle][].

<!--more-->

## Perforce and permissions for branches

[Perforce][] is an enterprise-class source control management (SCM) system,
remarkably similar to Subversion (Subversion was inspired by Perforce :)
Perforce is more bulletproof than Subversion in many ways and it's generally
faster. Git does not impose any security constraints or permissions on branches,
Perforce gives comprehensive security options allowing you to control access to
different branches: for example, development, staging, and production.
Subversion, however, can support permissions on branches with some extra
configuration (Apache plus mod_dav_svn/mod_dav_authz). For these reasons,
Perforce is a better option for storing configuration data than either Git or
Subversion.

## Perforce CMS as an application server

[Perforce Chronicle][] is a content management system (CMS) using Perforce as
the back-end store for configuration and content. The app-config application is
built on top of Chronicle because Perforce does not offer a web view into the
depot the way Subversion can through Apache. Branching and maintaining
divergence between environments can be managed through the user interface, and
Chronicle provides user authentication and management, so access between
different configuration files can be restricted appropriately. The INSTALL.txt
file that is distributed with Chronicle helps with an easy install, mine being
set up to run locally from `http://localhost`.

There is a key issue in using Chronicle, however. The system is designed for the
management of _content_ and not necessarily arbitrary _files_. In order to make
the app-config application work, I had to add a custom content type and write a
module. Configuration and HTML are both plain-text content, so I created a "
Plain Text" content type with the fields _title_ and _content_:

1. Go to "Manage" > "Content Types"
1. Click "Add Content Type"
1. Enter the following information:

```ini
Id:       plaintext
Label:    Plain Text
Group:    Assets
Elements:

[title]
type = text
options.label = Title
options.required = true
display.tagName = h1
display.filters.0 = HtmlSpecialChars

[content]
type = textarea
options.label = "Content"
options.required = true
display.tagName = pre
display.filters.0 = HtmlSpecialChars
```

Click "Save".

## The Config App

I've borrowed heavily from Paul's [app-config HTML page][], which uses
[AngularJS][] to manage the UI and interaction with the server. Where Paul's
app-config app used the [jshon][] command to encode and decode JSON, Zend
Framework has a utility class for encoding, decoding, and pretty-printing JSON,
and Chronicle also ships with the [simplediff][] utility for performing diffs
with PHP.

The source JSON configuration is the same, albeit sorted:

$include-code("json", "app-config/stack_configuration.json")$

The `index.html` page has been modified from the original to support only the
basic _commit_ and _diffs_ functionality:

$include-code("html", "app-config/index.html")$

Both of these assets were added by performing:

1. Click "Add" from the top navbar
1. Click "Add Content"
1. Select "Assets" > "Plain Text"
1. For "Title", enter "`index.html`" or "`stack_configuration.json`"
1. Paste in the appropriate "Content"
1. Click "URL", select "Custom", and enter the same value as "Title" (otherwise,
   Chronicle will convert underscores to dashes, so be careful!)
1. Click "Save", enter a commit message, then click the next "Save"
1. Both assets should be viewable as mangled Chronicle content entries
   from `http://localhost/index.html`
   and `http://localhost/stack_configuration.json`. _You normally will not use
   these URLs_.

At this point, neither asset is actually usable. Most content is heavily
decorated with additional HTML and then displayed within a layout template, but
I want both the `index.html` and `stack_configuration.json` assets to be
viewable as standalone files and provide a REST interface for AngularJS to work
against.

## Come back PHP! All is forgiven

Chronicle is largely built using [Zend Framework][] and makes adding extra
modules to the system pretty easy. My module needs to be able to display
plaintext assets, update their content using an `HTTP POST`, and provide diffs
between the last commit and the current content.

To create the module, the following paths need to be added:

* `INSTALL/application/appconfig`
* `INSTALL/application/appconfig/controllers`
* `INSTALL/application/appconfig/views/scripts/index`

Declare the module with `INSTALL/application/appconfig/module.ini`:

$include-code("ini", "app-config/module/module.ini")$

Add a view script for displaying plaintext
assets, `INSTALL/application/appconfig/views/scripts/index/index.phtml`:

$include-code("php", "app-config/module/views/scripts/index/index.phtml")$

Add a view script for displaying
diffs, `INSTALL/application/appconfig/views/scripts/index/diffs.phtml`:

$include-code("php", "app-config/module/views/scripts/index/diffs.phtml")$

And a controller
at `INSTALL/application/appconfig/controllers/IndexController.phtml`:

$include-code("php", "app-config/module/controllers/IndexController.php")$

## AngularJS

After all files are in place, Chronicle needs to be notified that the new module
exists by going to "Manage" > "Modules", where the "Appconfig" module will be
listed if all goes well :) Both assets will now be viewable
from `http://localhost/appconfig/index.html`
and `http://localhost/appconfig/stack_configuration.json`.
AngularJS' [resource service][] is used in `index.html` to fetch
stack_configuration.json and post changes back.

From `http://localhost/appconfig/index.html`, the data from
stack_configuration.json is loaded into the form:

$img("/images/app-config/start.png")$

Edits to stack_configuration.json can be made using the form, and the diffs
viewed by clicking on "View Diffs":

$img("/images/app-config/diffs.png")$

The changes can be saved by entering a commit message and clicking "Commit
Changes". After which, clicking "View Diffs" will show no changes:

$img("/images/app-config/diffs-after-commit.png")$

To show that edits have in fact been made to stack_configuration.json, go
to `http://localhost/stack_configuration.json`, select "History" and click on "
History List":

$img("/images/app-config/history.png")$

Chronicle also provides an interface for viewing diffs between revisions:

$img("/images/app-config/history-diffs.png")$

## Disk Usage

Something to remember in using Chronicle is that each resource requested from
Perforce is written to disk before being served to the client. This means that
for each request to `index.html`, Chronicle allocates a new Perforce workspace,
checks out the associated file, serves it to the client, then deletes the file
and the workspace at the end of the request. This allocate/checkout/serve/delete
cycle executes for stack_configuration.json and every other resource in the
system.

## @TODO

### Security!

There's one major flaw with the appconfig module: it performs zero access
checks. By default, Chronicle can be configured to disallow anonymous access by
going to "Manage" > "Permissions" and deselecting all permissions for "
anonymous" and "members". Logging out and attempting to access
either `http://localhost/appconfig/stack_configuration.json`
or `http://localhost/appconfig/index.html` will now give an error page and
prompt you to log in. Clicking "New User" will also give an error, as anonymous
users don't have the permission to create users.

Access rights on content are checked by the content module, but are also
hard-coded in the associated controllers as IF-statements. A better solution
will be required for proper access management in the appconfig module.

### Better integration

Chronicle's content module provides JSON integration for most of its actions,
but these mostly exist to support the [Dojo Toolkit]-enabled front-end.
Integrating with these actions over JSON requires detailed knowledge of
Chronicle's form structures.

Chronicle has some nice interfaces for viewing diffs. If I could call those up
from `index.html` I would be major happy :)

### Automatic creation of plaintext content type

Before the appconfig module is usable, the plaintext content type has to be
created. I would like to automate creation of the plaintext content type when
the module is first enabled.

### Making applications aware of updates to configuration

When stack_configuration.json is updated, there's no way to notify applications
to the change, and no interface provided so they may poll for changes. I'm not
entirely sure at this point what an appropriate solution would look like. In
order to complete the concept, I'd first have to create a client app dependent
on that configuration.

### Better interfaces for manipulating plaintext assets

I had to fiddle with `index.html` quite a bit. This basically involved editing a
local copy of `index.html`, then pasting the entire contents into the associated
form in Chronicle. I have not tried checking out `index.html` directly from
Perforce, and I imagine that any edits would need to be made within Chronicle.
Github offers an in-browser raw editor, and something like that would be real
handy in Chronicle.

### Handling conflicts

There is no logic in the appconfig module to catch conflicts if there are two
users editing the same file. Conflicts are detectable because an exception is
thrown if there is a conflict, but I'm not sure what the workflow for resolution
is in Chronicle terms, or how to integrate with it. Who wins?

### Working with branches

I did not take the time to see how Chronicle manages branches. I will need to
verify that Chronicle and the appconfig module can work with development,
staging, and production branches, with maintained divergence. For example, we're
still trying to figure out how to attach visual clients like P4V to the
repository and work independently of Chronicle.

## Kudos

I would like to thank the guys at Perforce for their assistance and answering
all my questions as I worked with Chronicle, especially Randy Defauw.

[proof of concept]: http://paulhammant.com/2012/08/14/app-config-using-git-and-angular/
[Perforce]: http://en.wikipedia.org/wiki/Perforce
[Perforce Chronicle]: http://www.perforce.com/products/chronicle
[app-config HTML page]: https://github.com/paul-hammant/app-config-app/blob/master/index.html
[resource service]: http://code.angularjs.org/0.9.19/docs-0.9.19/#!/api/angular.service.resource
[App-config workflow using SCM]: http://paulhammant.com/2012/07/10/app-config-workflow-using-scm/
[AngularJS]: http://angularjs.org/
[jshon]: http://kmkeen.com/jshon/
[simplediff]: https://github.com/paulgb/simplediff/
[Zend Framework]: http://framework.zend.com/
[Dojo Toolkit]: http://dojotoolkit.org/
