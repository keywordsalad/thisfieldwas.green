# Readme

## Setup

Run the following command to obtain the necessary software. A newer version of
`make` is installed with this, and your path will need to be changed to support
it. The specific directory to add is shown in the command's output:

```shell
brew bundle
```

Then install Hakyll:

```
stack install hakyll
```

### Troubleshooting:

**`stack install hakyll` fails on macOS:**

If this error occurs:
`hakyll> <command line>: can't load framework: Cocoa (not found)`

Follow [this workaround][] to build `macos11ghcwa.dylib` and then re-run the
command with the path to the library:

```
DYLD_INSERT_LIBRARIES="<PATH_TO>/macos11ghcwa.dylib" stack install hakyll
```

[this workaround]: https://github.com/yairchu/macos11-haskell-workaround/
