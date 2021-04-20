# Readme

## Setup

Run the following command to set up the necessary dependencies:
```shell
make init
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
