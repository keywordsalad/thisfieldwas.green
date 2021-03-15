# Readme

## Setup

```shell
brew install haskell-stack
stack install hakyll
```

### Troubleshooting:

**Build fails on macOS:** 

`hakyll> <command line>: can't load framework: Cocoa (not found)`

1. Follow [this workaround](https://github.com/yairchu/macos11-haskell-workaround/) to build `macos11ghcwa.dylib`
2. Re-run command with `DYLD_INSERT_LIBRARIES="<PATH_TO>/macos11ghcwa.dylib" stack install hakyll`
