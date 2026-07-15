# Readme

## Setup

Install the system dependencies (Haskell Stack and Sass) with Homebrew:

```shell
brew bundle
```

The build is driven by the `./go` task runner. Run it with no arguments to see
the available subcommands. On first use it verifies prerequisites and installs
Hakyll if needed, so you can jump straight to building or previewing the site:

```shell
./go build     # compile the generator and generate the site into _site/
./go watch     # build, then serve a live preview at http://localhost:8000
./go test      # run the test suite
```

See [`AGENTS.md`](AGENTS.md) for a full tour of the repository, commands, and
content conventions.

### Troubleshooting:

**`stack install hakyll` fails on macOS:**

If this error occurs:
`hakyll> <command line>: can't load framework: Cocoa (not found)`

Follow [this workaround](https://github.com/yairchu/macos11-haskell-workaround/) to build `macos11ghcwa.dylib` and then re-run the
command with the path to the library:

```shell
DYLD_INSERT_LIBRARIES="<PATH_TO>/macos11ghcwa.dylib" stack install hakyll
```
