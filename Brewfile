tap "dart-lang/dart"
brew "haskell-stack"
brew "sass"
# Provides `cwebp`, used by the site build to transcode PNG/JPEG images to
# near-lossless WebP (see src/Green/Site/Images.hs). Required for the build.
brew "webp"
# Optional — only needed to (re)generate favicons in `./go favicons`
# (invoked by `build`/`publish`). inkscape rasterizes site/images/grass.svg;
# imagemagick provides `convert`/`identify` to assemble favicon.ico. When these
# are absent (e.g. a work machine that can't install the inkscape cask), `./go`
# skips favicon generation and retains the existing favicons instead.
brew "imagemagick"
cask "inkscape"
