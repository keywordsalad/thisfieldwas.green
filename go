#!/usr/bin/env bash
# This "./go" script is the build script.
# For context behind the "./go" script, please read these:
# https://blog.thepete.net/blog/2014/03/28/_-attributes-of-an-amazing-dev-toolchain/
# https://code.ofvlad.xyz/vlad/lightning-runner
set -e

_verify-prerequisites () {
  git config core.hooksPath .githooks

  if ! command -v stack &> /dev/null
  then
      _bad-message "Install haskell-stack to continue"
      exit 1
  fi

  if ! command -v hakyll-init &> /dev/null
  then
    stack install hakyll
    if [ $? -ne 0 ]; then
      _bad-message "Failed to install Hakyll, check README.md for troubleshooting"
      exit 1
    fi
  fi
}

⚡build () {
  _help-line "Compile the site generator and generate the site"
  stack build
  stack exec site build
  ⚡favicon
  ⚡default_og_image
}

⚡clean () {
  _help-line "Clean generated site files"
  rm -rf _cache/* _site/*
}

⚡clean_all () {
  _help-line "Clean generated site files and site generator binaries"
  ⚡clean
  stack clean
}

⚡rebuild () {
  _help-line "Clean and then rebuild the generated site"
  ⚡clean
  ⚡build
}

⚡rebuild_all () {
  _help-line "Clean and then rebuild both the generated site and the site generator binary"
  ⚡clean_all
  ⚡build
}

⚡watch () {
  _help-line "Build the site generator, generate the site, and then serve it so that it may be viewed in a browser"
  ⚡build
  stack exec site watch
}

⚡publish () {
  _help-line "Build the site and then publish it live"
  current_branch="$(git branch --show-current)"
  if [[ "$current_branch" -ne "main" ]]; then
    _bad-message "Can only publish from main branch; tried to publish from $current_branch"
    exit 1
  fi
  ⚡test_sync "main"
  ⚡build

  if [ ! -d _site ] || [ -z "$(ls -A _site)" ]; then
    git clone --branch _site "$(git config --get remote.origin.url)" _site
  fi

  sha="$(git log -1 HEAD --pretty=format:%h)"
  pushd ./_site
  test_sync "_site"
  git add .
  git commit -m "Build on $(date) generated from $sha"
  git push origin "_site"
  rsync -ahp * closet.oflogan.xyz:/usr/share/nginx/thisfieldwas.green/
  popd
}

⚡test_sync () {
  _help-line "Verify that the current or specified local branch is up to date with the remote branch"

  branch=${1:-$(git branch --show-current)}
  git switch $branch
  git fetch origin $branch

  rev_parse_remote="$(git rev-parse origin/$branch)"
  rev_parse_local="$(git rev-parse $branch)"

  if [ "$rev_parse_local" != "$rev_parse_remote" ]; then
    _bad-message "Branch $branch not in sync with remote!"
    exit 1
  fi

  _good-message "Local branch $branch is up to date with remote"
}

⚡test () {
  _help-line "Run hspec tests"
  stack test
}

⚡force-publish () {
  _help-line "Publish generated site as-is. Only use this for emergencies!"
  rsync -ahp _site/* closet.oflogan.xyz:/usr/share/nginx/thisfieldwas.green/
}

⚡datestamp () {
  _help-line "Generate ISO-8601 datestamp with time and offset"
  DATE=$(date +"%Y-%m-%dT%H:%M:%S%:z")
  echo "$DATE" | pbcopy
  echo "Copied to clipboard: $DATE"
}

⚡favicon () {
  _help-line "Generate favicon from grass.svg"
  src_file="$(pwd)/site/images/grass.svg"
  out_file="$(pwd)/_site/favicon.ico"

  mkdir -p _cache/favicon_tmp
  pushd _cache/favicon_tmp

  sizes=(16 32 48 64 96 128 256)
  for x in ${sizes[@]}; do
    inkscape -w $x -h $x -b "aliceblue" -o $x.png "$src_file"
  done

  files=("${sizes[@]/%/.png}")
  convert "${files[@]}" "$out_file"
  identify "$out_file"
  popd
}

⚡default_og_image () {
  _help-line "Generate og:image for open graph"
  src_file="$(pwd)/site/images/grass.svg"
  out_file="$(pwd)/_site/grass_og-image.png"
  inkscape -w 1024 -h 1024 -b "aliceblue" -o "$out_file" "$src_file"
  identify "$out_file"
}

source ⚡
