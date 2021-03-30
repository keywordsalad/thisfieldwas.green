#!/usr/bin/env bash
set -e

setup () {
  brew install haskell-stack
  stack install hakyll
  brew install sass/sass/sass
}

build () {
  stack build
  stack exec site build
}

clean () {
  rm -rf _cache/*
  stack clean
}

clean_all () {
  clean
  rm -rf gh-pages/*
}

rebuild () {
  clean
  build
}

touch_site_src () {
  # touch all files in site-src so they're built again
  find site-src -type f -exec touch {} +
}

rebuild_all () {
  clean_all
  touch_site_src
  build
}

watch () {
  build
  stack exec site watch
}

publish () {
  current_branch="$(git branch --show-current)"
  if [ "$current_branch" -ne "develop" ]; then
    echo "Can't publish from current branch: $current_branch"
    exit 1
  fi
  test_sync "develop"
  build

  commit="$$(git log -1 HEAD --pretty=format:%H)"
  sha="$${commit:0:8}"
  pushd ./gh-pages
  test_sync "gh-pages"
  git add .
  git commit -m "Build on $$(date) generated from $sha"
  git push origin "gh-pages"
  popd
}

test_sync () {
  branch=$1
  git switch $branch
  git fetch origin $branch

  merge_base="$(git merge-base $branch origin/$branch)"
  rev_parse_remote="$(git rev-parse origin/$branch)"
  rev_parse_local="$(git rev-parse $branch)"

  if [ "$merge_base" == "$rev_parse_remote" ]; then
    echo "ERROR: Local branch $branch is ahead of remote!"
    exit 1
  elif [ "$merge_base" == "$rev_parse_local" ]; then
    echo "ERROR: Local branch $branch is behind remote!"
    exit 1
  fi

  echo "INFO: Local branch $branch is up to date with remote"
}
