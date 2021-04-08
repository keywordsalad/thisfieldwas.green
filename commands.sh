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
}

clean_all () {
  clean
  stack clean
  rm -rf gh-pages/*
}

rebuild () {
  clean
  build
}

touch_all () {
  # touch all files so they're built again
  find . -type f -exec touch {} +
}

rebuild_all () {
  clean_all
  touch_all
  build
}

watch () {
  build
  stack exec site watch
}

publish () {
  current_branch="$(git branch --show-current)"
  if [[ "$current_branch" -ne "main" ]]; then
    echo "Can't publish from current branch: $current_branch"
    exit 1
  fi
  test_sync "main"
  build

  commit="$$(git log -1 HEAD --pretty=format:%H)"
  sha="$${commit:0:8}"
  pushd ./gh-pages
  test_sync "gh-pages"
  git add .
  git commit -m "Build on $$(date) generated from $sha"
  git push origin "gh-pages"
  popd

  git add .
  git commit -m "Update gh-pages generated from $sha"
  git push origin main
}

test_sync () {
  branch=$1
  git switch $branch
  git fetch origin $branch

  rev_parse_remote="$(git rev-parse origin/$branch)"
  rev_parse_local="$(git rev-parse $branch)"

  if [ "$rev_parse_local" != "$rev_parse_remote" ]; then
    echo "ERROR: Branch $branch not in sync with remote!"
    exit 1
  fi

  echo "INFO: Local branch $branch is up to date with remote"
}

test () {
  stack test
}

