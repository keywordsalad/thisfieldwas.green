#!/usr/bin/env bash

build () {
  stack build
  stack exec site build
}

rebuild () {
  rm -r _cache/*
  ./build
}

watch () {
  stack exec site watch
}

publish () {
  commit="$(git log -1 HEAD --pretty=format:%H)"
  sha="${commit:0:8}"

  pushd ./_site
  test_sync "gh-pages"
  git add .
  git commit -m "Build on $(date) generated from $sha"
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
    echo "INFO: Local branch $branch is ahead of remote"
  elif [ "$merge_base" == "$rev_parse_local" ]; then
    echo "ERROR: Local branch $branch is behind remote!"
    exit 1
  else
    echo "INFO: Local branch $branch is up to date with remote"
  fi
}
