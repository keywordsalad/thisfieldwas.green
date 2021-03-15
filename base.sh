#!/usr/bin/env bash

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
