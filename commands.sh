#!/usr/bin/env bash
set -e

ARGS=()

if [ -n "$VERBOSE" ]; then
  ARGS+=("--verbose")
fi

init () {
  git config core.hooksPath .githooks
  brew bundle

  stack install hakyll
  if [ $? -ne 0 ]; then
    echo "Failed to install Hakyll, check README.md for troubleshooting"
    exit 1
  fi

  echo
  echo "Setup completed successfully"
  echo
}

build () {
  if ! command -v stack &> /dev/null; then
    init
  fi

  stack build
  stack exec site build -- ${ARGS[@]}
}

clean () {
  rm -rf _cache/*
}

clean_all () {
  clean
  stack clean
  rm -rf _cache/* _site/*
}

rebuild () {
  clean
  build
}

touch_all () {
  # touch all files so they're built again
  touch site
}

rebuild_all () {
  clean_all
  touch_all
  build
}

watch () {
  build
  stack exec site watch -- ${ARGS[@]}
}

publish () {
  init

  current_branch="$(git branch --show-current)"
  if [[ "$current_branch" -ne "main" ]]; then
    echo "Can't publish from current branch: $current_branch"
    exit 1
  fi
  test_sync "main"
  build

  if [ ! -d _site ] || [ -z "$(ls -A _site)" ]; then
    git clone --branch _site "$(git config --get remote.origin.url)" _site
  fi

  sha="$(git log -1 HEAD --pretty=format:%h)"
  pushd ./_site
  test_sync "_site"
  git add .
  git commit -m "Build on $(date) generated from $sha"
  git push origin "_site"
  scp -r * thisfieldwas.green:/var/www/thisfieldwas.green/
  popd
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

upload () {
  rsync -ahp _site/* closet.oflogan.xyz:/usr/share/nginx/thisfieldwas.green/
}

datestamp () {
    DATE=$(date +"%Y-%m-%dT%H:%M:%S%z")
    echo "$DATE" | pbcopy
    echo "Copied to clipboard: $DATE"
}

