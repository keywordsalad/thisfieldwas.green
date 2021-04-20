#!/usr/bin/env bash
set -e

new_make_path="/usr/local/opt/make/libexec/gnubin"

init () {
  git config core.hooksPath .githooks
  brew bundle

  stack install hakyll
  if [ $? -ne 0 ]; then
    echo "Failed to install Hakyll, check README.md for troubleshooting"
    exit 1
  fi

  if [[ "$PATH" != *"$new_make_path"* ]]; then
    echo
    echo "A new version of make has been installed."
    echo "Configure your \$PATH and rerun the command:"
    echo
    echo "export PATH=$new_make_path:\$PATH"
    echo
    exit 1
  fi

  echo
  echo "Setup completed successfully"
  echo
}

build () {
  if [[ "$PATH" != *"$new_make_path"* ]]; then
    echo "ERROR: Run `make init` first"
    exit 1
  fi

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
  paths=(
    README.md
    about-me.md
    blog
    code
    contact.md
    css
    images
    index.md
    js
    partials
    resume.md
    templates
  )
  find "$paths" -type f -exec touch {} +
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
  init

  current_branch="$(git branch --show-current)"
  if [[ "$current_branch" -ne "main" ]]; then
    echo "Can't publish from current branch: $current_branch"
    exit 1
  fi
  test_sync "main"
  build

  sha="$(git log -1 HEAD --pretty=format:%h)"
  pushd ./gh-pages
  test_sync "gh-pages"
  git add .
  git commit -m "Build on $(date) generated from $sha"
  git push origin "gh-pages"
  scp -r * thisfieldwas.green:/var/www/thisfieldwas.green/
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

