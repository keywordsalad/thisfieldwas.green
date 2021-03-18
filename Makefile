.PHONY: build clean rebuild clean-exe watch publish

build:
	set -e
	stack build
	stack exec site build

clean:
	set -e
	rm -rf _cache/*

rebuild: clean build

clean-exe:
	stack clean

watch: build
	set -e
	stack exec site watch

publish: build
	set -e
	commit="$$(git log -1 HEAD --pretty=format:%H)"
	sha="$${commit:0:8}"

	pushd ./_site
	test_sync "gh-pages"
	git add .
	git commit -m "Build on $$(date) generated from $sha"
	git push origin "gh-pages"
	popd

.ONESHELL:
