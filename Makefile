build:
	set -e; source commands.sh; build
.PHONY: build

clean:
	set -e; source commands.sh; clean
.PHONY: clean

clean-all:
	set -e; source commands.sh; clean_all
.PHONY: clean-all

rebuild:
	set -e; source commands.sh; rebuild
.PHONY: rebuild

rebuild-all:
	set -e; source commands.sh; rebuild_all
.PHONY: rebuild-all

watch:
	set -e; source commands.sh; watch
.PHONY: watch

publish:
	set -e; source commands.sh; publish
.PHONY: publish

init:
	set -e; source commands.sh; init
.PHONY: init

test:
	set -e; source commands.sh; test
.PHONY: test
