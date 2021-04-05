build:
	set -e; source "commands.sh"; build

clean:
	set -e; source "commands.sh"; clean

clean-all:
	set -e; source "commands.sh"; clean_all

rebuild:
	set -e; source "commands.sh"; rebuild

rebuild-all:
	set -e; source "commands.sh"; rebuild_all

watch:
	set -e; source "commands.sh"; watch

publish:
	set -e; source "commands.sh"; publish

setup:
	set -e; source "commands.sh"; setup

.ONESHELL:
.PHONY: publish watch rebuild build clean
