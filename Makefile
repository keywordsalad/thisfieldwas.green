build:
	set -e; source "commands.sh"; build

clean:
	set -e; source "commands.sh"; clean

rebuild:
	set -e; source "commands.sh"; rebuild

watch:
	set -e; source "commands.sh"; watch

publish:
	set -e; source "commands.sh"; publish

.ONESHELL:
.PHONY: publish watch rebuild build clean
