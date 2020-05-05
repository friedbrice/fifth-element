.PHONY: list
list: ## Show available targets.
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' | sed -e 's/##\s*\(.*\)/\n\t\1\n/'

.PHONY: clean
clean: ## Delete project-local Npm and Spago caches.
	rm -rf .cache
	rm -rf .psci_modules
	rm -rf .spago
	rm -rf dist
	rm -rf generated-docs
	rm -rf node_modules
	rm -rf output
	rm -f package-lock.json
	rm -f tags

node_modules/.bin/spago:
	npm install

node_modules/.bin/purs:
	npm install

node_modules/.bin/parcel:
	npm install

.PHONY: setup
setup: node_modules/.bin/spago node_modules/.bin/purs node_modules/.bin/parcel ## Install development tools into project-local cache.

.PHONY: build
build: setup ## Build project.
	npx spago build

.PHONY: repl
repl: build ## Load project interactively in the Purescript REPL.
	npx spago repl

.PHONY: watch
watch: build ## Recompile on file changes.
	npx pscid

.PHONY: dist
dist: build dist/chess.svg dist/sprites.png ## Package files for distribution

dist/chess.svg:
	mkdir -p dist
	cp assets/Chess_Pieces_Sprite.svg dist/chess.svg

dist/sprites.png:
	mkdir -p dist
	cp assets/ProjectUtumno_full.png dist/sprites.png

.PHONY: dev
dev: dist ## Host local dev server that updates when files are changed.
	npx parcel index.html

.PHONY: test
test: build ## Run tests.
	npx spago test

.PHONY: docs
docs: setup ## Generate documentation.
	npx spago docs
