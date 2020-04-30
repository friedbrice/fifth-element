.PHONY: list
list: ## Show available targets
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' | sed -e 's/##\s*\(.*\)/\n\t\1\n/'

.PHONY: clean
clean: ## Delete project-local Npm and Spago caches
	rm -rf .psci_modules
	rm -rf .spago
	rm -rf generated-docs
	rm -rf node_modules
	rm -rf output
	rm -f package-lock.json

node_modules/.bin/spago:
	npm install

node_modules/.bin/purs:
	npm install

.PHONY: setup
setup: node_modules/.bin/spago node_modules/.bin/purs ## Install development tools into project-local cache.

.PHONY: build
build: setup ## Build project
	npx spago build

.PHONY: repl
repl: setup ## Load project interactively in the Purescript REPL
	npx spago repl

.PHONY: watch
watch: setup ## Recompile on file changes
	npx pscid

.PHONY: test
test: setup ## Run tests
	npx spago test

.PHONY: run
run: setup ## Run app
	npx spago run

.PHONY: docs
docs: setup ## Generate documentation
	npx spago docs

.PHONY: bundle
bundle: clean test docs ## Package app for distribution
	# npx spago bundle-app
