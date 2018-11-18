elm := node_modules/.bin/elm
elm-format := node_modules/.bin/elm-format
elm-test := node_modules/.bin/elm-test


.PHONY: default
default: build test


.PHONY: test
test: $(elm-test)
	$(elm-test)
	$(elm-format) --validate src tests


.PHONY: build
build: $(elm)
	@tail -c1 elm.json | read -r _ || echo >> elm.json
	$(elm) make


.PHONY: format
format: $(elm-format)
	$(elm-format) --yes src tests


.PHONY: clean
clean:
	$(RM) -r elm-stuff node_modules


$(elm) $(elm-format) $(elm-test):
	npm install --no-package-lock
	@touch -c $@
