SRC = $(shell find src -name '*.elm')

ELM = elm

build: elm.js

elm.js: $(SRC)
	$(ELM) make src/Main.elm --output=$@

AUTHFILE = env/auth

index.html: index.html.tmpl $(AUTHFILE)
	python3 mk_index.py > index.html

watch:
	fswatch src/*.elm index.html | while read f; do echo $$f; make build; done
