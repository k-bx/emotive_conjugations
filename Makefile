VIRTUAL_ENV?=$(shell pwd)/venv
STACK ?= stack
DIST ?= ./dist
pip=$(VIRTUAL_ENV)/bin/pip3
python=$(VIRTUAL_ENV)/bin/python3
CONJ=$$(stack exec -- which conj)
DIST ?= ./dist

.PHONY: dev
dev:
	$(MAKE) pre
	$(MAKE) hs-dev
	$(MAKE) generate-elm
	$(MAKE) elm-dev
	$(MAKE) sass
	$(MAKE) hs-post
	cp -r ./ssl $(DIST)/

.PHONY: prod
prod:
	$(MAKE) pre
	$(MAKE) hs-prod
	$(MAKE) generate-elm
	$(MAKE) elm-prod
	$(MAKE) sass
	$(MAKE) hs-post
	mkdir -p $(DIST)/ssl
	stack install
	# cd $(DIST)/ssl && ln -sf /etc/letsencrypt/live/mywebsite.com/{privkey.pem,cert.pem} .

.PHONY: pre
pre:
	mkdir -p $(DIST)

.PHONY: generate-elm
generate-elm:
	$(CONJ) gen-elm > elm/Le/.Api.elm
	mv elm/Le/.Api.elm elm/Le/Api.elm

#
# haskell backend
#

.PHONY: hs-dev
hs-dev:
	$(STACK) build --fast --ghc-options="-j +RTS -A32M -RTS" --test --no-run-tests

.PHONY: hs-prod
hs-prod:
	$(STACK) build --allow-different-user --ghc-options="-j +RTS -A32M -RTS" --test --no-run-tests

.PHONY: hs-post
hs-post:
	rm -f $(DIST)/conj
	cp $(CONJ) $(DIST)/
	mkdir -p $(DIST)/sysadmin
	cp -r sysadmin/aws_credentials $(DIST)/sysadmin/

#
# elm
#

.PHONY: elm-dev
elm-dev:
	$(MAKE) elm-pre
	elm make --debug elm/Index.elm --output=$(DIST)/app.js
	cp $(DIST)/app.js $(DIST)/app.min.js
	$(MAKE) elm-post

.PHONY: elm-prod
elm-prod:
	$(MAKE) elm-pre
	rm -rf ./elm-stuff
	elm make elm/Index.elm --optimize --output=$(DIST)/app.js
	$(MAKE) elm-optimize
	$(MAKE) elm-post

.PHONY: elm-pre
elm-pre:
	@echo elm-pre

.PHONY: sass
sass:
	$(CONJ) shake
	# ./shakefile.hs
	# sass ./static/css/index.scss ./static/css/index.css --source-map --error-css
	$(MAKE) elm-post

.PHONY: elm-post
elm-post:
	cp -r ./static/* $(DIST)/

js := $(DIST)/app.js
min := $(DIST)/app.min.js

.PHONY: elm-optimize
elm-optimize:
	set -o pipefail; uglifyjs $(js) --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=$(min)
	@echo "Compiled size:$$(cat $(js) | wc -c) bytes  ($(js))"
	@echo "Minified size:$$(cat $(min) | wc -c) bytes  ($(min))"
	@echo "Gzipped size: $$(cat $(min) | gzip -c | wc -c) bytes"
	rm -f $(js)

.PHONY: install_venv
install_python:
	test -f $(python) || virtualenv -p /usr/bin/python3 venv
	$(pip) install -r ./requirements.txt
	# mkdir -p vendor
	# test -d vendor/news-please || (cd vendor && git clone https://github.com/fhamborg/news-please.git)
	cd vendor/news-please && $(pip) install -r ./requirements.txt && $(python) setup.py install
	# newspaper deps
	sudo apt-get install libxml2-dev libxslt-dev
	sudo apt-get install libjpeg-dev zlib1g-dev libpng-dev
	curl https://raw.githubusercontent.com/codelucas/newspaper/master/download_corpora.py | $(python)

.PHONY: tags
tags:
	hasktags -e .
	mv TAGS TAGS01
	find . -type f -name "*.elm" -print | etags --language=none --regex=@elm.tags -
	mv TAGS TAGS02
	cat TAGS02 >> TAGS
	rm TAGS02
	cat TAGS01 >> TAGS
	rm TAGS01

.PHONY: clean
clean:
	stack clean

.PHONY: deploy
deploy:
	./sysadmin/deploy.sh

.PHONY: launch-ssh-tunnels
launch-ssh-tunnels:
	par "ssh -N -L 6667:batonbooks:6666 batonbooks" "ssh -N -L 6668:meetup:6666 meetup"

.PHONY: recreate-db
recreate-db:
	PGPASSWORD=password psql -h localhost -U postgres -c 'DROP DATABASE conj' || true
	PGPASSWORD=password psql -h localhost -U postgres -c 'CREATE DATABASE conj' || true
	$(CONJ) migrate
