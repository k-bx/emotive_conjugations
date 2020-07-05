VIRTUAL_ENV?=$(shell pwd)/venv
STACK ?= stack
DIST ?= ./dist
pip=$(VIRTUAL_ENV)/bin/pip3
python=$(VIRTUAL_ENV)/bin/python3
# CONJ=$$(stack exec -- which conj)
CONJ=$$(stack path --local-install-root)/bin/conj
DIST ?= ./dist

.PHONY: dev
dev:
	$(MAKE) pre
	$(MAKE) hs-dev
	$(MAKE) generate-elm
	$(MAKE) elm-dev
	$(MAKE) sass
	$(MAKE) hs-post
	# cp -r ./ssl $(DIST)/

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
	$(STACK) build --fast --ghc-options="-j +RTS -A32M -RTS"

.PHONY: hs-prod
hs-prod:
	$(STACK) build --allow-different-user --ghc-options="-j +RTS -A32M -RTS" --test --no-run-tests

.PHONY: hs-post
hs-post:
	rm -f $(DIST)/conj
	cp $(CONJ) $(DIST)/
	mkdir -p $(DIST)/sysadmin
	rm -rf $(DIST)/sysadmin
	cp -r sysadmin/ $(DIST)/

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
	./optimise.sh

.PHONY: install_deps
install_deps:
	npm install -g uglify-js

.PHONY: install_venv
install_python:
	test -f $(python) || virtualenv -p /usr/bin/python3 venv
	$(pip) install -r ./requirements.txt
	# mkdir -p vendor
	# test -d vendor/news-please || (cd vendor && git clone https://github.com/fhamborg/news-please.git)
	# cd vendor/news-please && $(pip) install -r ./requirements.txt && $(python) setup.py install
	# newspaper deps
	sudo apt-get install libxml2-dev libxslt-dev
	sudo apt-get install libjpeg-dev zlib1g-dev libpng-dev
	curl https://raw.githubusercontent.com/codelucas/newspaper/master/download_corpora.py | $(python)
	# this one is big, only do on desktop
	$(python) -m spacy download en_core_web_lg
	$(python) -m spacy download en_core_web_md
	# $(python) -m spacy download en_vectors_web_lg
	mkdir -p ~/tmp/emotive_conjugations/data/fasttext/ && curl -C -o ~/tmp/emotive_conjugations/data/fasttext/amazon_review_full.ftz https://dl.fbaipublicfiles.com/fasttext/supervised-models/amazon_review_full.ftz

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

.PHONY: setup
setup:
	./sysadmin/setup.sh

.PHONY: setup-local
setup-local:
	./sysadmin/setup-local.sh

.PHONY: setup-webapp-python
setup-webapp-python:
	cd sysadmin && ./setup-webapp-python.sh

.PHONY: deploy-webapp-python
deploy-webapp-python:
	sudo systemctl restart conj-webapp-python

.PHONY: setup-queue-worker
setup-queue-worker:
	cd sysadmin && ./setup-queue-worker.sh

.PHONY: deploy-queue-worker
deploy-queue-worker:
	$(STACK) install
	cd sysadmin && ./deploy-queue-worker.sh

.PHONY: launch-ssh-tunnels
launch-ssh-tunnels:
	par "ssh -N -L 6667:batonbooks:6666 batonbooks" "ssh -N -L 6668:meetup:6666 meetup"

.PHONY: recreate-db
recreate-db:
	PGPASSWORD=password psql -h localhost -U postgres -c 'DROP DATABASE conj' || true
	PGPASSWORD=password psql -h localhost -U postgres -c 'CREATE DATABASE conj' || true
	$(CONJ) migrate

copy-prod-db:
	./sysadmin/copy-prod-db.sh
