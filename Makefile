VIRTUAL_ENV?=$(shell pwd)/venv
pip=$(VIRTUAL_ENV)/bin/pip3
python=$(VIRTUAL_ENV)/bin/python3

.PHONY: install_venv
install_python:
	test -f $(python) || virtualenv -p /usr/bin/python3 venv
	$(pip) install -r ./requirements.txt
	# mkdir -p vendor
	# test -d vendor/news-please || (cd vendor && git clone https://github.com/fhamborg/news-please.git)
	cd vendor/news-please && $(pip) install -r ./requirements.txt && $(python) setup.py install

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

.PHONY: prod
prod:
	stack build
	stack install

.PHONY: deploy
deploy:
	./sysadmin/deploy.sh
