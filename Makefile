VIRTUAL_ENV?=$(shell pwd)/venv
pip=$(VIRTUAL_ENV)/bin/pip3
python=$(VIRTUAL_ENV)/bin/python3

.PHONY: prod
prod:
	stack build
	stack install

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
