VIRTUAL_ENV?=$(shell pwd)/venv
pip=$(VIRTUAL_ENV)/bin/pip3
python=$(VIRTUAL_ENV)/bin/python3

.PHONY: install_venv
install_python:
	test -f $(python) || virtualenv -p /usr/bin/python3 venv
	$(pip) install -r ./requirements.txt
	test -d scripts/download-articles/news-please || (cd scripts/download-articles && git clone https://github.com/fhamborg/news-please.git)
	cd scripts/download-articles/news-please && $(pip) install -r ./requirements.txt && $(python) setup.py install
