#!/usr/bin/env sh

set -ex;

sudo cp ./conj-webapp-python.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable conj-webapp-python.service
sudo systemctl restart conj-webapp-python
