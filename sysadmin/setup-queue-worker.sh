#!/usr/bin/env sh

set -ex;

sudo cp ./conj-queue-worker.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable conj-queue-worker.service
sudo systemctl restart conj-queue-worker
