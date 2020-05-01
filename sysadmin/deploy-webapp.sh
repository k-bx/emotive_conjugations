#!/usr/bin/env sh

set -ex;

for host in ubuntu@conj; do
    ssh $host sudo systemctl stop conj-webapp || true;

    ssh $host apt-get -y install haproxy;

    ssh $host sudo systemctl daemon-reload ;
	ssh $host sudo systemctl enable conj-webapp.service ;
    ssh $host sudo systemctl restart conj-webapp ;
done
