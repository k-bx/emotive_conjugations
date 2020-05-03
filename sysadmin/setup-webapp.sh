#!/usr/bin/env sh

set -ex;

for host in ubuntu@conj; do
    ssh $host apt-get -y install haproxy postgresql libpq-dev;
    # TODO: put haproxy cfg
    # TODO: put letsencrypt certs and setup their renewal
done
