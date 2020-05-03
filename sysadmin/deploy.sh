#!/usr/bin/env sh

set -ex;

for host in ubuntu@conj ubuntu@batonbooks ubuntu@meetup; do
    ssh $host sudo systemctl stop conj-webapp || true;
    rsync -v -az --progress dist/ $host:conj/ ;
    ssh $host sudo systemctl restart conj-webapp ;
done
