#!/bin/sh

set -ex

dt=$(date -u +%Y%m%d%H%M%S)
dbname_remote=conj
dbname_local=conj
fname=$dbname_remote-$dt.pgdump.gz
ssh -t conj "PGPASSWORD=password pg_dump -h localhost -U postgres $dbname_remote | gzip > /tmp/$fname"
scp conj:/tmp/$fname ~/tmp/$fname
PGPASSWORD=password dropdb -h localhost -U postgres $dbname_local
PGPASSWORD=password psql -h localhost -U postgres -c "CREATE DATABASE $dbname_local"
gunzip -c ~/tmp/$fname | PGPASSWORD=password psql -h localhost -U postgres $dbname_local
