#!/usr/bin/env sh

set -ex;

for host in ubuntu@batonbooks ubuntu@meetup; do
    ssh $host sudo systemctl stop conj-webapp ;
    ssh $host mkdir -p /home/ubuntu/conj ;
    ssh $host mkdir -p /home/ubuntu/conj/sysadmin ;
    rsync -v -az $(stack exec -- which conj) $host:conj/ ;
    rsync -v -az ./sysadmin/conj-webapp.service $host: ;
    ssh $host sudo mv ./conj-webapp.service /etc/systemd/system/ ;
    rsync -v -az -C ./sysadmin/conj.dhall $host: ;
    rsync -v -az -C ./sysadmin/aws_credentials $host:conj/sysadmin/aws_credentials ;
    # if ! ssh $host grep -q conj ~/.aws/credentials ; then
    #     rsync -az -C ./sysadmin/aws_credentials $host:.aws/aws_credentials_conj ;
    #     ssh $host "cat .aws/aws_credentials_conj >> .aws/credentials"
    # fi
    rsync -v -az -C ./sysadmin/conj.dhall $host: ;
    ssh $host sudo systemctl daemon-reload ;
    ssh $host sudo systemctl restart conj-webapp ;
done
