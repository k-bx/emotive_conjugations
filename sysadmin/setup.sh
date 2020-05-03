#!/usr/bin/env sh

set -ex;

for host in ubuntu@conj ubuntu@batonbooks ubuntu@meetup; do
    ssh $host sudo systemctl stop conj-webapp || true;
    ssh $host mkdir -p /home/ubuntu/conj ;
    ssh $host sudo apt-get -y install postgresql libpq-dev;
    rsync -az -qi --progress dist/ $host:conj/ ;
    ssh $host sudo mv /home/ubuntu/conj/sysadmin/conj-webapp.service /etc/systemd/system/ ;
    if ! ssh $host grep -q conj /home/ubuntu/.aws/credentials ; then
        ssh $host mkdir -p /home/ubuntu/.aws
        ssh $host "cat /home/ubuntu/conj/sysadmin/aws_credentials >> /home/ubuntu/.aws/credentials"
    fi
    scp /home/kb/Dropbox/conf/emotive_conjugations/conj-default.dhall $host:/home/ubuntu/conj-default.dhall
    scp /home/kb/Dropbox/conf/emotive_conjugations/conj-webapp.dhall $host:/home/ubuntu/conj.dhall
    ssh $host sudo systemctl daemon-reload ;
	ssh $host sudo systemctl enable conj-webapp.service ;
    ssh $host sudo systemctl restart conj-webapp ;
done
