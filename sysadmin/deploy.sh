#!/usr/bin/env sh

set -ex;

for host in ubuntu@conj ubuntu@batonbooks ubuntu@meetup; do
    ssh $host sudo systemctl stop conj-webapp || true;
    ssh $host mkdir -p /home/ubuntu/conj ;
    ssh $host sudo apt-get -y install postgresql libpq-dev;
    rsync -v -az --progress dist/ $host:conj/ ;
    ssh $host sudo mv /home/ubuntu/conj/sysadmin/conj-webapp.service /etc/systemd/system/ ;
    if ! ssh $host grep -q conj /home/ubuntu/.aws/credentials ; then
        ssh $host mkdir -p /home/ubuntu/.aws
        ssh $host "cat /home/ubuntu/conj/sysadmin/aws_credentials >> /home/ubuntu/.aws/credentials"
    fi
    # rsync -v -az -C ./sysadmin/conj.dhall $host: ;
    scp /home/kb/Dropbox/conf/emotive_conjugations/conj-webapp.dhall $host:/home/ubuntu/conj.dhall
    # ssh $host cp /home/ubuntu/conj/sysadmin/conj.dhall /home/ubuntu/
    ssh $host sudo systemctl daemon-reload ;
	ssh $host sudo systemctl enable conj-webapp.service ;
    ssh $host sudo systemctl restart conj-webapp ;
done
