#!/usr/bin/env sh

set -ex;

sudo apt-get -y install curl wget;

if ! command -v stack &> /dev/null
then
    echo "stack not found. installing..."
    curl -sSL https://get.haskellstack.org/ | sh
fi
