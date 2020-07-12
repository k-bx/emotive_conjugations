#!/usr/bin/env sh

set -ex;

sudo apt-get -y install curl wget;

if ! command -v stack &> /dev/null
then
    echo "stack not found. installing..."
    curl -sSL https://get.haskellstack.org/ | sh
fi

sudo apt-get -y install libpq-dev libtinfo-dev nodejs npm

if ! command -v elm &> /dev/null
then
    echo "elm not found. installing..."
    (cd /tmp && \
         curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz && \
         gunzip elm.gz && \
         chmod +x elm && \
         sudo mv elm /usr/local/bin/)
fi

if ! command -v shake &> /dev/null
then
    echo "shake not found. installing..."
    (cd && stack install shake)
fi

if ! command -v sass &> /dev/null
then
    echo "sass not found. installing..."
    sudo npm install -g sass
fi

if ! command -v uglifyjs &> /dev/null
then
    echo "uglifyjs not found. installing..."
    sudo npm install -g uglify-js
fi
