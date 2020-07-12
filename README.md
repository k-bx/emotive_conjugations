# emotive_conjugations

In rhetoric, emotive or emotional conjugation mimics the form of a grammatical conjugation of an irregular verb to illustrate humans’ tendency to describe their own behavior more charitably than the behavior of others.

## Setup

`make setup-local`

## Building

Currently only Linux is supported, PRs adding macOS and checking WSL2 on Windows appreciated. Also, macOS isn't too hard to figure out from reading Makefile.

`PATH="~/.local/bin:$PATH" make`

or simply `make` if you have `~/.local/bin` in your `$PATH` already.

## Running

To just run webapp:

```
(cd dist && ./conj webapp)
```

## Developing

For development build which automatically rebuilds on file change:

```
$(stack path --local-install-root)/bin/conj dev-webapp
```
