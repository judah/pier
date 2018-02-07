#!/bin/bash
set -xueo pipefail

IMAGE=snoyberg/stackage:nightly

stack --docker --docker-image $IMAGE build pier -j 1
time stack --docker --docker-image $IMAGE \
    exec -- \
    pier build $(cat packages.txt) \
    --pier-yaml=example/pier.yaml \
    --shake-arg=--keep-going
