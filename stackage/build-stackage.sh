#!/bin/bash
set -xueo pipefail

IMAGE=snoyberg/stackage:nightly
PLAN=lts-12.8
LTSPATH=stackage/_pier/downloads/stackage/plan/$PLAN.yaml
cat >stackage/pier.yaml <<EOF
resolver: $PLAN
system-ghc: true
EOF
PACKAGES=stackage/packages.txt
PIER_ARGS="--download-local -j 2 --pier-yaml=stackage/pier.yaml"
STACK="time stack --docker --docker-image $IMAGE"
PIER="${STACK} exec -- pier ${PIER_ARGS}"

$STACK build pier -j 1

$PIER clean
$PIER setup --keep-temps

$STACK runghc stackage/list-packages.hs -- $LTSPATH \
    > $PACKAGES

${PIER} build --keep-going -V $(cat $PACKAGES)
