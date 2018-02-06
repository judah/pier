#!/bin/bash

IMAGE=snoyberg/stackage:nightly

stack --docker --docker-image $IMAGE  build pier -j 1
stack --docker --docker-image $IMAGE  exec pier build $(cat packages.txt)
