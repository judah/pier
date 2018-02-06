#!/bin/bash

stack --docker --docker-image fpco/stack-build:lts-10.3  build pier -j 1
stack --docker --docker-image fpco/stack-build:lts-10.3  exec pier build pier
