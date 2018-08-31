FROM ubuntu:16.04

ENV LANG C.UTF-8

RUN \
    apt-get update && \
    apt-get -y install \
        curl \
        libgmp3-dev \
        libsndfile1-dev \
        libxft-dev \
        libxrandr-dev \
        libxss-dev

RUN mkdir -p $HOME/.local/bin
RUN curl -L https://github.com/commercialhaskell/stack/releases/download/v1.7.1/stack-1.7.1-linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C $HOME/.local/bin '*/stack'
