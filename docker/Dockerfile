FROM debian:9-slim
MAINTAINER Chris Done

################################################################################
# Haskell system dependencies (basically never changes)

RUN apt-get update && \
    apt-get install -yq --no-install-suggests --no-install-recommends --force-yes -y -qq \
            netbase git ca-certificates xz-utils build-essential curl unzip libgmp-dev

################################################################################
# Download a specific Stack version

RUN curl https://github.com/commercialhaskell/stack/releases/download/v2.1.3/stack-2.1.3-linux-x86_64-static.tar.gz \
    --silent -L \
    -o stack.tar.gz && \
    tar zxf stack.tar.gz && mv stack-2.1.3-linux-x86_64-static/stack /usr/bin/

################################################################################
# Clone a base repo, this determines the LTS and GHC version

RUN curl --silent https://github.com/chrisdone/duta/archive/99f9c2fba71f5701a79b8e5acd55f42f5ecdcbb4.zip -L \
    -o duta.zip && unzip duta.zip -d duta && rm duta.zip

WORKDIR duta/duta-99f9c2fba71f5701a79b8e5acd55f42f5ecdcbb4

################################################################################
# Install the right GHC version and update package index

RUN stack setup && stack update

################################################################################
# Install the snapshot and system dependencies

RUN apt-get install -y libz-dev libicu-dev libpq-dev libspf2-dev
RUN stack build --only-snapshot
