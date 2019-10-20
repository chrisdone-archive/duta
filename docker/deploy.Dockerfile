FROM chrisdone/duta-build as builder
MAINTAINER Chris Done

################################################################################
# Clone down the latest repo

RUN curl --silent https://github.com/chrisdone/duta/archive/ac8110473708255e16a87107af04438a7ebd9526.zip -L \
    -o duta.zip && unzip duta.zip && rm duta.zip
WORKDIR duta-ac8110473708255e16a87107af04438a7ebd9526

################################################################################
# Build the local packages

RUN stack build

################################################################################
# Cleanup and copy binaries to /opt/duta

RUN mkdir -p /opt/duta && \
    cp $(stack exec which duta-smtp-receiver) /opt/duta/ && \
    cp $(stack exec which duta-web) /opt/duta/ && \
    rm -rf .stack-work .stack && \
    cd .. && rm -rf duta /duta /root/.stack && \
    apt-get purge git xz-utils build-essential curl unzip -y && \
    apt-get autoremove -y

################################################################################
# Reset the image and install basic deps

FROM debian:9-slim
COPY --from=builder /opt/duta /opt/duta
RUN apt-get update && \
    apt-get install -yq --no-install-suggests --no-install-recommends --force-yes -y -qq \
            netbase ca-certificates libgmp-dev libz-dev libicu-dev libpq-dev libspf2-dev
