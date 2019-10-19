FROM chrisdone/duta-build as builder
MAINTAINER Chris Done

RUN git clone https://github.com/chrisdone/duta.git --depth 1
WORKDIR duta
RUN stack build
RUN mkdir -p /opt/duta && \
    cp $(stack exec which duta-smtp-receiver) /opt/duta/ && \
    cp $(stack exec which duta-web) /opt/duta/ && \
    rm -rf .stack-work .stack && \
    cd .. && rm -rf duta /duta /root/.stack && \
    apt-get purge git xz-utils build-essential curl unzip -y && \
    apt-get autoremove -y

FROM debian:9-slim
COPY --from=builder /opt/duta /opt/duta
RUN apt-get update && \
    apt-get install -yq --no-install-suggests --no-install-recommends --force-yes -y -qq \
            netbase ca-certificates libgmp-dev libz-dev libicu-dev libpq-dev libspf2-dev
