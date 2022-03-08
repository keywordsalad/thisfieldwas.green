FROM ubuntu:latest

RUN DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends tzdata
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends locales

RUN set -eux; \
  locale-gen en_US.UTF-8; \
  update-locale LANG=en_US.UTF-8;

RUN DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends curl

ENV PATH "/root/.local/bin:$PATH"
RUN set -eux; \
  curl -sSL https://get.haskellstack.org/ | sh

RUN DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends sass
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends imagemagick
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends inkscape

CMD ["stack"]
