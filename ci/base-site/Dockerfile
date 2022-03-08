FROM ubuntu:latest

RUN set -eux; \
  DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends tzdata;
RUN set -eux; \
  DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends locales;

RUN set -eux; \
  locale-gen en_US.UTF-8; \
  update-locale LANG=en_US.UTF-8;

RUN set -eux; \
  DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends curl;

ENV PATH "/root/.local/bin:$PATH"
RUN set -eux; \
  curl -sSL https://get.haskellstack.org/ | sh;

RUN set -eux; \
  DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends sass;
RUN set -eux; \
  DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends imagemagick;
RUN set -eux; \
  DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends inkscape;

CMD ["stack"]
