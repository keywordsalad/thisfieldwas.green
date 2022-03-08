FROM ubuntu:latest

RUN set -eux; \
  apt-get update; \
  DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends tzdata;

RUN apt-get install -y curl

ENV PATH="/root/.local/bin:$PATH"
RUN set -eux; \
  curl -sSL https://get.haskellstack.org/ | sh

RUN apt-get install -y sass
RUN apt-get install -y imagemagick
RUN apt-get install -y inkscape

CMD ["stack"]
