FROM ubuntu:latest

RUN set -eux; \
  apt-get update; \
  apt-get install -y imagemagick inkscape; \
  curl -sSL https://get.haskellstack.org/ | sh;

CMD ["stack"]
