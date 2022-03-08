FROM registry.thisfieldwas.green/tfwg/base-site:latest

COPY site-source /site-source

RUN set -eux; \
  cd /site-source; \
  ./go prebake; \
  cd ..; \
  rm -rf /site-source;

CMD ["stack"]
