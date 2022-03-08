FROM registry.thisfieldwas.green/tfwg/base-site:latest

COPY site-source /site-source

RUN set -eux; \
  pushd /site-source; \
  ./go prebake; \
  popd ..; \
  rm -rf /site-source/*;

CMD ["stack"]
