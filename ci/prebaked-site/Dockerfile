FROM registry.thisfieldwas.green/tfwg/site-base-image:latest

COPY site-repo /site-repo

RUN set -eux; \
  pushd /site-repo; \
  stack build --only-dependencies; \
  stack test --only-dependencies; \
  popd; \
  rm /site-repo/*; \
  rm /site-repo/.*;

CMD ["stack"]
