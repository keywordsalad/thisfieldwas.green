FROM registry.thisfieldwas.green/tfwg/site-base-image:latest

COPY site-repo /site-repo

RUN set -eux; \
  cd /site-repo; \
  stack build --only-dependencies; \
  stack test --only-dependencies; \
  cd ..; \
  rm /site-repo/*; \
  rm /site-repo/.*;

CMD ["stack"]
