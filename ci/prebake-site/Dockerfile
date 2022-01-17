FROM archlinux:latest

RUN set -eux; \
  pacman -Sy --noconfirm git imagemagick inkscape; \
  git clone https://aur.archlinux.org/pacaur.git; \
  pushd pacaur; \
  makepkg -si; \
  popd; \
  pacaur -S --noedit --noconfirm stack-bin;

COPY site-repo /site-repo

RUN set -eux; \
  pushd /site-repo; \
  stack build --only-dependencies; \
  stack test --only-dependencies; \
  popd; \
  rm /site-repo/*; \
  rm /site-repo/.*;

CMD ["stack"]
