FROM archlinux:base

ENV PATH="/root/.local/bin:$PATH"

RUN pacman -Sy
RUN pacman -S --noconfirm sh curl
RUN pacman -S --noconfirm git
RUN pacman -S --noconfirm make
RUN pacman -S --noconfirm dart-sass
RUN pacman -S --noconfirm imagemagick
RUN pacman -S --noconfirm inkscape
RUN pacman -S --noconfirm stack

CMD ["stack"]
