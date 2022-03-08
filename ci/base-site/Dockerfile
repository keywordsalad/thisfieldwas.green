FROM archlinux:base

ENV PATH="/root/.local/bin:$PATH"

RUN pacman -Sy
RUN pacman -S sh curl
RUN pacman -S dart-sass
RUN pacman -S imagemagick
RUN pacman -S inkscape
RUN pacman -S stack

CMD ["stack"]
