FROM debian:buster


# Update latest version of packages
RUN apt-get update && apt-get upgrade && apt-get install -y apt-utils


# Configure locales
RUN apt-get install -y locales
RUN echo 'LANG=en_US.UTF-8' >/etc/default/locale
RUN echo 'en_US.UTF-8 UTF-8' >/etc/locale.gen
RUN locale-gen


# Configure timezone
RUN echo 'Europe/Amsterdam' >/etc/timezone
RUN cp /usr/share/zoneinfo/Europe/Amsterdam /etc/localtime


# Terminal setup
RUN apt-get install -y \
    ncurses-bin        \
    ncurses-term

COPY wasm_terminal_build/extern/gd100-gdterm/terminfo/g /etc/terminfo/g


# General tools
RUN apt-get install -y \
    curl               \
    neofetch           \
    sqlite3            \
    sqlite3-doc        \
    sqlite3-pcre       \
    tmux               \
    elinks             \
    git                \
    man-db

# Text editors
RUN apt-get install -y \
    ed                 \
    neovim             \
    vim

# Games
RUN apt-get install -y \
    bsdgames           \
    netris             \
    nudoku             \
    nethack-console

# C/C++ tools
RUN apt-get install -y \
    clang              \
    gcc                \
    gdb                \
    cmake              \
    binutils

# Python tools
RUN apt-get install -y \
    python3            \
    python3-pip        \
    python3-virtualenv

# R tools
RUN apt-get install -y \
    r-base             \
    r-base-dev         \
    r-recommended

# Lua tools
RUN apt-get install -y \
    lua5.3             \
    luajit

# TCL tools
RUN apt-get install -y \
    tcl

# Erlang tools
RUN apt-get install -y \
    erlang

RUN curl -o /usr/local/bin/rebar3 https://s3.amazonaws.com/rebar3/rebar3
RUN chmod a+x /usr/local/bin/rebar3

# Node tools
RUN apt-get install -y \
    nodejs             \
    npm


# Configure user
RUN useradd guest --create-home --shell /bin/bash
RUN echo 'set enable-bracketed-paste on' >/home/guest/.inputrc

RUN sed -i '1i\color_prompt=yes' /home/guest/.bashrc
RUN printf "\n\
export PATH='/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games'\n\
\n\
export LC_ALL=en_US.UTF-8\n\
export LANG=en_US.UTF-8\n\
export LANGUAGE=en_US.UTF-8\n\
" >>/home/guest/.bashrc

ADD https://raw.githubusercontent.com/dextercd/dotfiles/master/irregular/.tmux.conf /home/guest/

RUN chown -R guest:guest /home/guest/
