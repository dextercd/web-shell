FROM debian:bullseye


# Update latest version of packages
RUN apt-get --assume-yes update && apt-get --assume-yes upgrade && apt-get install --assume-yes --no-install-recommends apt-utils


# Configure timezone
RUN echo 'Europe/Amsterdam' >/etc/timezone
RUN cp /usr/share/zoneinfo/Europe/Amsterdam /etc/localtime


# Terminal setup
RUN apt-get install         \
    --assume-yes            \
    --no-install-recommends \
    ncurses-bin             \
    ncurses-term

COPY wasm_terminal_build_release/extern/gd100-gdterm/terminfo/g /etc/terminfo/g


# General tools
RUN apt-get install         \
    --assume-yes            \
    --no-install-recommends \
    ca-certificates         \
    ssh-client              \
    curl                    \
    neofetch                \
    sqlite3                 \
    sqlite3-doc             \
    sqlite3-pcre            \
    tmux                    \
    elinks                  \
    less                    \
    git                     \
    git-doc                 \
    git-email               \
    git-svn                 \
    patch                   \
    xdg-user-dirs           \
    bzip2                   \
    xz-utils                \
    manpages                \
    manpages-dev            \
    man-db

# Text editors
RUN apt-get install         \
    --assume-yes            \
    --no-install-recommends \
    ed                      \
    universal-ctags         \
    neovim                  \
    vim

# Games
RUN apt-get install         \
    --assume-yes            \
    --no-install-recommends \
    bsdgames                \
    netris                  \
    nudoku                  \
    nethack-console

# C/C++ tools
RUN apt-get install         \
    --assume-yes            \
    --no-install-recommends \
    clang                   \
    gcc                     \
    glibc-doc               \
    libstdc++-10-doc        \
    gdb                     \
    cmake                   \
    cmake-doc               \
    ninja-build             \
    autoconf                \
    automake                \
    make                    \
    build-essential         \
    binutils

# Python tools
RUN apt-get install         \
    --assume-yes            \
    --no-install-recommends \
    python3                 \
    python3-doc             \
    python3-pip             \
    python3-venv            \
    python3-virtualenv

# Lua tools
RUN apt-get install         \
    --assume-yes            \
    --no-install-recommends \
    lua5.3                  \
    luajit

# TCL tools
RUN apt-get install         \
    --assume-yes            \
    --no-install-recommends \
    tcl

# Erlang tools, excluding wx and jinterface
RUN apt-get install         \
    --assume-yes            \
    --no-install-recommends \
    erlang-nox              \
    erlang-doc              \
    erlang-manpages

RUN curl -o /usr/local/bin/rebar3 https://s3.amazonaws.com/rebar3/rebar3
RUN chmod a+x /usr/local/bin/rebar3

# Node tools
RUN apt-get install         \
    --assume-yes            \
    --no-install-recommends \
    nodejs                  \
    npm


# Configure locales

RUN apt-get install         \
    --assume-yes            \
    --no-install-recommends \
    locales

RUN printf "\n\
en_IE.UTF-8 UTF-8\n\
nl_NL.UTF-8 UTF-8\n\
" >/etc/locale.gen

RUN locale-gen


# Configure user
RUN useradd guest --create-home --shell /bin/bash
RUN echo 'set enable-bracketed-paste on' >/home/guest/.inputrc

RUN sed -i '1i\color_prompt=yes' /home/guest/.bashrc
RUN printf "\n\
export PATH='/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games'\n\
" >>/home/guest/.bashrc

ADD https://raw.githubusercontent.com/dextercd/dotfiles/master/irregular/.tmux.conf /home/guest/

RUN chown -R guest:guest /home/guest/

# Env
ENV LANG=en_IE.UTF-8           \
    LC_MONETARY=nl_NL.UTF-8    \
    LC_MEASUREMENT=nl_NL.UTF-8 \
    LC_PAPER=nl_NL.UTF-8       \
    LC_TELEPHONE=nl_NL.UTF-8   \
    LC_TIME=nl_NL.UTF-8
