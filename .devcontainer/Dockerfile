FROM mcr.microsoft.com/devcontainers/base:debian-12

ARG GHC_VERSION=9.10.3
ARG CABAL_VERSION=3.12.1.0

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install --yes --no-install-recommends \
    build-essential \
    curl \
    git \
    libffi-dev \
    libgmp-dev \
    libncurses-dev \
    libtinfo-dev \
    pkg-config \
    xz-utils \
    zlib1g-dev \
 && rm -rf /var/lib/apt/lists/*

USER vscode

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_MINIMAL=1 \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    PATH=/home/vscode/.ghcup/bin:/home/vscode/.cabal/bin:${PATH}

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh \
 && printf '\n[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"\n' >> /home/vscode/.profile \
 && printf '\n[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"\n' >> /home/vscode/.bashrc \
 && . /home/vscode/.ghcup/env \
 && ghcup install ghc ${GHC_VERSION} \
 && ghcup set ghc ${GHC_VERSION} \
 && ghcup install cabal ${CABAL_VERSION} \
 && ghcup set cabal ${CABAL_VERSION} \
 && ghcup install hls \
 && cabal update \
 && ghc --version \
 && cabal --version \
 && haskell-language-server-wrapper --version
