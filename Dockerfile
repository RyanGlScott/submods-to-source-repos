FROM alpine:3.15

ARG USER_ID
ARG GROUP_ID

RUN apk add --no-cache \
      bash \
      binutils \
      binutils-gold \
      bsd-compat-headers \
      curl \
      gcc \
      g++ \
      gmp-dev \
      libffi-dev \
      make \
      perl \
      ncurses-dev \
      tar \
      xz

RUN addgroup -g $GROUP_ID user && \
      adduser --disabled-password -g '' -u $USER_ID -G user user
USER user
ENV HOME /home/user
WORKDIR $HOME

ENV GHCUP_HOME $HOME/.ghcup/bin
ENV GHCUP $GHCUP_HOME/.ghcup

RUN mkdir -p $GHCUP_HOME && \
      curl https://downloads.haskell.org/~ghcup/0.1.17.4/x86_64-linux-ghcup-0.1.17.4 > $GHCUP && \
      chmod a+x $GHCUP && \
      $GHCUP install ghc 9.0.1 && \
      $GHCUP install cabal 3.6.2.0

ENTRYPOINT ["bash"]
