#!/usr/bin/env bash

VERSION=0.1

HOME=/home/user
GHCUP_HOME=${HOME}/.ghcup
CABAL=${GHCUP_HOME}/bin/cabal-3.6.2.0
GHC=${GHCUP_HOME}/bin/ghc-9.0.1
TOOL_NAME=submods-to-source-repos
TARGET=exe:${TOOL_NAME}
BINDIST=${TOOL_NAME}-${VERSION}.xz

${CABAL} update --ignore-project
${CABAL} build ${TARGET} --with-compiler ${GHC} --enable-executable-static
EXE=$(${CABAL} list-bin ${TARGET} --with-compiler ${GHC})
strip "${EXE}"
ls -l "${EXE}"
xz -c "${EXE}" > ${BINDIST}
ls -l ${BINDIST}
sha256sum ${BINDIST}
