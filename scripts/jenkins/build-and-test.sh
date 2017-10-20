#!/bin/bash

set -e
set -o pipefail

ssh-add "$HOME/.ssh/id_ed25519_semmc"

export GHC_VER=$1

ABSOLUTE_PATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/$(basename "${BASH_SOURCE[0]}")"
ABSOLUTE_DIR=$(dirname ${ABSOLUTE_PATH})
ROOT=${ABSOLUTE_DIR}/../..

# Tools
export GHC=/opt/ghc/${GHC_VER}/bin
CABAL=/opt/cabal/2.0/bin
HAPPY=/opt/happy/1.19.5/bin
ALEX=/opt/alex/3.1.7/bin

pushd "${ROOT}"

echo Building on "$(hostname -f)"
/sbin/ifconfig -a

echo "Updating submodules"
git submodule update --init

export PATH=$GHC:$CABAL:$HAPPY:$ALEX:$PATH

echo "Environment is:"
printenv

echo "Updating hackage index"
cabal update || exit 1

echo "Testing ghc"
/opt/ghc/${GHC_VER}/bin/ghc --version

cabal new-build renovate-x86 || exit 1
cabal new-build renovate-ppc || exit 1
