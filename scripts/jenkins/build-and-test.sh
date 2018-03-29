#!/bin/bash

set -e
set -o pipefail

# Export LOCAL_TEST=1 for local testing to avoid trying to add this
# SSH key.
if [ -z "$LOCAL_TEST" ]; then
  ssh-add "$HOME/.ssh/id_ed25519_semmc"
fi

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
ghc --version

ln -sf ./cabal.project.dist ./cabal.project

# Unset GHC_PACKAGE_PATH, in case we're running via 'stack exec',
# because 'cabal new-configure' fails if GHC_PACKAGE_PATH is set, and
unset GHC_PACKAGE_PATH
cabal new-build renovate-x86 || exit 1
cabal new-build renovate-ppc || exit 1
