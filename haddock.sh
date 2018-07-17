#!/bin/sh
set -e

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal haddock --builddir="$dir" --for-hackage --haddock-option=--hyperlinked-source
# Starting with cabal 2.0, `--publish` is needed for uploading to non-candidate releases
cabal upload --publish -d $dir/*-docs.tar.gz
