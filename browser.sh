#!/bin/sh
cabal configure --ghcjs
cabal build
firefox ./dist/build/qup/qup.jsexe/index.html
cabal configure --ghc
