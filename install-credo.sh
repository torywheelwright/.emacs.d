#!/usr/bin/env bash
git clone git@github.com:rrrene/credo.git
cd credo
mix deps.get
mix archive.build
mix archive.install
cd ..
rm -rf credo
