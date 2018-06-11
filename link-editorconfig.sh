#!/usr/bin/env sh
# Link .editorconfig into home directory.

pushd .
cd ${HOME}
ln -s .emacs.d/ext/editorconfig/.editorconfig
popd
