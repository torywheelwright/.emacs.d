#!/usr/bin/env sh
# This script builds and installs eclim from source on OS X. Eclipse must
# already be installed in the default directory. This is untested, but these
# arguments worked for installing from the downloaded jar. Unfortunately, that
# resides at a version-specific URI, so building from source is the best option
# for something that may work long term.

git clone git://github.com/ervandew/eclim.git
pushd .
cd eclim
ant \
-Declipse.home=${HOME}/eclipse/java-neon/Eclipse.app/Contents/Eclipse \
-Declipse.local=${HOME}/.p2/pool \
-Dvim.skip=true
popd
rm -rf eclim
rm ~/eclipse/java-neon/Eclipse.app/Contents/Eclipse/eclimd
rm ~/eclipse/java-neon/Eclipse.app/Contents/Eclipse/eclim
ln -s ${HOME}/.p2/pool/plugins/org.eclim_2.6.0/bin/eclimd ${HOME}/eclipse/java-neon/Eclipse.app/Contents/Eclipse/eclimd
ln -s ${HOME}/.p2/pool/plugins/org.eclim_2.6.0/bin/eclim ${HOME}/eclipse/java-neon/Eclipse.app/Contents/Eclipse/eclim
