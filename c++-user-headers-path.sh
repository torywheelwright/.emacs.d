#!/usr/bin/env sh
g++ -E -Wp,-v -xc++ /dev/null 2>&1 | sed '/#include "..." search starts here:/,/#include <...> search starts here:/!d' | grep -v "#include" | grep -v "\." | grep -v "("
