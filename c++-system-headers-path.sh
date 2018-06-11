#!/usr/bin/env sh
g++ -E -Wp,-v -xc++ /dev/null 2>&1 | sed '/#include <...> search starts here:/,/End of search list./!d' | grep -v "#include" | grep -v "\." | grep -v "("
