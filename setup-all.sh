#!/usr/bin/env sh
# Execute each .sh script in the current directory in alphabetical order.

find *.sh | xargs -n 1 sh
