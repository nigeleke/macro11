#!/bin/bash

# This script explicitly runs obj2bin.pl; rather than using its shebang.
# The shebang style does not work in a nix devShell.
#
echo "perl $(pwd)/$(dirname "$0")/obj2bin.pl $*"

