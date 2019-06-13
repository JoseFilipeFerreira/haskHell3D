#!/bin/bash
if hash stack 2>/dev/null; then
    stack ghc main.hs || exit 1
else
    ghc main.hs || exit 1
fi
rm *.o *.hi
./main
