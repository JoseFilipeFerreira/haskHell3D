#!/bin/bash
if hash stack 2>/dev/null; then
    stack ghc main.hs
else
    ghc main.hs
fi
rm *.o *.hi
./main
