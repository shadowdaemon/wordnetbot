#!/bin/sh
ghc --make wordnetbot.hs -o wordnetbot -iWordNet -no-user-package-db -XDeriveDataTypeable -XScopedTypeVariables -XImplicitParams -XRankNTypes -XMultiParamTypeClasses "$@"
