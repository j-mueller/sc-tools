#! /bin/bash

fd --extension cabal --exclude 'dist-newstyle/*' --exclude 'dist/*' --exclude '.stack-work/*' --exec bash -c "cabal-fmt --inplace {}"
