#!/usr/bin/env bash

fd --extension hs --exclude 'dist-newstyle/*' --exclude 'dist/*' --exclude '.stack-work/*' --exec bash -c "stylish-haskell -i {} || true"
