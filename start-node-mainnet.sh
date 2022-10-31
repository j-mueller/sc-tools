#! /bin/bash

cabal exec -- cardano-node run \
    --topology ~/sc-tools/node-config/mainnet-topology.json \
    --database-path ~/sc-tools/node-config/db-mainnet \
    --socket-path ~/sc-tools/node-config/socket \
    --config ~/sc-tools/node-config/mainnet-config.json \
    +RTS -N4
