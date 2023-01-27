#! /bin/bash

cabal exec -- cardano-node run \
    --topology ~/sc-tools/node-config/mainnet/mainnet-topology.json \
    --database-path ~/sc-tools/node-config/mainnet/db-mainnet \
    --socket-path ~/sc-tools/node-config/mainnet/socket \
    --config ~/sc-tools/node-config/mainnet/mainnet-config.json \
    +RTS -N4
