#! /bin/bash

cabal exec -- cardano-node run \
    --topology ~/sc-tools/node-config/preprod/topology.json \
    --database-path ~/sc-tools/node-config/preprod/db-preprod \
    --socket-path ~/sc-tools/node-config/preprod/socket \
    --config ~/sc-tools/node-config/preprod/config.json \
    +RTS -N4
