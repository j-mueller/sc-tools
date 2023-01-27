#! /bin/bash

cabal exec -- cardano-node run \
    --topology /home/jann/sc-tools/node-config/preprod/topology.json \
    --database-path /home/jann/sc-tools/node-config/preprod/db-preprod \
    --socket-path /home/jann/sc-tools/node-config/preprod/socket \
    --config /home/jann/sc-tools/node-config/preprod/config.json \
    +RTS -N4
