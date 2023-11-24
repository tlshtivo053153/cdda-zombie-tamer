#!/bin/bash

if [ -d "mods" ]; then
    echo "remove mods/ directory"
    rm -r mods
fi

stack run
zip -r zombie_tamer.zip mods
