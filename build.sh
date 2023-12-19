#!/bin/bash

if [ -d "mods" ]; then
    echo "remove mods/ directory"
    rm -r mods
fi

if [ -e "zombie_tamer.zip" ]; then
    echo "remove zombie_tamer.zip"
    rm zombie_tamer.zip
fi

stack run
find ./mods/zombie_tamer -name '*.json' | xargs -I {} ./format_json.sh {}
zip -r zombie_tamer.zip mods
