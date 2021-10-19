#!/bin/sh

[ -z "$ONYX_FOLDER" ] && ONYX_FOLDER="$HOME/dev/c/onyx"

[ ! -s "./modules" ]    && ln -s "$ONYX_FOLDER/modules" $(pwd)/modules
[ ! -s "./js" ]         && ln -s "$ONYX_FOLDER/bin"     $(pwd)/js
[ ! -f "./index.html" ] && cp "$ONYX_FOLDER/modules/ouit/index.html" .

onyx -r js -V --use-multi-threading --use-post-mvp-features -o ouit.wasm "$1"

