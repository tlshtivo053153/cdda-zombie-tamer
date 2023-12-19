#!/bin/bash

FILENAME="$1"
cat "$FILENAME" | json_formatter.cgi | sponge "$FILENAME"
