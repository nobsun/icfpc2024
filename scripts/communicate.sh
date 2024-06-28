#! /bin/sh

if [ -r ./env ]; then
    . ./env
fi

infile="$1"

set -x
curl \
    -H "Authorization: Bearer $AUTH_TOKEN" \
    -X POST \
    --data-binary "@$infile" \
    https://boundvariable.space/communicate
