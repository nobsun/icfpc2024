#! /bin/sh

if [ -r ./env ]; then
    . ./env
fi

errout="$1"
if [ x"$errout" = x ]; then
    errout=/dev/null
fi

set -x
curl \
    -H "Authorization: Bearer $AUTH_TOKEN" \
    -X POST \
    --data-binary "@-" \
    https://boundvariable.space/communicate \
    2> $errout
