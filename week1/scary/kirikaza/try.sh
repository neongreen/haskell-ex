#! /bin/bash

set -e
cd "$(dirname "$0")"
for dic in /usr/share/dict/words /usr/dict/words ; do
    if [ -r "$dic" ] ; then
        exec stack runghc Scary.hs < "$dic"
    fi
done
echo >&2 "couldn't find file with words"
exit 2
