#! /bin/bash

set -e
cd "$(dirname "$0")"
exec stack runghc Average.hs
