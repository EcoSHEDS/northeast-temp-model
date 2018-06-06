#!/bin/bash
# Identify locations in impoundment and tidal zones, export id's to txt file
# output: locations-exclude.txt
# usage: $ ./locations-exclude.sh

set -eu
set -o pipefail

. ./load-config.sh

./locations-impoundment.sh
./locations-tidal.sh

echo Merging impoundment and tidal locations...

sort -u $SHEDS_STM_WD/locations-tidal.txt $SHEDS_STM_WD/locations-impoundment.txt > $SHEDS_STM_WD/locations-exclude.txt
