#!/bin/bash
# Load and merge configuration variables
# usage: . ./load-config.sh

. ../config.sh
. ../version.sh

SHEDS_STM_WD=${SHEDS_STM_ROOT}/${SHEDS_STM_VERSION}
SHEDS_STM_LOG=${SHEDS_STM_WD}/temp-model.log
