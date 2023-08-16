#!/bin/bash

set -e

readonly SOURCE_PATH=$1
readonly FILE_NAME=$2
if [ -z $3 ]; then   
  readonly USERDIC_PATH=./userdic
else
  readonly USERDIC_PATH=$3
fi
readonly SOURCE_FILE=${SOURCE_PATH}/${FILE_NAME}
readonly USERDIC_FILE=${USERDIC_PATH}/${FILE_NAME}

if [ ! -d "$USERDIC_PATH" ]; then
    mkdir ${USERDIC_PATH}
    chmod -R 766 ${USERDIC_PATH}
fi

if [ ! -f "$USERDIC_FILE" ]; then
    cp ${SOURCE_FILE} ${USERDIC_FILE}
    chmod 766 ${USERDIC_FILE}
fi
