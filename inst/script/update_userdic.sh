#!/bin/bash
set -e

readonly PROG_NAME=$(basename $0)

if [ -z $1 ]; then   
readonly USERDIC_PATH=./user-dic
else
  readonly USERDIC_PATH=$1
fi

if [ -z $2 ]; then   
readonly OUTFILE=user-dic.dic
else
  readonly OUTFILE=$2
fi

readonly DIC_PATH=/usr/local/install_resources/mecab-ko-dic-2.1.1-20180720
readonly MECAB_EXEC_PATH=/usr/local/libexec/mecab
readonly DICT_INDEX=$MECAB_EXEC_PATH/mecab-dict-index

#---------------------------------------------
# 사용자 사전 컴파일 및 사전 생성
#---------------------------------------------
compile() {
  $DICT_INDEX \
  -d ${DIC_PATH} \
  -u ${USERDIC_PATH}/${OUTFILE} \
  -f utf-8 \
  -t utf-8 \
  ${USERDIC_PATH}/indexed/merged.csv
}

main() {
  echo "updating userdic..."
  
  compile
}

main
