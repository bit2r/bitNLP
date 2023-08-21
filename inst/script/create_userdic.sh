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

if [ ! -d "${USERDIC_PATH}/indexed" ]; then
  mkdir ${USERDIC_PATH}/indexed
fi

readonly DIC_PATH=/usr/local/install_resources/mecab-ko-dic-2.1.1-20180720
readonly MECAB_EXEC_PATH=/usr/local/libexec/mecab
readonly DICT_INDEX=$MECAB_EXEC_PATH/mecab-dict-index


get_userdics() {
  pushd $USERDIC_PATH &> /dev/null
  echo $(ls *.csv)
  popd &> /dev/null
}

#---------------------------------------------
# 사용자 사전 정의 파일 인덱스 생성
#---------------------------------------------
gen_cost() {
  local input_dic=$1
  echo $input_dic
  
  $DICT_INDEX \
  -m ${DIC_PATH}/model.def \
  -d ${DIC_PATH} \
  -u ${USERDIC_PATH}/indexed/nosys-${input_dic} \
  -f utf-8 \
  -t utf-8 \
  -a ${USERDIC_PATH}/$input_dic
}

#---------------------------------------------
# 사용자 사전 컴파일 및 사전 생성
#---------------------------------------------
compile() {
  pushd ${USERDIC_PATH}/indexed &> /dev/null
  cat nosys-*.csv > merged.csv
  popd &> /dev/null
  
  $DICT_INDEX \
  -d ${DIC_PATH} \
  -u ${USERDIC_PATH}/${OUTFILE} \
  -f utf-8 \
  -t utf-8 \
  ${USERDIC_PATH}/indexed/merged.csv
}

main() {
  echo "generating userdic..."
  
  for dic in $(get_userdics); do
  gen_cost $dic
  done
  
  compile
}

main
