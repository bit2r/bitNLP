#!/bin/bash
set -e

INSTALLD='/usr/local/install_resources'
DIC_DIR='mecab-ko-dic-2.1.1-20180720'

#---------------------------------------------
# 사용자 사전 컴파일
#---------------------------------------------
${INSTALLD}/${DIC_DIR}/tools/add-userdic.sh

#---------------------------------------------
# 사용자 사전 설치
#---------------------------------------------
cd ${INSTALLD}/${DIC_DIR}
make install