## ----environment, echo = FALSE, message = FALSE, warning=FALSE----------------
knitr::opts_chunk$set(collapse = TRUE, comment = "", out.width = "600px", dpi = 70)
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ---- eval=FALSE--------------------------------------------------------------
#  #!/bin/bash
#  set -e
#  
#  # 설치 리소스를 저장할 디렉토리
#  INSTALLD='/cloud/project/install_resources'
#  
#  #---------------------------------------------
#  # 은전한닙 형태소분석기 설치
#  #---------------------------------------------
#  mkdir -p ${INSTALLD}
#  cd ${INSTALLD}
#  wget https://bitbucket.org/eunjeon/mecab-ko/downloads/mecab-0.996-ko-0.9.2.tar.gz
#  tar xzvf mecab-0.996-ko-0.9.2.tar.gz
#  cd ${INSTALLD}/mecab-0.996-ko-0.9.2
#  ./configure --prefix=/cloud/lib
#  make
#  make install
#  ldconfig
#  
#  # 설치파일 삭제
#  rm -rf $INSTALLD/mecab-0.996-ko-0.9.2
#  rm -rf $INSTALLD/mecab-0.996-ko-0.9.2.tar.gz
#  
#  PATH=/cloud/lib/bin:$PATH
#  
#  #---------------------------------------------
#  # 은전한닙 형태소분석기 사전 설치
#  #---------------------------------------------
#  cd ${INSTALLD}
#  wget https://bitbucket.org/eunjeon/mecab-ko-dic/downloads/mecab-ko-dic-2.1.1-20180720.tar.gz
#  tar xvfz mecab-ko-dic-2.1.1-20180720.tar.gz
#  cd ${INSTALLD}/mecab-ko-dic-2.1.1-20180720
#  autoreconf
#  ./configure --prefix=/cloud/lib
#  make
#  make install
#  
#  # 설치파일 삭제
#  rm -rf $INSTALLD/mecab-ko-dic-2.1.1-20180720.tar.gz

## ---- eval=FALSE--------------------------------------------------------------
#  rsession-ld-library-path=/cloud/lib/lib

## ---- eval=FALSE--------------------------------------------------------------
#  # for binary
#  Sys.setenv(PATH=paste("/cloud/lib/bin", Sys.getenv("PATH"), sep = ":"))
#  
#  # for ld library
#  dyn.load("/cloud/lib/lib/libmecab.so.2")
#  
#  install.packages('RcppMeCab')

## ---- eval=FALSE--------------------------------------------------------------
#  remotes::install_github('bit2r/bitNLP')

## ---- eval=FALSE--------------------------------------------------------------
#  library("bitNLP")
#  dyn.load("/cloud/lib/lib/libmecab.so.2")
#  
#  morpho_mecab("아버지가 방에 들어가신다.")

## ---- eval=FALSE--------------------------------------------------------------
#  PATH=/cloud/lib/bin:${PATH}
#  LD_LIBRARY_PATH=/cloud/lib/lib:${LD_LIBRARY_PATH}

## ---- eval=FALSE--------------------------------------------------------------
#  dyn.load("/cloud/lib/lib/libmecab.so.2")

