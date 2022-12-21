## ----environment, echo = FALSE, message = FALSE, warning=FALSE----------------
knitr::opts_chunk$set(collapse = TRUE, comment = "", out.width = "600px", dpi = 70)
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ---- eval=FALSE--------------------------------------------------------------
#  > library(bitNLP)
#  To use bitNLP, you need to install mecab-ko and mecab-ko-dic.
#  You can install it with install_mecab_ko().
#  You have already installed mecab-ko in 'c:/mecab', register the installed path with regist_mecab_ko().

## ---- eval=FALSE--------------------------------------------------------------
#  > library(bitNLP)
#  To use bitNLP, you need to install mecab-ko and mecab-ko-dic.
#  You can install it with install_mecab_ko().
#  

## ---- eval=FALSE--------------------------------------------------------------
#  library(bitNLP)
#  
#  install_mecab_ko()

## ---- eval=FALSE--------------------------------------------------------------
#  > install_mecab_ko()
#  Install mecab-ko-msvc...trying URL 'https://github.com/Pusnow/mecab-ko-msvc/releases/download/release-0.9.2-msvc-3/mecab-ko-msvc-x64.zip'
#  Content type 'application/octet-stream' length 777244 bytes (759 KB)
#  downloaded 759 KB
#  
#  Install mecab-ko-dic-msvc...trying URL 'https://github.com/Pusnow/mecab-ko-dic-msvc/releases/download/mecab-ko-dic-2.0.3-20170922-msvc/mecab-ko-dic-msvc.zip'
#  Content type 'application/octet-stream' length 32531949 bytes (31.0 MB)
#  downloaded 31.0 MB

## ---- eval=FALSE--------------------------------------------------------------
#  regist_mecab_ko()

## ---- eval=FALSE--------------------------------------------------------------
#  > morpho_mecab("아버지가 방에 들어가신다.")
#  Error in morpho_mecab("아버지가 방에 들어가신다.") :
#    To use morpho_mecab(), you need to install RcppMeCab package.
#  You can install it with install.packages("RcppMeCab").

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("RcppMeCab")

## -----------------------------------------------------------------------------
library("bitNLP")

morpho_mecab("아버지가 방에 들어가신다.", type = "morpheme")

