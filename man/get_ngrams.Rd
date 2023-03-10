% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tokenizer.R
\name{get_ngrams}
\alias{get_ngrams}
\title{Tokenization with N-gram}
\usage{
get_ngrams(
  x,
  n = 2L,
  token = c("noun", "noun2", "word"),
  type = c("raw", "table"),
  user_dic = NULL
)
}
\arguments{
\item{x}{character. n-gram 토큰화에 사용할 document.}

\item{n}{integer. n-gram 토큰화에서의 n. 기본값은 2.}

\item{token}{character. n-gram 토큰화에서 토큰의 종류. "noun", "noun2", "word"
에서 선택. 기본값은 "noun"로 일반명사, "noun2"는 명사, "word"는 단어를 의미함.}

\item{type}{character. 반환하는 결과물의 종류. "raw"는 토큰화된 n-gram 자체를 반환하며,
"table"은 토큰화된 n-gram 집계 정보를 반환.}

\item{user_dic}{mecab-ko 형태소 분석기의 사용자 정의 사전 파일.
기본값은 NULL로 사용자 사전파일을 지정하지 않음.}
}
\value{
n-gram 토큰화된 character 벡터, 혹은 n-gram 집계 정보를 담은 데이터 프레임
}
\description{
n-gram 토큰화 및 n-gram 토큰화 집계.
}
\section{n-gram 집계 정보}{

n-gram 집계 정보를 담은 데이터 프레임 변수는 다음과 같음.:
\itemize{
\item ngrams : n-gram 토큰. character.
\item freq : n-gram 토큰의 도수. integer.
\item prop : n-gram 토큰의 상대도수. numeric.
}
}

\examples{
\donttest{
str <- "신혼부부나 주말부부는 놀이공원 자유이용권을 즐겨 구매합니다."

# bi-gram
get_ngrams(str)

# tri-gram
get_ngrams(str, n = 3)

# 워드(띄어쓰기) 기반 토큰화
get_ngrams(str, token = "word")

# 집계정보
get_ngrams(str, type = "table")

# 사용자 정의 사전 사용
dic_path <- system.file("dic", package = "bitNLP")
dic_file <- glue::glue("{dic_path}/buzz_dic.dic")
get_ngrams(str, user_dic = dic_file)

}

}
