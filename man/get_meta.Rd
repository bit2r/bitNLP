% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nlp.R
\name{get_meta}
\alias{get_meta}
\alias{set_meta}
\title{Meta information processing for text data pre-processing}
\usage{
get_meta(id = c("filter", "replace", "remove", "concat", "split"))

set_meta(
  id = c("filter", "replace", "remove", "concat", "split"),
  filename,
  sep = ",",
  fileEncoding = "utf-8",
  append = FALSE
)
}
\arguments{
\item{id}{character. 메타 정보의 아이디.}

\item{filename}{character. 등록할 메타 정보가 포함된 파일의 이름}

\item{sep}{character. 메타 정보를 기술한 파일의 컬럼 구분자}

\item{fileEncoding}{character. 파일의 인코딩}

\item{append}{logical. 메타 정보의 추가 여부. TRUE이면, 기 등록 메타에 추가한다.}
}
\value{
data.frame 등록된 메타정보를 담은 data.frame
}
\description{
텍스트 데이터의 전처리 과정인 패턴 일치되는 데이터 삭제, 문자열
대체, 불필요 문자열 제거, 문자열 연결 등을 수행하기 위한 메타 정보를 등록하고
조회한다.
}
\examples{
\donttest{
meta_path <- system.file("meta", package = "bitNLP")
fname <- glue::glue("{meta_path}/preparation_filter.csv")

## 데이터 필터링 메타 신규 등록
set_meta("filter", fname, fileEncoding = "utf8")

## 기 등록된 데이터 필터링 메타 조회
get_meta("filter")

## 데이터 필터링 메타 추가 등록
#fname <- "preparation_filter2.csv"
#set_meta("filter", fname, fileEncoding = "utf8", append = TRUE)
}
}
