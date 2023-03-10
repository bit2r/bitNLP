% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sentiment_dic.R
\name{get_polarity}
\alias{get_polarity}
\title{KNU Korean Sentiment Dictionary Sentiment Analysis}
\usage{
get_polarity(doc, n = 1, indiv = TRUE)
}
\arguments{
\item{doc}{character. 군산대학교 한국어 감성 사전을 이용해서 감성분석을 수행할 문자열 벡터}

\item{n}{integer. n-gram 토큰화 계수}

\item{indiv}{logical. 복수개의 문서일 때 개별 문서의 결과를 반환할 지를 선택함.
TRUE이면 데이터프레임에서 개별 문서의 결과를 관측치(observations)로 반환하고, FALSE이면 하나의 관측치로 반환함.
기본값은 TRUE}
}
\value{
data.frame 감성분석 결과를 담은 data.frame
\itemize{
  \item n_match: numeric. 감성사전에 매치된 토큰 개수
  \item n_negative: numeric. 감성사전의 부정 단어와 매치된 토큰 개수
  \item n_positive: numeric. 감성사전의 긍정 단어와 매치된 토큰 개수
  \item n_neutral: numeric. 감성사전의 중립 단어와 매치된 토큰 개수
  \item negative: numeric. 감성사전의 부정 단어와 매치된 토큰의 점수의 합
  \item positive: character. 감성사전의 긍정 단어와 매치된 토큰의 점수의 합
  \item polarity: numeric. 감성의 극성. (positive - negative) / (positive + negative).
}
}
\description{
군산대학교 한국어 감성 사전을 활용하여 문서의 감성분석 결과를 반환
}
\examples{
\donttest{
get_polarity(buzz$CONTENT[1])

# 개별 문서들의 감성분석
get_polarity(buzz$CONTENT[1:5])

# 전체 문서를 통합한 감성분석
get_polarity(buzz$CONTENT[1:5], indiv = FALSE)
}
}
