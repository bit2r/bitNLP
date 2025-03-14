% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dictonary.R
\name{update_userdic}
\alias{update_userdic}
\title{update user dictionary with user-defined dictionary files.}
\usage{
update_userdic(userdic_path = "./user_dic", dic_file = "user-dic.dic")
}
\arguments{
\item{userdic_path}{character. 사용자 정의 명사 사전 파일이 존재하는 경로.
지정하지 않으면 "./user_dic"이라는 이름의 경로를 사용함.}

\item{dic_file}{character. 생성할 사용자 사전 파일 이름.
지정하지 않으면 "user-dic.dic"이라는 이름으로 생성함.}
}
\description{
수정된 사용자 정의 사전 파일을 이용하여 사용자 사전 업데이트
}
\details{
사용자 사전정의 디렉토리 내에 있는 "merged.csv" 파일로 사용자 정의 사전을 업데이트/생성한다.
}
\examples{
\dontrun{
update_userdic()
}
}
