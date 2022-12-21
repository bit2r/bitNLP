## ----environment, echo = FALSE, message = FALSE, warning=FALSE----------------
knitr::opts_chunk$set(collapse = TRUE, comment = "", out.width = "600px", dpi = 70,
                      echo = TRUE, message = FALSE, warning = FALSE)
options(tibble.print_min = 4L, tibble.print_max = 4L)

## -----------------------------------------------------------------------------
library(bitNLP)

meta_path <- system.file("meta", package = "bitNLP")
fname <- glue::glue("{meta_path}/preparation_filter.csv")

## 데이터 필터링 메타 신규 등록
set_meta("filter", fname, fileEncoding = "utf8")

## -----------------------------------------------------------------------------
## 기 등록된 데이터 필터링 메타 조회
get_meta("filter")

## -----------------------------------------------------------------------------
doc_content <- buzz$CONTENT
is.character(doc_content)
length(doc_content)

sum(is.na(doc_content))

## ---- message=TRUE------------------------------------------------------------
doc_after_character <- filter_text(doc_content, as_logical = FALSE, mc.cores = 8)

length(doc_after_character)

## -----------------------------------------------------------------------------
library(dplyr)

buzz %>% 
  filter(filter_text(CONTENT, verbos = FALSE)) %>% 
  select(KEYWORD, SRC, CONTENT)

## -----------------------------------------------------------------------------
meta_path <- system.file("meta", package = "bitNLP")
fname <- glue::glue("{meta_path}/preparation_replace.csv")
set_meta("replace", fname, fileEncoding = "utf8")

# 등록된 문자열 대체 룰 확인하기
get_meta("replace")

## -----------------------------------------------------------------------------
doc_content <- buzz$CONTENT

stringr::str_detect(doc_content, "남편") %>% 
  sum(na.rm = TRUE)

stringr::str_detect(doc_content, "신랑") %>% 
  sum(na.rm = TRUE)

## ---- message=TRUE------------------------------------------------------------
buzz_after <- buzz %>% 
  mutate(CONTENT = replace_text(CONTENT, verbos = TRUE))

stringr::str_detect(buzz_after$CONTENT, "남편") %>% 
  sum(na.rm = TRUE)

stringr::str_detect(buzz_after$CONTENT, "신랑") %>% 
  sum(na.rm = TRUE)

## -----------------------------------------------------------------------------
meta_path <- system.file("meta", package = "bitNLP")
fname <- glue::glue("{meta_path}/preparation_concat.csv")
set_meta("concat", fname, fileEncoding = "utf8")

# 등록된 문자열 결합 룰 확인하기
get_meta("concat")

## -----------------------------------------------------------------------------
doc_content <- buzz$CONTENT

stringr::str_detect(doc_content, "가사도우미") %>% 
  sum(na.rm = TRUE)

stringr::str_detect(doc_content, "가사[[:space:]]+도우미") %>% 
  sum(na.rm = TRUE)

## ---- message=TRUE------------------------------------------------------------
buzz_after <- buzz %>% 
  mutate(CONTENT = concat_text(CONTENT, verbos = TRUE))

stringr::str_detect(buzz_after$CONTENT, "가사도우미") %>% 
  sum(na.rm = TRUE)

stringr::str_detect(buzz_after$CONTENT, "가사[[:space:]]+도우미") %>% 
  sum(na.rm = TRUE)

## -----------------------------------------------------------------------------
morpho_mecab("가사도우가 집안 청소를 했다.")

## -----------------------------------------------------------------------------
meta_path <- system.file("meta", package = "bitNLP")
fname <- glue::glue("{meta_path}/preparation_split.csv")
set_meta("split", fname, fileEncoding = "utf8")

# 등록된 문자열 분리 룰 확인하기
get_meta("split")

## -----------------------------------------------------------------------------
doc_content <- buzz$CONTENT

stringr::str_extract_all(doc_content, "(하원|등하원|등원|입주|교포|가사|산후|보육|산모)(도우미)") %>% 
  unlist() %>% 
  na.omit() %>% 
  as.vector()

## ---- message=TRUE------------------------------------------------------------
buzz_after <- buzz %>% 
  mutate(CONTENT = split_text(CONTENT, verbos = TRUE))

stringr::str_detect(buzz_after$CONTENT, "(하원|등하원|등원|입주|교포|가사|산후|보육|산모)(도우미)") %>% 
  sum(na.rm = TRUE)

## -----------------------------------------------------------------------------
meta_path <- system.file("meta", package = "bitNLP")
fname <- glue::glue("{meta_path}/preparation_remove.csv")
set_meta("remove", fname, fileEncoding = "utf8")

# 등록된 문자열 제거 룰 확인하기
get_meta("remove")

## ---- tidy.opts = list(blank = FALSE, width.cutoff = 70)----------------------
doc_content <- buzz$CONTENT

stringr::str_detect(doc_content, "게시판[[:space:]]*이용전[[:print:]]*이동됩니다.") %>% 
  which

doc_content[61]

stringr::str_remove(doc_content[61], "게시판[[:space:]]*이용전[[:print:]]*이동됩니다.") 


## ---- message=TRUE------------------------------------------------------------
buzz_after <- buzz %>% 
  mutate(CONTENT = remove_text(CONTENT, verbos = TRUE))

stringr::str_detect(buzz_after$CONTENT, "게시판[[:space:]]*이용전[[:print:]]*이동됩니다.") %>% 
  sum(na.rm = TRUE)

