## ----environment, echo = FALSE, message = FALSE, warning=FALSE----------------
knitr::opts_chunk$set(collapse = TRUE, comment = "", out.width = "600px", dpi = 70,
                      echo = TRUE, message = FALSE, warning = FALSE)
options(tibble.print_min = 4L, tibble.print_max = 4L)

## -----------------------------------------------------------------------------
library(tidyverse)

available.packages(repos = "https://cran.rstudio.com/") %>% 
  row.names() %>% 
  str_subset("tidy")

## -----------------------------------------------------------------------------
library(bitNLP)
library(tidyverse)
library(tidytext)

nho_noun_indiv <- president_speech %>%
  filter(president %in% "노무현") %>%
  filter(str_detect(category, "^외교")) %>%
  tidytext::unnest_tokens(
    out = "speech_noun",
    input = "doc",
    token = morpho_mecab
  )
  
 nho_noun_indiv 

## ---- eval=FALSE--------------------------------------------------------------
#  president_speech %>%
#    filter(president %in% "노무현") %>%
#    filter(str_detect(category, "^외교")) %>%
#    tidytext::unnest_tokens(
#      out = "speech_noun",
#      input = "doc",
#      token = morpho_mecab,
#      user_dic = user_dic
#    )

## -----------------------------------------------------------------------------
tokenize_noun_ngrams(president_speech$doc[1:2])

# simplify = TRUE
tokenize_noun_ngrams(president_speech$doc[1], simplify = TRUE)

str <- "신혼부부나 주말부부는 놀이공원 자유이용권을 즐겨 구매합니다."

tokenize_noun_ngrams(str)

# 불용어 처리
tokenize_noun_ngrams(str, stopwords = "구매")
 
# 사용자 정의 사전 사용
dic_path <- system.file("dic", package = "bitNLP")
dic_file <- glue::glue("{dic_path}/buzz_dic.dic")
tokenize_noun_ngrams(str, simplify = TRUE, user_dic = dic_file)

# n_min
tokenize_noun_ngrams(str, n_min = 1, user_dic = dic_file)

# ngram_delim
tokenize_noun_ngrams(str, ngram_delim = ":", user_dic = dic_file)

# bi-grams
tokenize_noun_ngrams(str, n = 2, ngram_delim = ":", user_dic = dic_file)

## -----------------------------------------------------------------------------
docs <- c("님은 갔습니다. 아아, 사랑하는 나의 님은 갔습니다.",
          "푸른 산빛을 깨치고 단풍나무 숲을 향하여 난 작은 길을 걸어서, 차마 떨치고 갔습니다.")

poem <- tibble(
  연 = rep(1, 2),
  행 = 1:2,
  내용 = docs
)

poem

## -----------------------------------------------------------------------------
poem %>% 
  mutate(명사 = collapse_noun(내용)) %>% 
  select(-내용)

## -----------------------------------------------------------------------------
poem %>% 
  unnest_tokens(
    명사,
    내용,
    token = morpho_mecab
  )

## -----------------------------------------------------------------------------
president_speech %>%
  select(title, doc) %>% 
  filter(row_number() <= 2) %>%
  unnest_noun_ngrams(
    noun_bigram,
    doc,
    n = 2,
    ngram_delim = ":",
    type = "noun2"
  )

## -----------------------------------------------------------------------------
president_speech %>%
  select(title, doc) %>% 
  filter(row_number() <= 2) %>%
  unnest_noun_ngrams(
    noun_bigram,
    doc,
    n = 2,
    ngram_delim = ":",
    drop = FALSE
  )   

## -----------------------------------------------------------------------------
# grouping using group_by() function
president_speech %>%
  filter(row_number() <= 4) %>%
  mutate(speech_year = substr(date, 1, 4)) %>% 
  select(speech_year, title, doc) %>% 
  group_by(speech_year) %>%
  unnest_noun_ngrams(
    noun_bigram,
    doc,
    n = 2,
    ngram_delim = ":"
  )

## -----------------------------------------------------------------------------
# grouping using collapse argument
president_speech %>%
  filter(row_number() <= 4) %>%
  mutate(speech_year = substr(date, 1, 4)) %>% 
  select(speech_year, title, doc) %>% 
  unnest_noun_ngrams(
    noun_bigram,
    doc,
    n = 2,
    ngram_delim = ":",
    collapse = "speech_year"
  )

## -----------------------------------------------------------------------------
args(unnest_noun_ngrams)

