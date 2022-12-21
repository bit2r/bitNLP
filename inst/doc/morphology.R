## ----environment, echo = FALSE, message = FALSE, warning=FALSE----------------
knitr::opts_chunk$set(collapse = TRUE, comment = "", out.width = "600px", dpi = 70,
                      echo = TRUE, message = FALSE, warning = FALSE)
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ---- echo=FALSE--------------------------------------------------------------
library(dplyr)

class_l <- c(rep("실질형태소", 15), rep("형식형태소", 18), rep("", 11))

class_m <- c(rep("체언", 6), rep("용언", 5), rep("수식언", 3), "독립언",
             rep("관계언", 9), "선어말어미", rep("어말어미", 4), "접두사",
             rep("접미사", 3), "어근", rep("부호", 7), rep("한글 이외", 3))

tag_sejong <- c("NNG", "NNP", "NNB", "NNB", "NR", "NP", "VV", "VA", "VX", "VCP", 
                "VCN", "MM", "MAG", "MAJ", "IC", "JKS", "JKC", "JKG", "JKO", 
                "JKB", "JKV", "JKQ", "JX", "JC", "EP", "EF", "EC", "ETN", "ETM", 
                "XPN", "XSN", "XSV", "XSA", "XR", "SF", "SE", "SS", "SS", "SP", 
                "SO", "SW", "SL", "SH", "SN")
tag_mecab <- c("NNG", "NNP", "NNB", "NNBC", "NR", "NP", "VV", "VA", "VX", "VCP", 
               "VCN", "MM", "MAG", "MAJ", "IC", "JKS", "JKC", "JKG", "JKO", 
               "JKB", "JKV", "JKQ", "JX", "JC", "EP", "EF", "EC", "ETN", "ETM", 
               "XPN", "XSN", "XSV", "XSA", "XR", "SF", "SE", "SSO", "SSC", "SP", 
               "SY", "SY", "SL", "SH", "SN")

desc_sejong <- c("일반 명사", "고유 명사", "의존 명사", "의존 명사", "수사", 
                 "대명사", "동사", "형용사", "보조 용언", "긍정 지정사", 
                 "부정 지정사", "관형사", "일반 부사", "접속 부사", "감탄사", 
                 "주격 조사", "보격 조사", "관형격 조사", "목적격 조사", 
                 "부사격 조사", "호격 조사", "인용격 조사", "보조사", "접속 조사", 
                 "선어말 어미", "종결 어미", "연결 어미", "명사형 전성 어미", 
                 "관형형 전성 어미 ", "체언 접두사", "명사 파생 접미사", 
                 "동사 파생 접미사", "형용사 파생 접미사", "어근", 
                 "마침표, 물음표, 느낌표", "줄임표","따옴표,괄호표,줄표", 
                 "따옴표,괄호표,줄표", "쉼표,가운뎃점,콜론,빗금", 
                 "붙임표(물결,숨김,빠짐)", "기타기호 (논리수학기호,화폐기호)", 
                 "외국어", "한자", "숫자")

desc_mecab <- c("일반 명사", "고유 명사", "의존 명사", "단위를 나타내는 명사", 
                "수사", "대명사", "동사", "형용사", "보조 용언", "긍정 지정사", 
                "부정 지정사", "관형사", "일반 부사", "접속 부사", "감탄사", 
                "주격 조사", "보격 조사", "관형격 조사", "목적격 조사", 
                "부사격 조사", "호격 조사", "인용격 조사", "보조사", "접속 조사", 
                "선어말 어미", "종결 어미", "연결 어미", "명사형 전성 어미", 
                "관형형 전성 어미 ", "체언 접두사", "명사 파생 접미사", 
                "동사 파생 접미사", "형용사 파생 접미사", "어근", 
                "마침표, 물음표, 느낌표", "줄임표","여는 괄호 (, [", 
                "닫는 괄호 ), ]", "쉼표,가운뎃점,콜론,빗금", "", "", 
                "외국어", "한자", "숫자")

data.frame(class_l = class_l, class_m = class_m, tag_sejong = tag_sejong,
           desc_sejong = desc_sejong, tag_mecab = tag_mecab, 
           desc_mecab = desc_mecab) %>% 
  kableExtra::kable(
    col.names = c("실질의미유뮤", "대분류(5언+기타)", "태그", "설명", "태그 ", "설명 ")
  ) %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) %>% 
  kableExtra::add_header_above(
    c(" " = 2, "세종 품사태그" = 2, "mecab-ko 품사태그" = 2))

## -----------------------------------------------------------------------------
library(bitNLP)

args(morpho_mecab)

## -----------------------------------------------------------------------------
morpho_mecab("님은 갔습니다. 아아, 사랑하는 나의 님은 갔습니다.",  type = "morpheme")

## -----------------------------------------------------------------------------
docs <- c("님은 갔습니다. 아아, 사랑하는 나의 님은 갔습니다.",
          "푸른 산빛을 깨치고 단풍나무 숲을 향하여 난 작은 길을 걸어서, 차마 떨치고 갔습니다.")
morpho_mecab(docs,  type = "morpheme")

## -----------------------------------------------------------------------------
morpho_mecab(docs, indiv = FALSE, type = "morpheme")

## -----------------------------------------------------------------------------
morpho_mecab(docs, indiv = FALSE)

## -----------------------------------------------------------------------------
morpho_mecab(docs, indiv = FALSE, type = "noun2")

## -----------------------------------------------------------------------------
morpho_mecab(docs, indiv = FALSE, type = "verb")

## ----word-1, echo=TRUE, eval=FALSE--------------------------------------------
#  library(dplyr)
#  
#  president_speech$doc[1:100] %>%
#    morpho_mecab(indiv = FALSE) %>%
#    table() %>%
#    wordcloud2::wordcloud2(fontFamily = "NanumSquare")

## ---- echo=FALSE, out.width='80%', fig.align='center', fig.pos="!h"-----------
knitr::include_graphics("images/wordcloud1.jpg")

## ----word-2, echo=TRUE, eval=FALSE--------------------------------------------
#  president_speech$doc[1:100] %>%
#    morpho_mecab(indiv = FALSE) %>%
#    table() %>%
#    sort(decreasing = TRUE) %>%
#    .[-c(1:10)] %>%
#    wordcloud2::wordcloud2(fontFamily = "NanumSquare")

## ---- echo=FALSE, out.width='80%', fig.align='center', fig.pos="!h"-----------
knitr::include_graphics("images/wordcloud2.jpg")

## -----------------------------------------------------------------------------
str <- "신혼부부나 주말부부는 놀이공원 자유이용권을 즐겨 구매합니다."
morpho_mecab(str)

## -----------------------------------------------------------------------------
dic_path <- system.file("dic", package = "bitNLP")
dic_file <- glue::glue("{dic_path}/buzz_dic.dic")

morpho_mecab(str, user_dic = dic_file)

