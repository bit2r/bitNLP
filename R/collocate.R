#' @import dplyr
#' @importFrom rlang arg_match
#' @importFrom stringr str_squish str_remove_all
#' @importFrom tidytext unnest_tokens
#' @importFrom tibble as_tibble
collocation <- function(doc, term, token = c("ngrams", "noun_ngrams"), n = 2,
                        stopwords = character()) {
  token <- rlang::arg_match(token)
  
  doc %>% 
    stringr::str_squish() %>%    
    stringr::str_remove_all("[0-9]") %>% 
    tibble::as_tibble() %>%
    mutate(doc_id = row_number()) %>% 
    tidytext::unnest_tokens(ngrams, value, token = token, n = n, 
                            stopwords = stopwords) %>% 
    group_by(ngrams) %>% 
    tally() %>% 
    rename("frequency" = n) %>% 
    arrange(desc(frequency))
}


#' Calculate table for co-occurrence analysis 
#' @description 공동발생 분석을 위한 공동발생 단어 추출 및 해당 단어의 공동발생 빈도 및 문서에서의 발생 빈도 정보 생성
#' @param x character. 공동발생(co-occurrences) 분석에 사용할 document.
#' @param node character. 공동발생 분석 단어(term)
#' @param span integer. 공동발생 window 단위. 기본값은 3.
#' @param type character. 공동발생에 사용할 단어를 생성하는 방법으로서의 형태소 분석의 결과 유형. 
#' 모든 품사, 명사, 동사 및 형용사와 같은 토큰화 결과 유형을 지정.
#'  "morpheme", "noun", "noun2", "verb", "adj"중에서 선택. 기본값은 "noun"로
#'  일반명사만 추출함.
#' @return data.frame. 공동발생 정보를 담은 data.frame
#' @examples
#' \donttest{
#' docs <- president_speech$doc[1]
#'
#' # default arguments
#' collocate(docs, "우정")
#' 
#' # change span argument
#' collocate(docs, "우정", span = 4)
#' 
#' # change type argument
#' collocate(docs, "우정", type = "morpheme")
#' }
#' @import dplyr
#' @importFrom stringr str_detect
#' @importFrom purrr map
#' @export
collocate <- function(x, node, span = 3, type = c("noun", "noun2", "verb", "adj", "morpheme")) {
  tokens <- morpho_mecab(x, type = type)
  
  idx_match <- stringr::str_detect(tokens, node) %>% 
    which()
  
  if (length(idx_match) == 0) {
    return(NULL)
  }
  
  idx_after <- idx_match %>% 
    purrr::map(
      function(x) x + (1:span)
    ) %>% 
    unlist()
  
  idx_after <- idx_after[idx_after <= length(tokens)]
  
  idx_before <- idx_match %>% 
    purrr::map(
      function(x) x - (1:span)
    ) %>% 
    unlist()
  
  idx_before <- idx_before[idx_before > 0]
  
  df_after <- tokens[idx_after] %>% 
    table() %>% 
    as.data.frame(stringsAsFactors = FALSE)
  
  df_before <- tokens[idx_before] %>% 
    table() %>% 
    as.data.frame(stringsAsFactors = FALSE)
  
  df_total <- tokens %>% 
    table() %>% 
    as.data.frame(stringsAsFactors = FALSE)
  
  names(df_after) <- c("Term", "After")
  names(df_before) <- c("Term", "Before")
  names(df_total) <- c("Term", "Total")
  
  tabs <- df_before %>% 
    full_join(df_after, by = "Term") %>% 
    mutate_at(c("After", "Before"), ~ifelse(is.na(.), 0, .)) %>% 
    mutate(Span = Before + After) %>% 
    left_join(df_total, by = "Term") %>% 
    arrange(Term) 
  
  tabs %>% 
    bind_rows(data.frame(
      Term = node,
      Before = NA,
      After = NA,
      Span = sum(tokens %in% node),
      Total = sum(tokens %in% node)
    )) %>%     
    bind_rows(data.frame(
      Term = "[[TOKENS]]",
      Before = sum(tabs$Before),
      After = sum(tabs$Afte),
      Span = sum(tabs$Span),
      Total = length(tokens)
    ))
}



#' Calculate t-score and mutual information score
#' @description 공동발생 분석을 위한 공동발생 단어에 대한 t-score, MI(mutual information)-score 계산
#' @param x data.frame. "collocate()"를 수행한 공동발생(co-occurrences) 분석결과 
#' @param node character. 공동발생 분석 단어(term)
#' @param span integer. 공동발생 window 단위. 기본값은 3.
#' @return data.frame. 공동발생 정보와 T-score, MI-score를 담은 data.frame
#' @examples
#' \donttest{
#' docs <- president_speech$doc[1]
#'
#' # default arguments
#' collocate(docs, "우정", type = "morpheme")
#' 
#' # change span argument
#' tab_colloc <- collocate(docs, "우정", type = "morpheme") 
#' coll_scores(tab_colloc, "우정", span = 2)
#' 
#' # change span argument
#' tab_colloc <- collocate(docs, "우정", type = "morpheme") 
#' coll_scores(tab_colloc, "국민", span = 2)
#' }
#' @export
coll_scores <- function (x, node, span = 3) 
{
  if (!is.data.frame(x) || (num3 <- which(x$Term == "[[TOKENS]]")) < 1) {
    stop("first argument must be result of collocate()")
  }
  if (nchar(node) < 1) {
    stop("second argument (node) must be a node morpheme")
  }
  if (span == 0) {
    stop("third argument (span) must be specified")
  }
  
  num1 <- which(x$Term == node)
  spanTokens <- span * 2 * x[num1, "Total"]
  
  tscore <- (x[, "Span"] - (x[, "Total"]/x[num3, "Total"] * spanTokens)) / sqrt(x[, "Span"])
  tscore[c(num1, num3)] <- NA
  x$T <- tscore
  
  mutual <- log2(x[, "Span"] / (x[, "Total"] / x[num3, "Total"] * spanTokens))
  mutual[c(num1, num3)] <- NA
  x$MI <- mutual
  
  x
}








