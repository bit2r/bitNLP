#' Search for tokenizer plans based on word cost
#' @description 낱말비용 기반의 토크나이저 플랜을 조회한다.
#' @param x character. 플랜을 조회할 단어나 문장.
#' @param topn integer. 플랜을 조회한 후 표시할 상위 저비용 플랜 개수. 기본값은 3임.
#' @param dic_path character. mecab-ko-dic의 시스템 사전이 설치된 경로. 
#' @param userdic character. 사용자 사전. 경로와 이름을 기술함.
#' 지정하지 않으면, bitNLP가 설치한 사전 경로를 사용한다.
#' @details mecab-ko가 주어진 단어나 문장을 토크화(tokenization)하는 플랜을 조회한다.
#' 이 기능을 통해서 사전에서의 단어 비용 조정과 신규 사용자 단어의 추가를 의사결정 할 수 있다.
#' 우선 순위에 따른 10개의 플랜을 조회하며, 실제로 형태소분석기는 우선 순위가 1인 것으로 토큰화한다.
#' 플랜 정보에서 변수는 다음과 같다.:
#' \itemize{
#' \item "우선순위" : 토큰화 우선 순위.
#' \item "표층형" : 토큰화되는 토큰
#' \item "품사태그" : 토큰의 품사.
#' \item "의미부류" : 인명, 혹은 지명과 같은 의미.
#' \item "좌문맥ID" : 좌문맥 ID.
#' \item "우문맥ID" : 우문맥 ID.
#' \item "낱말비용" : 가중치. 값은 낮을수록 가중치가 올라간다.
#' \item "연접비용" : 좌측에 공백 문자를 포함하는 품사의 연접 비용.
#' \item "누적비용" : 누적 낱말비용
#' }
#' @return tbl_df. 플랜을 담은 tibble 객체.
#' @examples
#' \dontrun{
#' get_plan_cost("가면무도회")
#' }
#' @export
#' @import dplyr
#' @importFrom glue glue
#' @importFrom purrr map_int
#' @importFrom tibble as_tibble
get_plan_cost <- function(x, topn = 3, dic_path = NULL, userdic = NULL) {
  if (is_windows()) {
    installd  <- "c:/mecab"
    exec_path <- paste0(installd, "/mecab.exe")
    
    if (is.null(dic_path)) {
      dic_path  <- paste0(installd, "/mecab-ko-dic")
    }
  } else {
    installd <- '/usr/local/lib/mecab/dic' 
    exec_path <- "/usr/local/bin/mecab"    
    
    if (is.null(dic_path)) {
      dic_path  <- paste0(installd, "/mecab-ko-dic")
    }
  }
  
  in_file <- paste0(tempdir(), "/input.txt")
  out_file <- paste0(tempdir(), "/out.txt")
  param <- glue::glue("-F\"%m,%f[0],%f[1],%phl,%phr,%pw,%pC,%pc\n\" -N{topn * 5}")
  
  cat(x, file = in_file)
  
  if (is.null(userdic)) {
    create_cmd <- glue::glue("{exec_path} -d {dic_path} -o {out_file} {in_file} {param}")
  } else {
    create_cmd <- glue::glue("{exec_path} -d {dic_path} -u {userdic} -o {out_file} {in_file} {param}")
  }
  
  
  system(create_cmd)
  
  plan <- read.csv(out_file, header = FALSE, stringsAsFactors = FALSE,
                   col.names = c("표층형", "품사태그", "의미부류", "좌문맥ID",
                                 "우문맥ID", "낱말비용", "연접비용", "누적비용"))
  
  system(glue::glue("rm {in_file}"))  
  system(glue::glue("rm {out_file}"))
  
  idx <- 1L
  
  index <- NROW(plan) %>%
    seq() %>%
    purrr::map_int(
      function(x) {
        idx <<- ifelse(plan$표층형[x] %in% "EOS", idx + 1L, idx)
        idx
      }
    )
  
  dup_plan <- plan %>%
    bind_cols(우선순위 = index) %>%
    filter(!표층형 %in% "EOS") %>%
    select(우선순위, 표층형:누적비용) %>%
    tibble::as_tibble()
  
  index_same <- NULL
  
  max_plan <- max(dup_plan$우선순위)
  
  max_plan %>%
    seq() %>%
    rev() %>% 
    purrr::walk(
      function(x) {
        if (x != 1) {
          curr <- dup_plan %>%
            filter(우선순위 == x) %>% 
            select(-우선순위)
          
          prev <- dup_plan %>%
            filter(우선순위 == (x-1)) %>%
            select(-우선순위)      
          
          if (NROW(curr) == NROW(prev)) {
            is_same <- all(prev == curr, na.rm = TRUE)
            if (is_same) index_same <<- c(index_same, x) 
          }
        }
      }
    )
  
  plan <- dup_plan %>% 
    filter(!우선순위 %in% index_same)
  
  orders <- plan$우선순위 %>% 
    as.character()
  
  incr_index <- seq(unique(plan$우선순위))
  curr_index <- unique(plan$우선순위)
  
  curr_index %>% 
    seq() %>% 
    purrr::walk(
      function(x) {
        orders <<- sub(paste0("^", curr_index[x], "$"), incr_index[x], orders)
      }
    )
  
  plan$우선순위 <- as.integer(orders)
  
  plan %>% 
    filter(우선순위 <= topn)
}
