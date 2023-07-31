#' Search for tokenizer plans based on word cost
#' @description 낱말비용 기반의 토크나이저 플랜을 조회한다.
#' @param x character. 플랜을 조회할 단어나 문장.
#' @param topn integer. 플랜을 조회한 후 표시할 상위 저비용 플랜 개수.
#' 기본값은 3임.
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
get_plan_cost <- function(x, topn = 3) {
  if (is_windows()) {
    installd  <- "c:/mecab"
    exec_path <- paste0(installd, "/mecab.exe")
    dic_path  <- paste0(installd, "/mecab-ko-dic")
  } else {
    installd  <- '/usr/local/install_resources'
    exec_path <- "/usr/local/bin/mecab"    
    dic_path  <- paste0(installd, "/mecab-ko-dic-2.1.1-20180720")
  }
  
  in_file <- paste0(tempdir(), "/input.txt")
  out_file <- paste0(tempdir(), "/out.txt")
  param <- glue::glue("-F\"%m,%f[0],%f[1],%phl,%phr,%pw,%pC,%pc\n\" -N{topn}")
  
  cat(x, file = in_file)
  create_cmd <- glue::glue("{exec_path} -d {dic_path} -o {out_file} {in_file} {param}")
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
  
  plan %>%
    bind_cols(우선순위 = index) %>%
    filter(!표층형 %in% "EOS") %>%
    select(우선순위, 표층형:누적비용) %>%
    tibble::as_tibble() %>%
    print(n = Inf, width = Inf)
}
