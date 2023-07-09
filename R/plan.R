#' Search for tokenizer plans based on word cost
#' @description 낱말비용 기반의 토크나이저 플랜을 조회한다.
#' @param x character. 플랜을 조회한 단어나 문장.
#' @details mecab-ko가 주어진 단어나 문장을 토크화(tokenization)하는 플랜을 조회한다. 
#' 이 기능을 통해서 사전에서의 단어 비용 조정과 신규 사용자 단어의 추가를 의사결정 할 수 있다.
#' 우선 순위에 따른 10개의 플랜을 조회하며, 실제로 형태소분석기는 우선 순위가 1인 것으로 토큰화한다.
#' 현재 이 함수는 Windows 운영체제를 지원하지 않는다.
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
#' @importFrom stringr str_replace
#' @importFrom tidyr separate_wider_delim
#' @importFrom tidyselect ends_with
get_plan_cost <- function(x) {
  if (is_windows()) {
    installd <- "c:/mecab"
    tool_path <- "mecab-ko-dic/tools"    
    stop("현재 이 기능은 Windows 운영체제를 지원하지 않습니다.")
  } else {
    installd <- '/usr/local/install_resources' 
    tool_path <- "mecab-ko-dic-2.1.1-20180720/tools"   
    cmd <- "mecab-bestn.sh"
  }

  plan <- system(glue::glue("echo '{x}' | {installd}/{tool_path}/{cmd}"), 
                 intern = TRUE)
  
  plan <- data.frame(plan = plan, 
                     stringsAsFactors = FALSE) %>% 
    filter(row_number() >= 4) 
  
  idx <- 1
  
  index <- NROW(plan) %>% 
    seq() %>% 
    purrr::map_int(
      function(x) {
        idx <<- ifelse(plan$plan[x] %in% "EOS", idx + 1, idx) 
        idx
      }
    )
  
  plan %>% 
    bind_cols(우선순위 = index) %>% 
    filter(!plan %in% "EOS") %>% 
    mutate(plan = stringr::str_replace(plan, "\\t", ",")) %>% 
    tidyr::separate_wider_delim(
      plan, ",", 
      names = c("표층형", "품사태그", "의미부류", "좌문맥ID", "우문맥ID", 
                "낱말비용", "연접비용", "누적비용")) %>% 
    mutate_at(vars(tidyselect::ends_with(c("비용", "ID"))), as.integer) %>% 
    select(우선순위, 표층형:누적비용) %>% 
    print(n = Inf, width = Inf)
}
