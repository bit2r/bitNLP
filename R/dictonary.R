#' Query the user-defined person dictionary file.
#' @description 사용자 사전 중에서 고유명사 사전의 내용을 조회한다.
#' @param type character. 인명사전과 지명사전에서 조회할 사용자 정의 고유명사 사전 선택.
#' @param userdic_path character. 사용자 정의 고유명사 파일이 존재하는 경로.
#' 지정하지 않으면 사전이 설치된 기본 경로에서 파일을 읽어온다.
#' @details 사용자 사전정의 디렉토리의 사전파일 읽어, 정의된 내용을 tibble 객체로 반환한다. 
#' 이 기능을 통해서 사용자 고유명사 사전의 등록(정의) 여부를 파악할 수 있다.:
#' \itemize{
#' \item 인명사전 : person.csv
#' \item 지명사전 : place.csv
#' \item 고유명사사전 : nnp.csv
#' }
#' 고유명사 사전의 경우에는 타입, 첫번째 품사, 마지막 품사, 원형, 인텍스표현의 정보는 의미가 없어 모두 *로 표현함.
#' @return spec_tbl_df. 고유명사 사전 정의를 담은 tibble 객체.
#' tibble 객체에서 변수는 다음과 같다.:
#' \itemize{
#' \item "표층형" : 단어명.
#' \item "미지정1" : 사용하지 않는 컬럼.
#' \item "미지정2" : 사용하지 않는 컬럼.
#' \item "미지정3" : 사용하지 않는 컬럼.
#' \item "품사태그" : 인명의 품사. NNP를 사용함.
#' \item "의미부류" : 인명, 혹은 지명과 같은 의미 부류.
#' \item "종성유무" : 단어의 마지막 음절의 종성 여부. T, F 입력.
#' \item "읽기" : 읽어서 소리나는 말.
#' \item "타입" : inflected, compound, Preanalysis, *.
#' \item "첫번째 품사" : 기분석으로 나눠지는 토큰에 대한 각 품사 입력.
#' \item "마지막 품사" : 기분석으로 나눠지는 토큰에 대한 각 품사 입력.
#' \item "원형" : 단어가 토큰들로 나눠질 경우의 원형을 +로 묶어 입력
#' \item "인텍스표현" :  단어가 토큰들로 나눠질 경우의 원형을 +로 묶어 입력
#' }
#' @examples
#' \dontrun{
#' get_userdic_nnp("person")
#' }
#' @importFrom glue glue
#' @importFrom readr read_csv
#' @export
get_userdic_nnp <- function(type = c("person", "place", "nnp"), userdic_path = NULL) {
  type <- match.arg(type)
  
  if (is_windows()) {
    installd <- "c:/mecab"
    dic_path <- "user-dic"    
  } else {
    installd <- '/usr/local/install_resources' 
    dic_path <- "mecab-ko-dic-2.1.1-20180720/user-dic"   
  }
  
  if (is.null(userdic_path)) {
    fname <- glue::glue("{installd}/{dic_path}/{type}.csv")
  } else {
    fname <- glue::glue("{userdic_path}/{type}.csv")
  }
  
  meta <- readr::read_csv(fname, col_names = FALSE, show_col_types = FALSE)
  names(meta) <- c("표층형", "미지정1", "미지정2", "미지정3", "품사태그", 
                   "의미부류", "종성유무", "읽기", "타입", "첫번째품사", 
                   "마지막품사", "원형", "인텍스표현")
  meta
}


#' Write to the user-defined person dictionary file.
#' @description 사용자 사전 중 고유명사 사전에 등록하기 위해 인명/지명을 인명/지명/고유명사 사전 파일에 추가
#' @param x character. 고유명사 사전에 등록할 이름들.
#' @param type character. 인명사전과 지명사전, 고유명사 사전에서 등록할 사용자 정의 고유명사 사전 선택.
#' @param userdic_path character. 사용자 정의 고유명사 사전 파일이 존재하는 경로.
#' 지정하지 않으면 사전이 설치된 기본 경로에서 파일을 읽어온다.
#' @details 사용자 사전정의 디렉토리의 person.csv/place.csv/nnp.csv 파일에 등록할 인명/지명/고유명사를 추가한다. 
#' @examples
#' \dontrun{
#' # 인명 사전
#' get_userdic_nnp()
#' append_userdic_nnp(c("변학도"))
#' 
#' # 지명 사전
#' get_userdic_nnp("place")
#' append_userdic_nnp(c("영귀미면"), "place")
#' get_userdic_nnp("place")
#' 
#' # 기타 고유명사 사전  
#' get_userdic_nnp("nnp")
#' append_userdic_nnp(c("릴리움"), "nnp")
#' get_userdic_nnp("nnp")
#' }
#' @import dplyr
#' @importFrom glue glue
#' @importFrom readr write_csv
#' @importFrom purrr map_lgl
#' @importFrom rstudioapi askForPassword
#' @importFrom cli cli_rule cli_alert_success cli_alert_warning
#' @export
append_userdic_nnp <- function(x, type = c("person", "place", "nnp"), userdic_path = NULL) {
  type <- match.arg(type)
  
  type_name <- case_when(
    type %in% "person" ~ "인명",
    type %in% "place"  ~ "지명",
    type %in% "nnp"    ~ "*",    
    TRUE ~               "인명"
  )
  
  if (is_windows()) {
    installd <- "c:/mecab"
    dic_path <- "user-dic" 
    userdic_path <- glue::glue("{installd}/{dic_path}")    
    
    if (is.null(userdic_path)) {
      userdic_path <- glue::glue("{installd}/{dic_path}")
    } 
    
    if (!dir.exists(userdic_path)) {
      dir.create(userdic_path)
    }
    
    fname <- glue::glue("{userdic_path}/{type}.csv")
  } else {
    installd <- '/usr/local/install_resources' 
    dic_path <- "mecab-ko-dic-2.1.1-20180720/user-dic"   

    if (is.null(userdic_path)) {
      userdic_path <- glue::glue("{installd}/{dic_path}")
    } 
    
    if (!dir.exists(userdic_path)) {
      dir.create(userdic_path)
    }
    
    fname <- glue::glue("{userdic_path}/{type}.csv")

    if (file.access(fname, 2) == -1) {
      if (.Platform$GUI %in% "RStudio") {
        input <- rstudioapi::askForPassword("sudo password")
      } else {
        input <- readline("Enter your password: ")
      }
      
      system(glue::glue("sudo -kS chmod 766 {fname}"), input = input)
    } 
  }
  
  meta <- get_userdic_nnp(type, userdic_path)
  nnps <- setdiff(x, meta$표층형)
  
  type_name2 <- ifelse(type_name %in% "*", "고유명사", type_name)
  
  if (length(nnps) == 0) {
    cli::cli_alert_warning(glue::glue("등록할 {type_name2}(이/가) 없거나 이미 모두 등록되어 있을 수도 있습니다."))
    return(NULL)
  }
  
  final_consonants <- nnps %>% 
    purrr::map_lgl(
      has_final_consonant, last = TRUE
    )
  
  df_nnps <- meta %>% 
    bind_rows(
      data.frame(
        표층형 = nnps,
        미지정1 = NA,
        미지정2 = NA,
        미지정3 = NA,
        품사태그 = "NNP",
        의미부류 = type_name,
        종성유무 =  final_consonants,
        읽기 = nnps,
        타입 = "*",  
        첫번째품사 = "*",
        마지막품사 = "*", 
        원형 = "*", 
        인텍스표현 = "*"
      )
    )
  
  df_nnps$종성유무 <- substr(as.character(df_nnps$종성유무), 1, 1)
  readr::write_csv(df_nnps, file = fname, na = "", col_names = FALSE)  
  
  n_add_nnps <- length(nnps)  
  
  cli::cli_rule(glue::glue("사전 파일에 {type_name2} 추가하기"))
  cli::cli_alert_success(c("신규 추가 건수: {n_add_nnps}"))
  cli::cli_alert_success(c("최종 {type_name2} 건수: {NROW(df_nnps)}"))  
}


#' Add user-defined dictionary files to user dictionary.
#' @description 사용자가 정의한 사용자 정의 사전 파일을 사용자 사전에 추가
#' @details 사용자 사전정의 디렉토리에 있는 모든 사용자정의 사전 파일을 사용자 사전에 추가한다.
#' @examples
#' \dontrun{
#' add_userdic()
#' }
#' @importFrom glue glue
#' @importFrom rstudioapi askForPassword
#' @export
add_userdic <- function() {
  if (is_windows()) {
    installd <- "c:/mecab"
    dic_path <- "mecab-ko-dic" 
    
    cmd <- glue::glue('powershell -Command "Start-Process powershell \\"-ExecutionPolicy Bypass -NoProfile -NoExit -Command `\\"cd \\`\\"{installd}\\`\\"; & \\`\\"./tools/add-userdic-win.ps1\\`\\"`\\"\\" -Verb RunAs"')
    system(glue::glue("{cmd}"))
  } else {
    script_path <- system.file("script", package = "bitNLP")
    add_script <- glue::glue("/bin/sh {script_path}/add_userdic.sh")
    
    if (.Platform$GUI %in% "RStudio") {
      input <- rstudioapi::askForPassword("sudo password")
    } else {
      input <- readline("Enter your password: ")
    }
    
    system(glue::glue("sudo -kS {add_script}"), input = input)
  }
}

