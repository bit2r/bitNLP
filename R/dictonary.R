#' Query the user-defined person dictionary file.
#' @description 사용자 사전 중에서 명사 사전의 내용을 조회한다.
#' @param noun_type character. 인명사전, 지명사전, 고유명사 사전, 일반명사 사전에서 
#' 조회할 사용자 정의 명사 사전 선택. 기본값은 "person"로 인명사전을 지정함.
#' @param userdic_path character. 사용자 정의 명사 사전 파일이 존재하는 경로.
#' 지정하지 않으면 사전이 설치된 기본 경로에서 파일을 읽어온다.
#' @details 사용자 사전정의 디렉토리의 사전파일 읽어, 정의된 내용을 tibble 객체로 반환한다. 
#' 이 기능을 통해서 사용자 명사 사전의 등록(정의) 여부를 파악할 수 있다.
#' 다음과 같은 명사 사용자 정의 사전 파일을 참조한다.
#' \itemize{
#' \item 인명사전 : person.csv
#' \item 지명사전 : place.csv
#' \item 고유명사사전 : nnp.csv
#' \item 일반명사사전 : nng.csv
#' }
#' 인명, 지명, 고유명사, 일반명사 사전의 경우에는 타입, 첫번째 품사, 마지막 품사,
#' 인덱스표현의 정보는 의미가 없어 모두 *로 표현함.
#' @return spec_tbl_df. 명사 사전 정의를 담은 tibble 객체.
#' tibble 객체에서 변수는 다음과 같다.:
#' \itemize{
#' \item "표층형" : 낱말명.
#' \item "미지정1" : 사용하지 않는 컬럼.
#' \item "미지정2" : 사용하지 않는 컬럼.
#' \item "미지정3" : 사용하지 않는 컬럼.
#' \item "품사태그" : 인명의 품사. NNP를 사용함.
#' \item "의미부류" : 인명, 혹은 지명과 같은 의미 부류.
#' \item "종성유무" : 낱말의 마지막 음절의 종성 여부. T, F 입력.
#' \item "읽기" : 읽어서 소리나는 말.
#' \item "타입" : inflected, compound, Preanalysis, *.
#' \item "첫번째 품사" : 기분석으로 나눠지는 토큰에 대한 각 품사 입력.
#' \item "마지막 품사" : 기분석으로 나눠지는 토큰에 대한 각 품사 입력.
#' \item "표현" : 낱말이 토큰들로 나눠질 경우의 원형을 +로 묶어 입력
#' \item "인덱스표현" : 사용하지 않는 컬럼, *로 표현
#' }
#' @references {
#' mecab-ko-dic 품사 태그 설명. 
#' <https://docs.google.com/spreadsheets/d/1-9blXKjtjeKZqsf4NzHeYJCrr49-nXeRF6D80udfcwY/edit#gid=1718487366>
#' }
#' @examples
#' \dontrun{
#' get_userdic_meta("person")
#' }
#' @importFrom glue glue
#' @importFrom readr read_csv
#' @importFrom cli cli_alert_warning
#' @export
get_userdic_meta <- function(noun_type = c("person", "place", "nnp", "nng"), 
                             userdic_path = NULL) {
  noun_type <- match.arg(noun_type)
  
  if (is_windows()) {
    installd <- "c:/mecab"
    dic_path <- "user-dic"    
  } else {
    installd <- '/usr/local/install_resources' 
    dic_path <- "mecab-ko-dic-2.1.1-20180720/user-dic"   
  }
  
  if (is.null(userdic_path)) {
    fname <- glue::glue("{installd}/{dic_path}/{noun_type}.csv")
  } else {
    fname <- glue::glue("{userdic_path}/{noun_type}.csv")
  }
  
  if (!file.exists(fname)) {
    cli::cli_alert_warning(glue::glue("사용자 사전 정의 파일 {fname}가 존재하지 않습니다."))
    return(NULL)
  }
  
  meta <- readr::read_csv(fname, col_names = FALSE, show_col_types = FALSE)
  names(meta) <- c("표층형", "미지정1", "미지정2", "미지정3", "품사태그", 
                   "의미부류", "종성유무", "읽기", "타입", "첫번째품사", 
                   "마지막품사", "표현", "인덱스표현")
  meta
}


#' Write to the user-defined noun dictionary file.
#' @description 사용자 명사 사전에 등록하기 위해 인명/지명을 인명/지명/고유명사/일반명사 사전 파일에 추가
#' @param term character. mecab-ko 사전에 등록할 이름들.
#' mecab-ko-dic 품사 태그 설명에서 '표층형', '읽기'에 적용됨
#' @param type character. mecab-ko 사전에 등록할 타입들.
#' mecab-ko-dic 품사 태그 설명에서 '타입'에 적용됨. 
#' @param prototype character. mecab-ko 사전에 등록할 원형들.
#' mecab-ko-dic 품사 태그 설명에서 '표현'에 적용됨. 
#' @param noun_type character. 인명사전과 지명사전, 고유명사, 일반명사 사전에서 
#' 등록할 사용자 정의 명사 사전 선택.
#' @param dic_type character. 생성할 사용자 정의 사전을 시스템사전에 빌드할 지, 
#' 사용자 사전으로 빌드할 지의 선택. 기본값은 "sysdic"으로 시스템사전에 빌드할 목적으로 작업함.
#' @param userdic_path character. 사용자 정의 명사 사전 파일이 존재하는 경로.
#' 지정하지 않으면 사전이 설치된 기본 경로에서 파일을 읽어온다.
#' @details 사용자 사전정의 디렉토리의 person.csv/place.csv/nnp.csv/nng.csv 파일에 
#' 등록할 인명/지명/고유명사/일반명사를 추가한다. 
#' mecab-ko-dic 품사 태그 설명에서 '타입'은 두 개 이상의 토큰으로 구성된 복합명사일 때만 사용하며, 
#' 'Compound', 'Preanalysis', 'Inflected' 중에 하나를 기술하는데 의미는 다음과 같음.:
#' \itemize{
#' \item Compound : 가장 흔한 사례의 복합명사로 개별 토큰의 의미가 합쳐져서도 의미가 유지되는 사례
#'   \itemize{
#'     \item 예) 주말부부: 주말/NNG + 부부/NNG
#'   }
#' \item Preanalysis : 개별 토큰의 의미가 합쳐지면서 의미가 상실되는 사례
#'   \itemize{
#'     \item 예) 인터파크: 인터/NNG + 파크/NNG
#'   }
#' \item Inflected : 토큰이 합쳐질 때, 개별 토큰에 변형이 일어나는 경우로 복합명사에서는 거의 발생하지 않음
#' }
#' @references {
#' mecab-ko-dic 품사 태그 설명. 
#' <https://docs.google.com/spreadsheets/d/1-9blXKjtjeKZqsf4NzHeYJCrr49-nXeRF6D80udfcwY/edit#gid=1718487366>
#' }
#' @examples
#' \dontrun{
#' # 인명 사전
#' get_userdic_meta()
#' append_userdic_meta(c("변학도"))
#' 
#' # 지명 사전
#' get_userdic_meta("place")
#' append_userdic_meta(c("영귀미면"), noun_type = "place")
#' get_userdic_meta("place")
#' 
#' # 고유명사 사전  
#' get_userdic_meta("nnp")
#' append_userdic_meta(c("릴리움", "인터파크"), c("*", "Preanalysis"), 
#'                     c("*", "인터/NNG/*+파크/NNG/*"), noun_type = "nnp")
#' get_userdic_meta("nnp")
#' 
#' # 일반명사 사전을 사용자 사전에 빌드할 목적으로 등록함  
#' get_userdic_meta("nng", dic_type = "userdic")
#' append_userdic_meta(c("주말부부", "쿼토"), c("Compound", "*"), 
#'                     c("주말/NNG/*+부부/NNG/*", "*"), 
#'                     noun_type = "nng",
#'                     dic_type = "userdic")
#' get_userdic_meta("nng")
#' }
#' @import dplyr
#' @importFrom glue glue
#' @importFrom readr write_csv
#' @importFrom purrr map_lgl map_df
#' @importFrom rstudioapi askForPassword
#' @importFrom cli cli_rule cli_alert_success cli_alert_warning
#' @importFrom stringr str_extract
#' @export
append_userdic_meta <- function(term, type = NULL, prototype = NULL, 
                                noun_type = c("person", "place", "nnp", "nng"), 
                                dic_type = c("sysdic", "userdic"),
                                userdic_path = NULL) {
  noun_type <- match.arg(noun_type)
  dic_type  <- match.arg(dic_type)
  
  type_name <- case_when(
    noun_type %in% "person" ~ "인명",
    noun_type %in% "place"  ~ "지명",
    noun_type %in% "nnp"    ~ "*", 
    noun_type %in% "nng"    ~ "*", 
    TRUE                    ~ "인명"
  )
  
  type_name2 <- case_when(
    noun_type %in% "nnp" ~ "고유명사",
    noun_type %in% "nng" ~ "일반명사",
    TRUE                 ~ type_name
  )  
  
  tags <- case_when(
    noun_type %in% "nng" ~ "NNG",
    TRUE                 ~ "NNP"
  )    
  
  idx_preanal <- which(type %in% "Preanalysis")
  
  if (length(idx_preanal) == 0) {
    first_tag <- "*"
    last_tag <- "*"
  } else {
    tab <- length(prototype) |> 
      seq() |> 
      purrr::map_df(
        function(x) {
          if (x %in% idx_preanal) {
            first <- stringr::str_extract(prototype[x], "[A-Z]+")
            last <- stringr::str_extract(prototype[x], "[A-Z]+\\/\\*") |> 
              stringr::str_extract(., "[A-Z]+")
          } else {
            first <- "*"
            last <- "*"
          }
          
          data.frame(first = first, last = last)
        }
      )
    
    first_tag <- tab$first
    last_tag <- tab$last
  }
  
  if (is_windows()) {
    installd <- "c:/mecab"
    dic_path <- "user-dic" 
    
    if (is.null(userdic_path)) {
      if (dic_type %in% "sysdic") {
        userdic_path <- glue::glue("{installd}/{dic_path}") 
      } else {
        userdic_path <- "./user_dic"
      }
    } 
    
    if (!dir.exists(userdic_path)) {
      dir.create(userdic_path)
    }
    
    fname <- glue::glue("{userdic_path}/{noun_type}.csv")
    
    if (!file.exists(fname)) {
      dic_file <- file.path(system.file(package = "bitNLP"), "dic", 
                            glue::glue("{noun_type}.csv"))
      
      file.copy(from = dic_file, to = fname)
    }
    
  } else {
    if (is.null(userdic_path)) {
      if (dic_type %in% "sysdic") {
        installd <- '/usr/local/install_resources' 
        dic_path <- "mecab-ko-dic-2.1.1-20180720/user-dic"   
        
        userdic_path <- glue::glue("{installd}/{dic_path}") 
      } else {
        userdic_path <- "./user_dic"
        
        if (!dir.exists(userdic_path)) {
          dir.create(userdic_path)
        }
      }
    } 
    
    fname <- glue::glue("{userdic_path}/{noun_type}.csv")
    
    if (!file.exists(fname) | !dir.exists(userdic_path)) {
      cmd <- file.path(system.file(package = "bitNLP"), "script", 
                       "append_userdic.sh")    
      source_path <- file.path(system.file(package = "bitNLP"), "dic")
      file_nm <- glue::glue("{noun_type}.csv")
      
      if (file.access(userdic_path, 2) == -1) {
        if (.Platform$GUI %in% "RStudio") {
          input <- rstudioapi::askForPassword("sudo password")
        } else {
          input <- readline("Enter your password: ")
        }
        
        system(glue::glue("sudo -kS /bin/bash {cmd} {source_path} {noun_type}.csv {userdic_path}"), 
               input = input)
      } else {
        system(glue::glue("/bin/bash {cmd} {source_path} {noun_type}.csv {userdic_path}"))
      }  
    }
  }
  
  meta <- get_userdic_meta(noun_type, userdic_path)
  
  n_dup <- length(intersect(term, meta$표층형))
  meta <- meta |> 
    filter(!`표층형` %in% term)
  
  if (n_dup > 0) {
    cli::cli_alert_warning(
      glue::glue("기 정의된 {n_dup}건의 낱말이 업데이트 되었습니다."))
  }
  
  final_consonants <- term |> 
    purrr::map_lgl(
      has_final_consonant, last = TRUE
    )
  
  if (is.null(type)) {
    type <- "*"
  }
  
  if (is.null(prototype)) {
    prototype <- "*"
  }
  
  df_nouns <- meta |> 
    bind_rows(
      data.frame(
        표층형 = term,
        미지정1 = NA,
        미지정2 = NA,
        미지정3 = NA,
        품사태그 = tags,
        의미부류 = type_name,
        종성유무 =  final_consonants,
        읽기 = term,
        타입 = type,  
        첫번째품사 = first_tag,
        마지막품사 = last_tag, 
        표현 = prototype, 
        인덱스표현 = "*"
      )
    )
  
  df_nouns$종성유무 <- substr(as.character(df_nouns$종성유무), 1, 1)
  readr::write_csv(df_nouns, file = fname, na = "", col_names = FALSE)  
  
  n_add_nouns <- length(term)  
  
  cli::cli_rule(glue::glue("사전 파일에 {type_name2} 추가하기"))
  cli::cli_alert_success(c("신규 추가 건수: {n_add_nouns}"))
  cli::cli_alert_success(c("최종 {type_name2} 건수: {NROW(df_nouns)}"))  
}


#' Add user-defined dictionary files to system dictionary.
#' @description 사용자가 정의한 사용자 정의 사전 파일을 시스템 사전에 추가
#' @details 사용자 사전정의 디렉토리에 있는 모든 사용자정의 사전 파일을 시스템 사전에 추가한다.
#' @examples
#' \dontrun{
#' add_sysdic()
#' }
#' @importFrom glue glue
#' @importFrom rstudioapi askForPassword
#' @export
add_sysdic <- function() {
  if (is_windows()) {
    installd <- "c:/mecab"
    dic_path <- "mecab-ko-dic" 
    
    cmd <- glue::glue('powershell -Command "Start-Process powershell \\"-ExecutionPolicy Bypass -NoProfile -NoExit -Command `\\"cd \\`\\"{installd}\\`\\"; & \\`\\"./tools/add-userdic-win.ps1\\`\\"`\\"\\" -Verb RunAs"')
    system(glue::glue("{cmd}"))
  } else {
    script_path <- system.file("script", package = "bitNLP")
    add_script <- glue::glue("/bin/bash {script_path}/add_sysdic.sh")
    
    if (.Platform$GUI %in% "RStudio") {
      input <- rstudioapi::askForPassword("sudo password")
    } else {
      input <- readline("Enter your password: ")
    }
    
    system(glue::glue("sudo -kS {add_script}"), input = input)
  }
}


#' create user dictionary with user-defined dictionary files.
#' @description 사용자가 정의한 사용자 정의 사전 파일을 사용자 사전으로 생성
#' @details 사용자 사전정의 디렉토리에 있는 모든 사용자정의 사전 파일을 엮어 사용자 사전에 추가한다.
#' @param userdic_path character. 사용자 정의 명사 사전 파일이 존재하는 경로.
#' 지정하지 않으면 "./user_dic"이라는 이름의 경로를 사용함.
#' @param dic_file character. 생성할 사용자 사전 파일 이름.
#' 지정하지 않으면 "user-dic.dic"이라는 이름으로 생성함.
#' @examples
#' \dontrun{
#' create_userdic()
#' }
#' @importFrom glue glue
#' @importFrom rstudioapi askForPassword
#' @export
create_userdic <- function(userdic_path = "./user_dic", dic_file = "user-dic.dic") {
  if (is_windows()) {
    script_path <- system.file("script", package = "bitNLP")
    script <- glue::glue("{script_path}\\create_userdic_win.ps1")
    
    cmd <- glue::glue('powershell -File "{script}" {userdic_path} {dic_file}')
    system(glue::glue("{cmd}"))
  } else {
    script_path <- system.file("script", package = "bitNLP")
    create_script <- glue::glue("/bin/bash {script_path}/create_userdic.sh")
    
    cmd <- glue::glue("{create_script} {userdic_path} {dic_file}")
    
    exec_file <- "/usr/local/libexec/mecab/mecab-dict-index"
    if (file.access(exec_file, 1) == -1) {
      if (.Platform$GUI %in% "RStudio") {
        input <- rstudioapi::askForPassword("sudo password")
      } else {
        input <- readline("Enter your password: ")
      }
      
      system("sudo -kS chmod -R 755 /usr/local/libexec/mecab", input = input)
    }
    
    if (file.access(userdic_path, 2) == -1) {
      if (!exists("input")) {
        if (.Platform$GUI %in% "RStudio") {
          input <- rstudioapi::askForPassword("sudo password")
        } else {
          input <- readline("Enter your password: ")
        }
      }
      
      system(glue::glue("sudo -kS {cmd}"), input = input)
    } else {
      system(cmd)
    }  
  }
}


#' Modify the word cost of a word in a dictionary definition file.
#' @description 사용자 정의 사전 파일에서 낱말의 낱말비용을 수정
#' @details 사용자 사전정의 디렉토리에 있는 병합 사용자 사전 정의 파일의 낱말비용을 수정함.
#' 대상 파일은 "사용자 정의 사전 파일 경로/indexed/merged.csv" 파일임. 
#' 그리고 낱말비용이 수정되면 수정된 낱말의 정보는 "사용자 정의 사전 파일 경로/indexed/changed.csv" 
#' 파일에 수정 이력이 저장됨. 이 정보는 사용자 사전 정의 파일을 재정의할 때 
#' 수정된 낱말비용 정보가 없어지는 매커니즘을 보완하기 위한 이력 파일임.
#' 낱말비용이 수정되었다는 것 만으로 그 정보가 사전에 반영되는 것은 아님.
#' @param userdic_path character. 사용자 정의 사전 파일이 존재하는 경로.
#' 지정하지 않으면 "./user_dic"이라는 이름의 경로를 사용함.
#' @param dic_file character. 생성할 사용자 사전 파일 이름.
#' 지정하지 않으면 "user-dic.dic"이라는 이름으로 생성함.
#' @examples
#' \dontrun{
#' edit_termcost()
#' }
#' @import dplyr
#' @importFrom glue glue
#' @importFrom readr read_csv write_csv
#' @importFrom cli cli_rule cli_alert_success cli_alert_warning
#' @importFrom DataEditR data_edit
#' @export
edit_termcost <- function(userdic_path = "./user_dic", dic_file = "user-dic.dic") {
  logo <- "https://r2bit.com/bitNLP/reference/figures/bitNLP_logo.png"
  title <- "사용자 사전 낱말비용 편집기"
  
  read_indexfile <- function(fname) {
    meta <- readr::read_csv(fname, col_names = FALSE, show_col_types = FALSE)
    
    names(meta) <- c("표층형", "좌문맥ID", "우문맥ID", "낱말비용", "품사태그", 
                     "의미부류", "종성유무", "읽기", "타입", "첫번째품사", 
                     "마지막품사", "표현", "인덱스표현")
    
    meta
  }
  
  fname <- glue::glue("{userdic_path}/indexed/merged.csv")
  merged <- read_indexfile(fname)
  
  edited <- DataEditR::data_edit(
    merged,
    col_readonly = c("표층형", "좌문맥ID", "우문맥ID", "품사태그", "의미부류",
                     "종성유무", "읽기", "타입", "첫번째품사", "마지막품사",
                     "표현", "인덱스표현"),
    logo = logo, title = title) 
  
  changed <- edited |> 
    left_join(merged |> 
                select(`표층형`, `낱말비용_before`=`낱말비용`),
              by = "표층형") |> 
    filter(`낱말비용` != `낱말비용_before`) |> 
    select(-`낱말비용_before`)
  
  n_changed <- n_dup <- 0
  
  if (NROW(changed) > 0) {
    n_changed <- NROW(changed)
    
    ## 인덱스 파일 파트 --------------------------------------------------------
    readr::write_csv(edited, 
                     file = glue::glue("{userdic_path}/indexed/merged.csv"),
                     col_names = FALSE)
    
    
    ## 인덱스 수정 이력 파일 파트 ----------------------------------------------
    fname <- glue::glue("{userdic_path}/indexed/changed.csv")
    
    if (file.exists(fname)) {
      change_log <- read_indexfile(fname)
      
      n_dup <- length(intersect(change_log$표층형, changed$표층형))
      
      change_log <- change_log |> 
        filter(!`표층형` %in% changed$표층형)
      
      changed <- changed |> 
        bind_rows(change_log)
    }
    
    readr::write_csv(changed, file = fname, col_names = FALSE)  
  }
  
  cli::cli_rule(glue::glue("사전 파일의 낱말비용 수정하기"))  
  if (n_dup > 0) {
    cli::cli_alert_warning(
      glue::glue("기 수정된 {n_dup}건의 낱말비용이 업데이트 되었습니다."))
  }  
  cli::cli_alert_success(c("낱말비용 수정 건수: {n_changed}"))
}


#' update user dictionary with user-defined dictionary files.
#' @description 수정된 사용자 정의 사전 파일을 이용하여 사용자 사전 업데이트
#' @details 사용자 사전정의 디렉토리 내에 있는 "merged.csv" 파일로 사용자 정의 사전을 업데이트/생성한다.
#' @param userdic_path character. 사용자 정의 명사 사전 파일이 존재하는 경로.
#' 지정하지 않으면 "./user_dic"이라는 이름의 경로를 사용함.
#' @param dic_file character. 생성할 사용자 사전 파일 이름.
#' 지정하지 않으면 "user-dic.dic"이라는 이름으로 생성함.
#' @examples
#' \dontrun{
#' update_userdic()
#' }
#' @importFrom glue glue
#' @export
update_userdic <- function(userdic_path = "./user_dic", dic_file = "user-dic.dic") {
  if (is_windows()) {
    script_path <- system.file("script", package = "bitNLP")
    script <- glue::glue("{script_path}\\update_userdic_win.ps1")
    
    cmd <- glue::glue('powershell -File "{script}" {userdic_path} {dic_file}')
    system(glue::glue("{cmd}"))
  } else {
    script_path <- system.file("script", package = "bitNLP")
    update_script <- glue::glue("/bin/bash {script_path}/update_userdic.sh")
    
    cmd <- glue::glue("{update_script} {userdic_path} {dic_file}")
    
    exec_file <- "/usr/local/libexec/mecab/mecab-dict-index"
    if (file.access(exec_file, 1) == -1) {
      if (.Platform$GUI %in% "RStudio") {
        input <- rstudioapi::askForPassword("sudo password")
      } else {
        input <- readline("Enter your password: ")
      }
      
      system("sudo -kS chmod -R 755 /usr/local/libexec/mecab", input = input)
    }
    
    if (file.access(userdic_path, 2) == -1) {
      if (!exists("input")) {
        if (.Platform$GUI %in% "RStudio") {
          input <- rstudioapi::askForPassword("sudo password")
        } else {
          input <- readline("Enter your password: ")
        }
      }
      
      system(glue::glue("sudo -kS {cmd}"), input = input)
    } else {
      system(cmd)
    }  
  }
}

