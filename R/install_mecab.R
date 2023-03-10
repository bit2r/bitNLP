is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

is_linux <- function() {
  identical(tolower(Sys.info()[["sysname"]]), "linux")
}

is_mac <-function() {
  identical(tolower(Sys.info()[["sysname"]]), "darwin")
}

# RmecabKo의 mecab_libs() 참조
mecab_libs <- function() {
  mecabLibsData <- system.file("mecabLibs", package = "bitNLP")
  
  if (mecabLibsData != "") {
    con <- file(mecabLibsData, "r")
    
    tryCatch({
      mecabLibsLoc <- readLines(con)
    },
    finally = {
      close(con)
    })
  } else {
    mecabLibsLoc <- ""
  }
  
  return(list(mecab.libpath = mecabLibsLoc))
}

is_mecab_installed <- function(mecabLocation = "c:/mecab") {
  if (is_linux() | is_mac()) {
    if (is.null(suppressWarnings(system("which mecab", intern = TRUE)) %>% 
                attr("status"))) 
      return(TRUE)
    else
      return(FALSE)
  } else if (is_windows()) {
    mecabLibs <- getOption("mecab.libpath")
    
    if (is.null(mecabLibs) | mecabLibs == "") {
      mecabLibs <- mecabLocation
    }
    
    if (!is.null(mecabLibs)) {
      if (mecabLibs != "") {
        return(file.exists(file.path(mecabLibs, "mecab.exe"))) 
      } else {
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
  }
}


#' Register the path where Mecab-Ko is installed
#' @description 은전한닢 형태소분석기인 mecab-ko가 설치된 경로는 bitNLP 패키지에 등록.
#' @details 이 메뉴는 MS-Windows 운영체제에서만 지원한다. bitNLP를 설치하기 전에, 
#' 이미 mecab-ko를 설치하였을 경우에 그 설치 경로를 bitNLP가 인식하도록 도와준다. 
#' Windows는 mecab-ko를 반드시 "c:/mecab" 경로에 설치해야 한다. 
#' 만약이 다른 경로에 이미 설치하였다면, 이 경로로 옮겨 놓아야 한다.
#' @examples
#' \dontrun{
#' # regist_mecab_ko()
#' }
#' @export
#' @importFrom glue glue
regist_mecab_ko <- function() {
  if (is_linux() | is_mac()) {
    stop("regist_mecab_ko() function is possible only in Linux or Mac OSX")    
  }
  
  if (is_mecab_installed()) {
    stop("mecab-ko is already registed.")
  }
  
  mecabLocation <- "c:/mecab"
  
  if (file.exists(file.path(mecabLocation, "mecab.exe"))) {
    mecabLibsLoc <- file.path(system.file(package = "bitNLP"), "mecabLibs")
    
    if (!file.exists(mecabLibsLoc)) {
      con <- file(mecabLibsLoc, "w")
      tryCatch({
        cat(mecabLocation, file = con, sep = "\n")
      }, finally = {
        close(con)
      })
    }
    
    options(list(mecab.libpath = mecabLocation))
  } else {
    stop(glue::glue("Could not find mecab.exe in '{mecabLocation}'. Install mecab-ko with install_mecab_ko().")) 
  }
}  


#' Installation of Eunjeonhan morpheme analyzer and dic
#' @description 은전한닢 형태소분석기인 mecab-ko와 은전한닢 형태소분석기 사전인 
#' mecab-ko-dic을 사용자 환경에 설치한다.
#' @details Linux와 Mac은 소스를 가져다 컴파일하며, Windows는 바이너리를 가져다 복사한다.
#' Windows는 "c:/mecab" 경로에 설치된다. 만약이 다른 경로에 이미 설치하였다면, 이 경로로 옮겨 놓아야 한다.
#' @examples
#' \dontrun{
#' # install_mecab_ko()
#' }
#' @export
#' @importFrom glue glue
#' @importFrom rstudioapi askForPassword
install_mecab_ko <- function() {
  if (is_mecab_installed()) {
    stop("mecab-ko is already installed.")
  }
  
  script_path <- system.file("script", package = "bitNLP")
  install_script <- glue::glue("/bin/sh {script_path}/install_mecab.sh")
  
  if (!is_windows() & .Platform$GUI %in% "RStudio") {
    input <- rstudioapi::askForPassword("sudo password")
  } else if (!is_windows()) {
    input <- readline("Enter your password: ")
  }
  
  if (is_linux() | is_mac()) {
    # system2(command = "/bin/sh", args = install_script)
    system(glue::glue("sudo -kS {install_script}"), input = input)
  } else if (is_windows()) {
    ## Modify install_mecab() of RmecabKo package created by Kim Junhewk to install mecab-ko on Windows
    ## https://github.com/junhewk/RmecabKo/blob/master/R/install.R
    mecabLocation <- "c:/mecab"
    
    dir.create(mecabLocation, recursive = TRUE, showWarnings = FALSE)
    
    if (file.exists(file.path(mecabLocation, "mecab.exe"))) {
      mecabLibsLoc <- file.path(system.file(package = "bitNLP"), "mecabLibs")
      
      if (!file.exists(mecabLibsLoc)) {
        con <- file(mecabLibsLoc, "w")
        tryCatch({
          cat(mecabLocation, file = con, sep = "\n")
        }, finally = {
          close(con)
        })
      }
      
      options(list(mecab.libpath = mecabLocation))
      
      stop("Mecab is already existed. The package will use the binary in this location.")
    }
    
    if (.Machine$sizeof.pointer != 8) {
      mecabDist <- "https://github.com/Pusnow/mecab-ko-msvc/releases/download/release-0.9.2-msvc-3/mecab-ko-msvc-x86.zip"
    }
    else {
      mecabDist <- "https://github.com/Pusnow/mecab-ko-msvc/releases/download/release-0.9.2-msvc-3/mecab-ko-msvc-x64.zip"
    }
    
    mecabDest <- file.path(mecabLocation, "mecab.zip")
    
    cat("Install mecab-ko-msvc...")
    
    if (getRversion() >= "3.2") {
      method <- "wininet"
    }
    else {
      setI2 <- utils::"setInternet2"
      internal2 <- setI2(NA)
      
      if (!internal2) {
        on.exit(suppressWarnings(setI2(internal2)))
        suppressWarnings(setI2(TRUE))
      }
      
      method <- "internal"
    }
    
    suppressWarnings(download.file(url = mecabDist, destfile = mecabDest, 
                                   method = method))
    
    unzip(mecabDest, exdir = mecabLocation)
    
    cat("Install mecab-ko-dic-msvc...")
    
    mecabDicDist <- "https://github.com/Pusnow/mecab-ko-dic-msvc/releases/download/mecab-ko-dic-2.0.3-20170922-msvc/mecab-ko-dic-msvc.zip"
    mecabDicDest <- file.path(mecabLocation, "mecab_dic.zip")
    
    suppressWarnings(download.file(url = mecabDicDist, destfile = mecabDicDest, 
                                   method = method))
    
    unzip(mecabDicDest, exdir = mecabLocation)
    
    suppressWarnings(file.remove(mecabDest))
    suppressWarnings(file.remove(mecabDicDest))
    
    mecabLibsLoc <- file.path(system.file(package = "bitNLP"), "mecabLibs")
    
    con <- file(mecabLibsLoc, "w")
    tryCatch({
      cat(mecabLocation, file = con, sep = "\n")
    }, finally = {
      close(con)
    })
    
    options(list(mecab.libpath = mecabLocation))
  }  
}
