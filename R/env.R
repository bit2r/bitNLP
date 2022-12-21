set_taenv <- function(name, value) {
  assign(name, value, envir = .bitNLPEnv)
}

unset_taenv <- function(name) {
  value <- .getTAEnv(name)
  if (!is.null(value)) {
    rm(list = name, envir = .bitNLPEnv)
  }
}

get_taenv <- function(name) {
  if (missing(name)) {
    as.list(.bitNLPEnv)
  } else {
    .bitNLPEnv[[name]]
  }
}
