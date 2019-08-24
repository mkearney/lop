is_call <- function(x) tryCatch(inherits(x, "call"), error = function(e) TRUE)
is_sym <- function(x) inherits(x, "symbol")
is_class <- function(class) {
  function(x) {
    if (is_call(x)) {
      return(FALSE)
    }
    if (is_sym(x)) {
      x <- eval(x)
    }
    inherits(x, class)
  }
}
is_fun <- is_class("function")

runtwice <- function(.f) {
  if (is_fun(.f)) {
    return(function(...) {
      x <- tryCatch(.f(...), error = function(e) NULL)
      if (is.null(x)) {
        x <- tryCatch(.f(...), error = function(e) NULL)
      }
      x
    })
  }
  x <- suppressWarnings(
    tryCatch(eval(.f, envir = parent.frame()), error = function(e) NULL)
  )
  if (!is.null(x)) {
    return(x)
  }
  suppressWarnings(
    tryCatch(eval(.f, envir = parent.frame()), error = function(e) NULL)
  )
}
