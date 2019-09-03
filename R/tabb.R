#' tabb
#'
#' Generates a frequency table tibble
#'
#' @param ... One or more vectors, a data frame, or a data frame with tidy-
#'   selected columns
#' @param sort Logical indicating whether to sort in order from most to least
#'   frequent, defaults to TRUE
#' @param useNA whether to use NA values, default is "ifany" and other options
#'   include "always" and "no"
#' @return A frequency tibble data frame
#' @export
tabb <- function(..., sort = TRUE, useNA = "ifany") {
  if (is.data.frame(..1) && length(capture_dots(...)) > 1L) {
    x <- sel(...)
  } else if (!is.data.frame(..1)) {
    x <- list(...)
    names(x) <- names(pretty_dots(...))
  } else {
    x <- ..1
  }
  x <- as.data.frame.table(
    table(x, useNA = useNA, dnn = names(x)),
    responseName = "n", stringsAsFactors = FALSE)
  if (sort) {
    x <- x[order(x$n, decreasing = TRUE), ]
  }
  as_tbl(x)
}

capture_dots <- function(...) {
  eval(substitute(alist(...)), envir = parent.frame())
}

expr_names <- function(args) {
  vapply(args, deparse, USE.NAMES = FALSE, FUN.VALUE = character(1))
}

pretty_dots <- function(...) {
  dots <- capture_dots(...)
  if (length(dots) == 0) {
    return(NULL)
  }
  if (is.null(names(dots))) {
    names(dots) <- expr_names(dots)
  }
  nms <- names(dots)
  if ("" %in% nms) {
    names(dots)[nms == ""] <- expr_names(dots[nms == ""])
  }
  dots
}
