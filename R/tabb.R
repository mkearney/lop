#' tabb
#'
#' Generates a frequency table tibble
#'
#' @param ... One or more vectors, a data frame, or a data frame with tidy-
#'   selected columns
#' @param sort Logical indicating whether to sort in order from most to least
#'   frequent, defaults to TRUE
#' @export
tabb <- function(..., sort = TRUE) {
  if (is.data.frame(..1) && length(capture_dots(...)) > 1L) {
    x <- sel(...)
  } else {
    x <- list(...)
    names(x) <- names(pretty_dots(...))
  }
  uqs <- lapply(x, unique)
  x[] <- mapply(factor, x, levels = uqs, SIMPLIFY = FALSE,
    MoreArgs = list(exclude = NULL, ordered = FALSE))
  x$dnn <- names(x)
  x$useNA <- "always"
  if (NCOL(x) > 1) {
    x <- table(x)
  } else {
    x <- do.call("table", x)
  }
  x <- as.data.frame.table(x, responseName = "n",
    stringsAsFactors = FALSE)
  if (sort) {
    x <- arr(x, -n)
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
