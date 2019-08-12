tabb <- function(..., sort = TRUE) {
  if (is.data.frame(..1) && length(tbltools:::capture_dots(...)) > 1L) {
    x <- sel(...)
  } else {
    x <- list(...)
    names(x) <- names(tbltools:::pretty_dots(...))
  }
  uqs <- lapply(x, unique)
  x[] <- mapply(factor, x, levels = uqs, SIMPLIFY = FALSE)
  if (NCOL(x) > 1) {
    x <- table(x)
  } else {
    x <- do.call("table", x)
  }
  x <- as.data.frame.table(x, responseName = "n")
  if (sort) {
    x <- arr(x, -n)
  }
  as_tbl(x)
}
