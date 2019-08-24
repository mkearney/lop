pj <- function(x) UseMethod("pj")

pj.xml_document <- function(x) {
  x <- unlist(strsplit(as.character(x), "(?<=\\>)[ ]{0,}\n", perl = TRUE))
  pj(x)
}

pj.list <- function(x) {
  x <- http_url(x)
  pj(x)
}
pj.character <- function(x, ...) {
  if (length(x) == 1 && grepl("^https?://\\S+", x)) {
    x <- http_get(x, ...)
    return(pj(x))
  }
  if (length(x) == 1) {
    x <- unlist(strsplit(as.character(x), "(?<=\\>)[ ]{0,}\n", perl = TRUE))
  }
  x <- gsub("&quot;", '"', x)
  x <- grep('":\\s{0,}\\S+', x, value = TRUE)
  if (length(x) == 0) {
    return(list())
  }
  if (length(x) == 1) {
    return(fj(x))
  }
  x <- lap(x, fj)
  if (all(lengths(x) == 0)) {
    return(list())
  }
  if (sum(lengths(x) > 0) == 1) {
    return(x[[lengths(x) > 0]])
  }
  x[lengths(x) > 0]
}
http_text <- function(x) {
  httr::content(x, as = "text", encoding = "UTF-8")
}
http_parsed <- function(x) {
  httr::content(x, as = "parsed")
}

pj.response <- function(x) {
  x <- as.character(http_parsed(x))
  pj(x)
}
