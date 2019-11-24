#' Parse json
#'
#' Retrieves and/or parses JSON inputs into R lists and such
#'
#' @param x Input object. If a character vector is supplied then JSON content
#'   is parsed from the text. If a JSON file, URL, or xml_document, then the
#'   content is extracted into a character vector and then JSON content parsed.
#' @return A list representing the JSON data
#' @export
pj <- function(x) UseMethod("pj")

#' @export
pj.xml_document <- function(x) {
  x <- unlist(strsplit(as.character(x), "(?<=\\>)[ ]{0,}\n", perl = TRUE))
  pj(x)
}

#' @export
pj.list <- function(x) {
  x <- http_url(x)
  pj(x)
}

parse_quoted_names <- function(x) {
  m <- gregexpr("(?<=\")[^\"]+(?=\":)", x, perl = TRUE)
  regmatches(x, m)[[1]]
}

parse_quoted_values <- function(x) {
  x <- gsub("\\\\\"", "&Q", x)
  m <- gregexpr("(?<=\":)[[:alnum:]]+(?=,)", x, perl = TRUE)
  regmatches(x, m)[[1]] <- paste0('"', regmatches(x, m)[[1]], '"')
  m <- gregexpr("(?<=\")[^\"]{0,}(?=\")", x, perl = TRUE)
  i <- seq(1, length(m[[1]]), 2)
  m1 <- m[[1]][i]
  attr(m1, "match.length") <- attr(m[[1]], "match.length")[i]
  m1 <- structure(
    list(m1),
    class = "list"
    )
  regmatches(x, m1)
}
# foo <- function(x) {
#   parse_quoted_values(x)
#   i <- 0L
#   x <- gsub("\\\\\\{", "  ", x)
#   while (grepl("\\{[^\\{]+\\}", x)) {
#     cat(i <- i + 1L, fill = TRUE)
#     x <- sub("(?<=.{1})\\{", " ", x, perl = TRUE)
#     x <- sub("\\}", " ", x)
#   }
#   x
# }

#' @export
pj.character <- function(x, ...) {
  ## if local file
  if (length(x) == 1 && file.exists(x)) {
    if (!is.null(j <- try_null(jsonlite::fromJSON(x)))) {
      return(j)
    }
    x <- http_get(x, ...)
    return(pj(x))
  }
  ## if URL
  if (length(x) == 1 && grepl("^https?://\\S+", x)) {
    if (!is.null(j <- try_null(jsonlite::fromJSON(x)))) {
      return(j)
    }
    x <- http_get(x, ...)
    return(pj(x))
  }
  ## if character string split by line
  if (length(x) == 1) {
    x <- unlist(strsplit(as.character(x), "(?<=\\>)[ ]{0,}\n", perl = TRUE))
  }
  x <- gsub("&quot;", '"', x)
  x <- grep('":\\s{0,}\\S+', x, value = TRUE)
  if (length(x) == 0) {
    return(list())
  }
  if (length(x) == 1) {
    return(frj(x))
  }
  x <- lap(x, frj)
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
