
## UTILS
as_params <- function(...) {
  x <- list(...)
  if (length(x) == 1 && is.list(x[[1]])) {
    x <- x[[1]]
  }
  if (!length(x)) {
    return("")
  }
  paste0("?", paste(names(x), x, sep = "=", collapse = "&"))
}
unslash_end <- function(x) sub("/$", "", x)
slash_end <- function(x) sub("(?<!/)$", "", x, perl = TRUE)
unslash_begin <- function(x) sub("^/", "", x)
slash_begin <- function(x) sub("^(?<!/)", "/", x, perl = TRUE)

## construct URL strings for http requests
http_url <- function(x, ...) UseMethod("http_url")

http_url.default <- function(x, ...) {
  x <- unslash_end(x)
  dots <- list(...)
  if (any(c("endpoint", "path") %in% names(dots))) {
    ep <- slash_begin(dots[[max(grep("^endpoint$|^path$", names(dots)))]])
    x <- paste0(x, ep)
  }
  paste0(x, as_params(dots[!names(dots) %in% c("", "endpoint", "path")]))
}

http_url.url <- function(x, ...) {
  httr::build_url(x)
}
http_url.url_list <- function(x) {
  url_str(x)
}

## create URL as list
url_lst <- function(x, ...) UseMethod("url_lst")

url_lst.default <- function(x, ...) {
  x <- httr::parse_url(http_url(x, ...))
  url_lst(x)
}
url_lst.url <- function(x) {
  x <- x[lengths(x) > 0]
  structure(x, class = c("url_list", "list"))
}
url_lst.list <- function(x) {
  x <- x[lengths(x) > 0]
  structure(x, class = c("url_list", "list"))
}

## create URL as string
url_str <- function(x, ...) UseMethod("url_str")

url_str.url <- function(x) {
  structure(httr::build_url(x), class = c("url_str", "character"))
}
url_str.list <- function(x) {
  structure(httr::build_url(x), class = c("url_str", "character"))
}
url_str.url_list <- function(x) {
  class(x) <- "url"
  structure(httr::build_url(x), class = c("url_str", "character"))
}

url_str.character <- function(x, ...) {
  http_url(x, ...)
}


## GET method
http_get <- function(x, config = list(), ...) {
  UseMethod("http_get")
}
http_get.character <- function(x, config = list(), ...) {
  x <- url_str(x, ...)
  httr::GET(x, config = config)
}
http_get.list <- function(x, config = list(), ...) {
  x <- url_str(x)
  httr::GET(x, config = config, ...)
}
http_get.url_str <- function(x, config = list(), ...) {
  httr::GET(x, config = config, ...)
}

## POST method
http_post <- function(x, config = list(), ...) {
  UseMethod("http_post")
}
http_post.character <- function(x, config = list(), ...) {
  x <- url_str(x, ...)
  httr::POST(x, config = config)
}
http_post.list <- function(x, config = list(), ...) {
  x <- url_str(x)
  httr::POST(x, config = config, ...)
}
http_post.url_str <- function(x, config = list(), ...) {
  httr::POST(x, config = config, ...)
}


try_null <- function(...) {
  tryCatch(
    ...,
    error = function(e) NULL
  )
}

try_lst <- function(...) {
  tryCatch(
    ...,
    error = function(e) list()
  )
}
try_df <- function(...) {
  tryCatch(
    ...,
    error = function(e) tbl()
  )
}

`%||%` <- function(a, b) {
  if (is.null(a))
    b
  else
    a
}
pbracket <- function(x) paste0("[", x, "]")

fix_json_string <- function(x) {
  x <- sub("^[^[{]+", "", x)
  while (any(grepl("^\\[\\]|^\\{\\}", x))) {
    x <- sub("^\\[\\]|^\\{\\}", "", x)
    x <- sub("^[^[{]+", "", x)
  }
  x <- sub("[^]}]+$", "", x)
  while (any(grepl("\\[\\]$|\\{\\}$", x))) {
    x <- sub("\\[\\]$|\\{\\}$", "", x)
    x <- sub("[^]}]+$", "", x)
  }
  x
}

pbracket <- function(x) paste0("[", x, "]")
fj_ <- function(x, ...) {
  o <- try_null(jsonlite::fromJSON(x, ...))
  o <- o %||% try_null(jsonlite::fromJSON(
    fix_json_string(x), ...))
  o %||% try_null(jsonlite::fromJSON(
    pbracket(x), ...))
}
fj <- function(x, ...) {
  if (length(x) == 1) {
    fj_(x, ...)
  } else {
    dapr::lap(x, fj_, ...)
  }
}
# url_str("http://mikewk.com/hey", this = 3)
# url_lst("http://mikewk.com/", path = "hey", now = TRUE, this = 3)
# url_str("http://mikewk.com/", path = "hey", now = TRUE, this = 3)
# url_str(url_lst("http://mikewk.com/hey", this = 3))
# url_str("http://mikewk.com/hey=2", this = 3)
# url_str("http://mikewk.com/hey=2", this = 3)
