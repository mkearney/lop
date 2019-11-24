
pluckurl <- function(x) {
  if (length(x) == 0) {
    stop("No URL found")
  }
  if ("url" %in% names(x)) {
    return(x$url)
  }
  if (is.null(names(x))) {
    return(x[dapr::vap_lgl(x, is.character)][[1]])
  }
  x <- x[names(x) == ""]
  if (length(x) == 0) {
    stop("No URL found")
  }
  x[[1]]
}
## construct URL strings for http requests
http_url <- function(x) UseMethod("http_url")
http_url.list <- function(x) {
  query <- x[!names(x) %in% c("", "url")]
  x <- httr::parse_url(
    pluckurl(x)
  )
  x$query <- query
  httr::build_url(x)
}
http_url.character <- function(x) {
  httr::build_url(httr::parse_url(x))
}
http_url.url <- function(x) {
  if (inherits(x, "connection")) {
    x <- httr::parse_url(summary(x)[[1]])
  }
  httr::build_url(x)
}


## GET method
http_get <- function(x, ...) {
  UseMethod("http_get")
}
http_get.character <- function(x, ...) {
  httr::GET(x, ...)
}
http_get.list <- function(x, ...) {
  x <- http_url(x)
  httr::GET(x, ...)
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
`%|||%` <- function(a, b) {
  if (!is.null(a))
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
frj_ <- function(x, ...) {
  o <- try_null(jsonlite::fromJSON(x, ...))
  o <- o %||% try_null(jsonlite::fromJSON(
    fix_json_string(x), ...))
  o %||% try_null(jsonlite::fromJSON(
    pbracket(x), ...))
}
frj <- function(x, ...) {
  if (length(x) == 1) {
    frj_(x, ...)
  } else {
    dapr::lap(x, frj_, ...)
  }
}

all_pos_json <- function(x) {
  c(
    hnodes(x, "script") %>% htext(),
    hnodes(x, "script") %>% hattrs() %>% unlist(),
    hnodes(x, "meta") %>% hattrs() %>% unlist()
  )
}
all_var_names <- function(x) {
  UseMethod("all_var_names")
}
all_var_names.default <- function(x) {
  character()
}
all_var_names.list <- function(x) {
  if (!any(dapr::vap_lgl(x, is.recursive))) {
    return(names(x))
  }
  c(names(x), unlist(unname(dapr::lap(x, all_var_names)), recursive = FALSE))
}

all_pos_txt <- function(x) {
  sh <- utils::capture.output(xml2::html_structure(x))
  sh <- trimws(sh)
  sh <- grep("^[^<]|[^>]$", sh, value = TRUE, invert = TRUE)
  sh <- gsub("\\s{0,}\\[[^]]+\\]>", ">", sh)
  sh <- gsub("\\..*", ">", sh)
  sh <- ifelse(grepl("<script", sh), sh %P% "</script>", sh)
  sh <- ifelse(grepl("<style", sh), sh %P% "</style>", sh)
  yn <- grep("^<[/ ]{0,}html", sh, invert = TRUE)
  sh[yn] <- "<" %P% gsub("^<|[.# >].*", "", sh[yn]) %P% "/>"
  all_nodes <- paste(sh, collapse = " ") %P% "</html>" %>% hread() %>%
    hlst() %>% all_var_names() %>% unique()
  x <- unlist(dapr::lap(all_nodes, ~ c(hnodes(x, .x) %>% hattrs() %>% unlist(),
    hnodes(x, .x) %>% htext())))
  grep('":', x, value = TRUE)
}
rm_empty <- function(x) UseMethod("rm_empty")

rm_empty.default <- function(x) {
  if (is.atomic(x)) {
    return(x)
  }
  x[lengths(x) > 0]
}

rm_empty.data.frame <- function(x) {
  x
}

rm_empty.list <- function(x) {
  x <- dapr::lap(x[lengths(x) > 0], rm_empty)
  x[lengths(x) > 0]
}

get_text <- function(x) {
  httr::content(httr::GET(x), as = "text",
    encoding = "UTF-8")
}
get_parsed <- function(x) {
  httr::content(httr::GET(x), as = "parsed")
}


get_var_ <- function(x, var) UseMethod("get_var_")
get_var_.list <- function(x, var) {
  if (length(x) == 0) {
    x <- NA
  } else {
    while (any(recs <- dapr::vap_lgl(x, is_rec))) {
      x <- c(lapply(x[!recs], unlist),
        unlist(unname(x[recs]), recursive = FALSE))
    }
    if (length(var) > 0) {
      x <- unlist(x[names(x) == var], use.names = FALSE)
    } else {
      x <- NA
    }
  }
  x
}
get_var_.default <- function(x, var) NULL

names2 <- function(x) {
  x <- names(x)
  #x[!duplicated(sub("\\d+$", "", x))]
  x
}

all_the_names <- function(x) {
  nms <- names2(x)
  while (any(dapr::vap_lgl(x, is.recursive))) {
    x <- unlist(unname(x), recursive = FALSE)
    nms <- c(nms, sub("^[^.]+\\.", "", names2(x)))
  }
  nms
}

get_var <- function(x, ...) UseMethod("get_var")
get_var.list <- function(x, ...) {
  dots <- tidyselect::vars_select(unique(all_the_names(x)), ...)
  dapr::lap(dots, ~ get_var_(x, .x))
}
get_var.default <- function(x, var) NULL

is_rec <- function(x) is.recursive(x) && isTRUE(any(names(x) != ""))
