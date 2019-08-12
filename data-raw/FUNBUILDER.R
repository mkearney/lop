## code to prepare `FUNBUILDER` dataset goes here

usethis::use_data("FUNBUILDER")

doc_pkg_fun <- function(pkg, og, fun) {
  glue::glue("#' {fun}
#'
#' See \\code{{{pkg}::\\link[{pkg}:{og}]{{{og}}}} for details.
#'
#' @name {fun}
#' @rdname {fun}
#' @keywords internal
#' @export
{fun} <- {pkg}::{og}\n")
}

dots <- list(
  c("tibble", "tibble", "tbl"),
  c("tibble", "as_tibble", "as_tbl"),
  c("dplyr", "bind_rows", "brows"),
  c("dplyr", "bind_cols", "bcols"),
  c("dplyr", "group_by", "gby"),
  c("dplyr", "ungroup", "ung"),
  c("dplyr", "mutate", "mut"),
  c("dplyr", "mutate_if", "mut_if"),
  c("dplyr", "mutate_all", "mut_all"),
  c("dplyr", "summarise", "sms"),
  c("dplyr", "summarise_if", "sms_if"),
  c("dplyr", "summarise_all", "sms_all"),
  c("dplyr", "select", "sel"),
  c("dplyr", "select_if", "sel_if"),
  c("dplyr", "filter", "fil"),
  c("dplyr", "arrange", "arr"),
  c("dplyr", "desc", "dsc"),
  c("dplyr", "pull", "pul"),

  c("tidyr", "spread", "spr"),
  c("tidyr", "gather", "gat"),
  c("tidyr", "unnest", "unn"),

  c("glue", "glue", "glu"),

  c("future.apply", "future_lapply", "fap"),

  c("xml2", "read_html", "hread"),
  c("xml2", "xml_url", "hurl"),
  c("xml2", "as_list", "hlst"),
  c("rvest", "html_node", "hnode"),
  c("rvest", "html_nodes", "hnodes"),
  c("rvest", "html_text", "htext"),
  c("rvest", "html_attr", "hattr"),
  c("rvest", "html_attrs", "hattrs"),
  c("rvest", "html_table", "htable")
)

cat(paste(dapr::vap_chr(
  dots,
  ~ do.call("doc_pkg_fun", as.list(.x))
), collapse = "\n\n"),
  file = "R/utils-funs.R")
