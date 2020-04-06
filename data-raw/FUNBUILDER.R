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

doc_pkg_fun_null <- function(pkg, og) {
  glue::glue("#' {og}
#'
#' See \\code{{{pkg}::\\link[{pkg}:{og}]{{{og}}}} for details.
#'
#' @name {og}
#' @rdname {og}
#' @keywords internal
#' @importFrom {pkg} {og}
#' @export
NULL\n")
}

more_dots <- list(
  c("dapr", "lap"),
  c("dapr", "ilap"),
  c("dapr", "vap_chr"),
  c("dapr", "vap_lgl"),
  c("dapr", "vap_int"),
  c("dapr", "vap_dbl"),
  c("dapr", "dapc"),
  c("dapr", "dapc_if")
)

dots_funs <- dapr::vap_chr(dots,
  ~ do.call("doc_pkg_fun", as.list(.x)))
more_dots_funs <- dapr::vap_chr(more_dots,
  ~ do.call("doc_pkg_fun_null", as.list(.x)))

cat(paste(c(dots_funs, more_dots_funs), collapse = "\n\n"),
  file = "R/utils-funs.R")


x <- c(
  doc_pkg_fun("ggplot2", "ggplot", "ggp"),
  doc_pkg_fun("ggplot2", "aes", "aes"),
  doc_pkg_fun("ggplot2", "geom_point", "gpn"),
  doc_pkg_fun("ggplot2", "geom_line", "gln"),
  doc_pkg_fun("ggplot2", "geom_smooth", "gsm"),
  doc_pkg_fun("ggplot2", "stat_smooth", "ssm"),
  doc_pkg_fun("ggplot2", "geom_polygon", "gpl"),
  doc_pkg_fun("ggplot2", "geom_text", "gtx"),
  doc_pkg_fun("ggplot2", "geom_label", "glb"),
  doc_pkg_fun("ggplot2", "geom_segment", "gsg"),
  doc_pkg_fun("ggplot2", "geom_vline", "gvl"),
  doc_pkg_fun("ggplot2", "geom_hline", "ghl"),
  doc_pkg_fun("ggplot2", "labs", "labs"),
  doc_pkg_fun("ggplot2", "ylim", "ylim"),
  doc_pkg_fun("ggplot2", "xlim", "xlim"),
  doc_pkg_fun("ggplot2", "rel", "rel"),
  doc_pkg_fun("ggplot2", "facet_wrap", "fwr"),
  doc_pkg_fun("ggplot2", "facet_grid", "fgr"),
  doc_pkg_fun("ggplot2", "annotate", "ant"),
  doc_pkg_fun("ggplot2", "element_line", "eln"),
  doc_pkg_fun("ggplot2", "element_text", "etx"),
  doc_pkg_fun("ggplot2", "element_rect", "erc"),
  doc_pkg_fun("ggplot2", "element_blank", "ebl"),
  doc_pkg_fun("ggplot2", "theme", "thm"),
  doc_pkg_fun("ggplot2", "scale_fill_viridis_d", "sfvd"),
  doc_pkg_fun("ggplot2", "scale_fill_viridis_c", "sfvc"),
  doc_pkg_fun("ggplot2", "scale_x_continuous", "sxc"),
  doc_pkg_fun("ggplot2", "scale_x_discrete", "sxd"),
  doc_pkg_fun("ggplot2", "scale_y_continuous", "sxc"),
  doc_pkg_fun("ggplot2", "scale_y_discrete", "syd"),
  doc_pkg_fun("ggplot2", "coord_cartesion", "ccr"),
  doc_pkg_fun("ggplot2", "coord_flip", "cfl")
)
cat(paste(
  x,
  collapse = "\n\n"
),
  file = "R/funs-ggplot.R")

ggplot2::scale_fill_viridis_d()
