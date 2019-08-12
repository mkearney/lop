#' tbl
#'
#' See \code{tibble::\link[tibble:tibble]{tibble}} for details.
#'
#' @name tbl
#' @rdname tbl
#' @keywords internal
#' @export
tbl <- tibble::tibble

#' as_tbl
#'
#' See \code{tibble::\link[tibble:as_tibble]{as_tibble}} for details.
#'
#' @name as_tbl
#' @rdname as_tbl
#' @keywords internal
#' @export
as_tbl <- tibble::as_tibble

#' brows
#'
#' See \code{dplyr::\link[dplyr:bind_rows]{bind_rows}} for details.
#'
#' @name brows
#' @rdname brows
#' @keywords internal
#' @export
brows <- dplyr::bind_rows

#' bcols
#'
#' See \code{dplyr::\link[dplyr:bind_cols]{bind_cols}} for details.
#'
#' @name bcols
#' @rdname bcols
#' @keywords internal
#' @export
bcols <- dplyr::bind_cols

#' gby
#'
#' See \code{dplyr::\link[dplyr:group_by]{group_by}} for details.
#'
#' @name gby
#' @rdname gby
#' @keywords internal
#' @export
gby <- dplyr::group_by

#' ung
#'
#' See \code{dplyr::\link[dplyr:ungroup]{ungroup}} for details.
#'
#' @name ung
#' @rdname ung
#' @keywords internal
#' @export
ung <- dplyr::ungroup

#' mut
#'
#' See \code{dplyr::\link[dplyr:mutate]{mutate}} for details.
#'
#' @name mut
#' @rdname mut
#' @keywords internal
#' @export
mut <- dplyr::mutate

#' mut_if
#'
#' See \code{dplyr::\link[dplyr:mutate_if]{mutate_if}} for details.
#'
#' @name mut_if
#' @rdname mut_if
#' @keywords internal
#' @export
mut_if <- dplyr::mutate_if

#' mut_all
#'
#' See \code{dplyr::\link[dplyr:mutate_all]{mutate_all}} for details.
#'
#' @name mut_all
#' @rdname mut_all
#' @keywords internal
#' @export
mut_all <- dplyr::mutate_all

#' sms
#'
#' See \code{dplyr::\link[dplyr:summarise]{summarise}} for details.
#'
#' @name sms
#' @rdname sms
#' @keywords internal
#' @export
sms <- dplyr::summarise

#' sms_if
#'
#' See \code{dplyr::\link[dplyr:summarise_if]{summarise_if}} for details.
#'
#' @name sms_if
#' @rdname sms_if
#' @keywords internal
#' @export
sms_if <- dplyr::summarise_if

#' sms_all
#'
#' See \code{dplyr::\link[dplyr:summarise_all]{summarise_all}} for details.
#'
#' @name sms_all
#' @rdname sms_all
#' @keywords internal
#' @export
sms_all <- dplyr::summarise_all

#' sel
#'
#' See \code{dplyr::\link[dplyr:select]{select}} for details.
#'
#' @name sel
#' @rdname sel
#' @keywords internal
#' @export
sel <- dplyr::select

#' sel_if
#'
#' See \code{dplyr::\link[dplyr:select_if]{select_if}} for details.
#'
#' @name sel_if
#' @rdname sel_if
#' @keywords internal
#' @export
sel_if <- dplyr::select_if

#' fil
#'
#' See \code{dplyr::\link[dplyr:filter]{filter}} for details.
#'
#' @name fil
#' @rdname fil
#' @keywords internal
#' @export
fil <- dplyr::filter

#' arr
#'
#' See \code{dplyr::\link[dplyr:arrange]{arrange}} for details.
#'
#' @name arr
#' @rdname arr
#' @keywords internal
#' @export
arr <- dplyr::arrange

#' dsc
#'
#' See \code{dplyr::\link[dplyr:desc]{desc}} for details.
#'
#' @name dsc
#' @rdname dsc
#' @keywords internal
#' @export
dsc <- dplyr::desc

#' pul
#'
#' See \code{dplyr::\link[dplyr:pull]{pull}} for details.
#'
#' @name pul
#' @rdname pul
#' @keywords internal
#' @export
pul <- dplyr::pull

#' spr
#'
#' See \code{tidyr::\link[tidyr:spread]{spread}} for details.
#'
#' @name spr
#' @rdname spr
#' @keywords internal
#' @export
spr <- tidyr::spread

#' gat
#'
#' See \code{tidyr::\link[tidyr:gather]{gather}} for details.
#'
#' @name gat
#' @rdname gat
#' @keywords internal
#' @export
gat <- tidyr::gather

#' unn
#'
#' See \code{tidyr::\link[tidyr:unnest]{unnest}} for details.
#'
#' @name unn
#' @rdname unn
#' @keywords internal
#' @export
unn <- tidyr::unnest

#' glu
#'
#' See \code{glue::\link[glue:glue]{glue}} for details.
#'
#' @name glu
#' @rdname glu
#' @keywords internal
#' @export
glu <- glue::glue

#' fap
#'
#' See \code{future.apply::\link[future.apply:future_lapply]{future_lapply}} for details.
#'
#' @name fap
#' @rdname fap
#' @keywords internal
#' @export
fap <- future.apply::future_lapply

#' hread
#'
#' See \code{xml2::\link[xml2:read_html]{read_html}} for details.
#'
#' @name hread
#' @rdname hread
#' @keywords internal
#' @export
hread <- xml2::read_html

#' hurl
#'
#' See \code{xml2::\link[xml2:xml_url]{xml_url}} for details.
#'
#' @name hurl
#' @rdname hurl
#' @keywords internal
#' @export
hurl <- xml2::xml_url

#' hlst
#'
#' See \code{xml2::\link[xml2:as_list]{as_list}} for details.
#'
#' @name hlst
#' @rdname hlst
#' @keywords internal
#' @export
hlst <- xml2::as_list

#' hnode
#'
#' See \code{rvest::\link[rvest:html_node]{html_node}} for details.
#'
#' @name hnode
#' @rdname hnode
#' @keywords internal
#' @export
hnode <- rvest::html_node

#' hnodes
#'
#' See \code{rvest::\link[rvest:html_nodes]{html_nodes}} for details.
#'
#' @name hnodes
#' @rdname hnodes
#' @keywords internal
#' @export
hnodes <- rvest::html_nodes

#' htext
#'
#' See \code{rvest::\link[rvest:html_text]{html_text}} for details.
#'
#' @name htext
#' @rdname htext
#' @keywords internal
#' @export
htext <- rvest::html_text

#' hattr
#'
#' See \code{rvest::\link[rvest:html_attr]{html_attr}} for details.
#'
#' @name hattr
#' @rdname hattr
#' @keywords internal
#' @export
hattr <- rvest::html_attr

#' hattrs
#'
#' See \code{rvest::\link[rvest:html_attrs]{html_attrs}} for details.
#'
#' @name hattrs
#' @rdname hattrs
#' @keywords internal
#' @export
hattrs <- rvest::html_attrs

#' htable
#'
#' See \code{rvest::\link[rvest:html_table]{html_table}} for details.
#'
#' @name htable
#' @rdname htable
#' @keywords internal
#' @export
htable <- rvest::html_table