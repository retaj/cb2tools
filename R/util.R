# ---------------------------------------------------------------------------- #
#' collapse a character vector to SQL multiquery thing
#'
#' cook a character vector of baz, quux, quux, ... into format suitable for
#' SELECT * FROM foo WHERE bar IN ("baz", "quux", "quuux")
#'
#' A simple wrapper for DBI::dbConnect, exists pretty much only to
#' take advantage of password prompt.
#'
#' @param charvec a character vector
#'
#' @export
collapseVector <- function(charvec) {
  collapsed.charvec <- paste0("(\"",
                              paste(charvec, collapse = "\", \""),
                              "\")")

  return(collapsed.charvec)
}
