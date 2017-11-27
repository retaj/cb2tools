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

# ---------------------------------------------------------------------------- #
#' fetch circBase IDs from the database
#'
#' for a given SummarizedExperiment, fetch circBase IDs based on chr:start-endStrand
#' for now, generate dummy temporary IDs
#'
#' @export
getcircBaseIDs <- function(DT) {

  return(paste0("dum_circ_", sprintf("%07d", 1:nrow(DT))))
}
