# ---------------------------------------------------------------------------- #
#' load a ciRcus object into circBase2
#'
#' Do whatever it takes to push an object + annotation to circBase2
#'
#' @param conn DBI connection
#' @param CS circRNAs (ciRcus object)
#' @param exp.annot experiment annotation (data table for now, would be nice to have a solid object down the line TODO)
#'
#' @importFrom rstudioapi askForPassword
#' @importFrom DBI dbConnect
#' @importFrom RMySQL MySQL
#' @export
dbDump <- function(conn, CS, exp.annot) {
  # cook table names
  table_prefix = paste0(exp.annot$organism, "_", exp.annot$assembly, "_")
  tables <- c(paste0(table_prefix, c("circles", "expression", "hosts")),
              "library", "study", "study_library")


  return(tables)
}
