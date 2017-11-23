#' cb2tools: circBase2 management tools
#'
#' The foo package provides three categories of important functions:
#' foo, bar and baz.
#'
#' @section Foo functions:
#' The foo functions ...
#'
#' @docType package
#' @name cb2tools
#'
#' @importFrom data.table data.table
# @import DBI
# @import RMySQL
NULL

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.cb2 <- list(
    db.username = "cb2admin",
    mysql.db    = "circBase2"
  )
  toset <- !(names(op.cb2) %in% names(op))
  if (any(toset)) options(op.cb2[toset])

  invisible()
}
