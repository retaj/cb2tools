# ---------------------------------------------------------------------------- #
#' connect to circBase2
#'
#' Connect to a local (default) or remote instance of circBase2.
#'
#' A simple wrapper for DBI::dbConnect, exists pretty much only to
#' take advantage of password prompt.
#'
#' @param host hostname (localhost)
#' @param user username (cb2admin)
#' @param password cb2 password
#' @param dbname database name (circBase2)
#'
#'
#' @importFrom rstudioapi askForPassword
#' @importFrom DBI dbConnect
#' @importFrom RMySQL MySQL
#' @export
cb2connect <- function(host     = "localhost",
                       db.user  = getOption("db.username"),
                       password = rstudioapi::askForPassword("cb2 password"),
                       dbname   = getOption("mysql.db")) {

  connection <- dbConnect(drv = MySQL(), host = host, user = db.user, password = password, dbname = dbname)

  return(connection)
}


# ---------------------------------------------------------------------------- #
#' report a number of lines per table
#'
#' Quick and dirty way of ckecking what is in the database, should be
#' suerseded by a more intelligent reporting function in a future version TODO
#'
#' A simple wrapper for DBI::dbConnect, exists pretty much only to
#' take advantage of password prompt.
#'
#' @param conn DBI connection
#'
#' @importFrom RMySQL dbSendQuery fetch
#' @export
dbLinesPerTable <- function(conn) {
  res <- dbSendQuery(conn, paste0("SELECT table_name, table_rows
                                   FROM INFORMATION_SCHEMA.TABLES
                                   WHERE TABLE_SCHEMA = \"", getOption("mysql.db"), "\""))
  DT <- data.table(fetch(res))

  return(DT)
}
