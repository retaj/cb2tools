% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manage.R
\name{dbLinesPerTable}
\alias{dbLinesPerTable}
\title{report a number of lines per table}
\usage{
dbLinesPerTable(conn)
}
\arguments{
\item{conn}{DBI connection}
}
\description{
Quick and dirty way of ckecking what is in the database, should be
suerseded by a more intelligent reporting function in a future version TODO
}
\details{
A simple wrapper for DBI::dbConnect, exists pretty much only to
take advantage of password prompt.
}
