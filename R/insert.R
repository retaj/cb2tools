# ---------------------------------------------------------------------------- #
#' load a ciRcus object into circBase2
#'
#' Do whatever it takes to push an object + annotation to circBase2
#'
#' @param conn DBI connection
#' @param CS circRNAs (ciRcus object)
#' @param exp.annot experiment annotation (data table for now, would be nice to have a solid object down the line TODO)
#'
#' @importFrom DBI dbExecute
#' @importFrom RMySQL MySQL dbListFields
#' @importFrom ciRcus resTable
#' @export
dbDump <- function(conn, CS, CS.annot) {
  # cook table names
  table_prefix <- paste0(CS.annot$organism, "_", CS.annot$assembly, "_")
  db.tables <- c(paste0(table_prefix, c("circles", "expression", "hosts")),
                 "library", "study", "study_library")

  DT <- resTable(CS)
  DT$circBaseID <- getcircBaseIDs(DT)



  ###################
  ### X_Y_circles ###
  ###################
  #dbListFields(conn, db.tables[1])
  tmp.DT <- DT[,.(circBaseID, chr, start, end, strand)]
  setnames(tmp.DT, c("circBaseID", "chrom", "pos_start", "pos_end", "strand"))

  insertStatement <- sqlAppendTable(conn, db.tables[1], tmp.DT) # TODO: a more solid tables object
  dbExecute(conn, insertStatement)
  rm(tmp.DT)

  #################
  ### X_Y_hosts ###
  #################
  # dbListFields(conn, db.tables[3])
  res <- dbSendQuery(conn, paste("SELECT circID, circBaseID
                                   FROM", db.tables[1],
                                 "WHERE circBaseID IN ",
                                   collapseVector(DT$circBaseID)))

  res.DT <- data.table(fetch(res))

  tmp.DT <- merge(DT, res.DT, by="circBaseID")
  tmp.DT$spliced_length <- tmp.DT$end - tmp.DT$start # TODO: this is dummy shit
  tmp.DT <- tmp.DT[,.(circID, spliced_length, gene_id, feature, junct.known)]
  tmp.DT$best_transcript <- tmp.DT$gene_id
  tmp.DT$gene_symbol <- tmp.DT$gene_id
  tmp.DT$feature_start <-  sapply(strsplit(tmp.DT$feature, ":"), "[", 1)
  tmp.DT$feature_end   <-  sapply(strsplit(tmp.DT$feature, ":"), "[", 2)
  setnames(tmp.DT, "junct.known", "junct_known")
  tmp.DT <- tmp.DT[,.(circID, spliced_length, best_transcript, gene_id,
                      gene_symbol, feature_start, feature_end, junct_known)]

  rm(res.DT)

  insertStatement <- sqlAppendTable(conn, db.tables[3], tmp.DT)
  dbExecute(conn, insertStatement)
  rm(tmp.DT)



  return(db.tables)
}
