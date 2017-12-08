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

  id2cbid <- data.table(fetch(res))

  tmp.DT <- merge(DT, id2cbid, by="circBaseID")
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

  #############
  ### study ###
  #############
  # dbListFields(conn, "study")
  insertStatement <- sqlAppendTable(con   = conn,
                                    table = "study",
                                    values = CS.annot$study)
  dbExecute(conn, insertStatement)

  ###############
  ### library ###
  ###############
  # dbListFields(conn, "library")
  insertStatement <- sqlAppendTable(con = conn,
                                    table = "library",
                                    values = CS.annot$lib)
  dbExecute(conn, insertStatement)

  #####################
  ### study_library ###
  #####################
  # will probably not need this in the future when the keys are set properly
  refz <- data.table(study   = rep("ENCODE", length(colnames(circs.se))),
                     library = colnames(circs.se))

  res <- dbSendQuery(conn, paste0("SELECT studyID, name FROM study WHERE name IN ", collapseVector(refz$study)))
  tmp <- fetch(res)

  refz <- merge(refz, tmp, by.x = "study", by.y = "name")

  res <- dbSendQuery(conn, paste0("SELECT libraryID, name FROM library WHERE name IN ", collapseVector(refz$library)))
  tmp <- fetch(res)

  refz <- merge(refz, tmp, by.x = "library", by.y = "name")

  # ^this shit should be tested before INSERT
  insertStatement <- sqlAppendTable(con    =   conn,
                                    table  = "study_library",
                                    values = refz[,.(studyID, libraryID)])
  dbExecute(conn, insertStatement) # YOLO

  ######################
  ### X_Y_expression ###
  ######################
  #dbListFields(conn, "hsa_hg19_expression")

  DT2 <- merge(DT, id2cbid, by="circBaseID")
  tab.list <- lapply(CS.annot$lib$name, function(x) DT2[, c(1, grep(x, names(DT2))), with=F])
  tab.list <- lapply(tab.list, setnames, c("circBaseID",
                                           "n_reads_circ",
                                           "n_reads_circ_uniq",
                                           "n_reads_5pr",
                                           "n_reads_3pr",
                                           "ratio"))
  tab.list <- lapply(tab.list, function(x) merge(x, DT2[,.(circBaseID, circID)], by="circBaseID"))
  names(tab.list) <- CS.annot$lib$name

  res <- dbSendQuery(conn, paste0("SELECT libraryID, name FROM library WHERE name IN ", collapseVector(names(tab.list))))
  libid2libname <- data.table(fetch(res))


  for (lib in names(tab.list)) {
    # yay for loop
    print(lib)
    tab.list[[lib]]$libraryID <- libid2libname[name == lib]$libraryID
    tab.list[[lib]]$n_reads_5pr_uniq <- NA
    tab.list[[lib]]$n_reads_3pr_uniq <- NA

    insertStatement <- sqlAppendTable(con   = conn,
                                      table = db.tables[2],
                                      tab.list[[lib]][,.(circID, libraryID,
                                                         n_reads_circ, n_reads_5pr, n_reads_3pr,
                                                         n_reads_circ_uniq, n_reads_5pr_uniq,
                                                         n_reads_3pr_uniq)])
    dbExecute(conn, insertStatement) # YOLO


  }



  return(db.tables)
}
