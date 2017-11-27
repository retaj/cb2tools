# load some data
library(ciRcus)
library(data.table)
annot.list <- loadAnnotation(paste(path.package("ciRcus"), "extdata/db/hsa_ens75_minimal.sqlite", sep="/"))
cdata <- data.frame(sample = c("FC1", "FC2", "H1", "H2", "L1", "L2"),
                    filename = dir(paste(path.package("ciRcus"), "extdata/encode_demo_small/", sep="/"),
                                   full.names = T))

circs.se <- summarizeCircs(colData = cdata, keep.linear = TRUE, wobble = 1,
                           subs = "all", qualfilter = TRUE, keepCols = 1:12)
circs.se <-  annotateCircs(se = circs.se, annot.list = annot.list,
                           assembly = "hg19")
circs.se


# connect to circBase2
library(cb2tools)
library(DBI)
conn <- cb2connect(password="01cb2admin")

# list tables
dbListTables(conn)
dbLinesPerTable(conn)

CS.annot <- list(
  organism = "hsa",
  assembly = "hg19"
)

dbDump(conn, circs.se, CS.annot)
dbLinesPerTable(conn)
dbDisconnect(conn)


##################
### study_name ###
##################
dbListFields(conn, "study")
insertStatement <- sqlAppendTable(conn,
                                  "study",
                                  data.frame(name        = "ENCODE",
                                             description = "magic ENCODE stuff from the interwebz",
                                             reference   = "www.encodeproject.org"))
dbExecute(conn, insertStatement)

####################
### library_name ###
####################
libz <- data.table(name        = colnames(circs.se),
                   description = paste(colnames(circs.se),  "desc"),
                   reference   = paste0(colnames(circs.se), "_ref"))
dbListFields(conn, "library")
insertStatement <- sqlAppendTable(conn,
                                  "library",
                                  libz)
dbExecute(conn, insertStatement)

#####################
### study_library ###
#####################
refz <- data.table(study   = rep("ENCODE", length(colnames(circs.se))),
                   library = colnames(circs.se))


res <- dbSendQuery(conn, paste0("SELECT studyID, name FROM study WHERE name IN ", collapseVector(refz$study)))
tmp <- fetch(res)

refz <- merge(refz, tmp, by.x = "study", by.y = "name")

res <- dbSendQuery(conn, paste0("SELECT libraryID, name FROM library WHERE name IN ", collapseVector(refz$library)))
tmp <- fetch(res)

refz <- merge(refz, tmp, by.x = "library", by.y = "name")

# this shit should be tested before INSERT
insertStatement <- sqlAppendTable(conn,
                                  "study_library",
                                  refz[,.(studyID, libraryID)])
dbExecute(conn, insertStatement) # YOLO





###########################
### hsa_hg19_expression ###
###########################
dbListFields(conn, "hsa_hg19_expression")

tab2 <- merge(tab, chunk, by="circBaseID")
tab.list <- lapply(refz$library, function(x) tab2[, c(1, grep(x, names(tab2))), with=F])
tab.list <- lapply(tab.list, setnames, c("circBaseID",
                                         "n_reads_circ",
                                         "n_reads_circ_uniq",
                                         "n_reads_5pr",
                                         "n_reads_3pr",
                                         "ratio"))
tab.list <- lapply(tab.list, function(x) merge(x, tab2[,.(circBaseID, circID)], by="circBaseID"))
names(tab.list) <- refz$library

for (lib in names(tab.list)) {
  # yay for loop
  print(lib)
  tab.list[[lib]]$libraryID <- refz[library == lib]$libraryID
  tab.list[[lib]]$n_reads_5pr_uniq <- NA
  tab.list[[lib]]$n_reads_3pr_uniq <- NA

  insertStatement <- sqlAppendTable(conn,
                                    "hsa_hg19_expression",
                                    tab.list[[lib]][,.(circID, libraryID,
                                                       n_reads_circ, n_reads_5pr, n_reads_3pr,
                                                       n_reads_circ_uniq, n_reads_5pr_uniq,
                                                       n_reads_3pr_uniq)])
  dbExecute(conn, insertStatement) # YOLO


}

dbDisconnect(conn)

