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
conn <- cb2connect(password="01cb2admin")

# list tables
dbListTables(conn)
dbLinesPerTable(conn)

CS.annot <- list(
  organism <- "hsa",
  assembly <- "hg19"
)



tab <- resTable(circs.se)
tab$circBaseID <- paste0("hsa_circ_000000", 1:4)

########################
### hsa_hg19_circles ###
########################

dbListFields(conn, "hsa_hg19_circles")
tab1 <- tab[,.(circBaseID, chr, start, end, strand)]
setnames(tab1, c("circBaseID", "chrom", "pos_start", "pos_end", "strand"))

insertStatement <- sqlAppendTable(conn, "hsa_hg19_circles", tab1)
dbExecute(conn, insertStatement)


######################
### hsa_hg19_hosts ###
######################
dbListFields(conn, "hsa_hg19_hosts")
res <- dbSendQuery(conn, paste0("SELECT circID, circBaseID
                                FROM   hsa_hg19_circles
                                WHERE circBaseID IN ",
                                "(\"",
                                paste(tab$circBaseID, collapse = "\", \""),
                                "\")"))
while(!dbHasCompleted(res)){
  chunk <- fetch(res, n = 10000)
}

tab2 <- merge(tab, chunk, by="circBaseID")
tab2$spliced_length <- tab2$end - tab2$start
tab3 <- tab2[,.(circID, spliced_length, gene_id, feature, junct.known)]
tab3$best_transcript <- tab3$gene_id
tab3$gene_symbol <- tab3$gene_id
tab3$feature_start <-  sapply(strsplit(tab3$feature, ":"), "[", 1)
tab3$feature_end   <-  sapply(strsplit(tab3$feature, ":"), "[", 2)
setnames(tab3, "junct.known", "junct_known")
tab3 <- tab3[,.(circID, spliced_length, best_transcript, gene_id,
                gene_symbol, feature_start, feature_end, junct_known)]

rm(tab2)
rm(tab3)

insertStatement <- sqlAppendTable(conn, "hsa_hg19_hosts", tab3)
dbExecute(conn, insertStatement)



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

