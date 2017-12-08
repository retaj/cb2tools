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
  assembly = "hg19",
  study    = data.frame(name        = "ENCODE",
                        description = "magic ENCODE stuff from the interwebz",
                        reference   = "encodeproject.org"),
  lib      = data.frame(name        = colnames(circs.se),
                        description = paste(colnames(circs.se),  "desc"),
                        reference   = paste0(colnames(circs.se), "_ref"))
)

dbDump(conn, circs.se, CS.annot)
dbLinesPerTable(conn)
dbDisconnect(conn)
