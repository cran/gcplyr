## ----global options, include = FALSE------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(root.dir = tempdir())

#Note: to knit to pdf, word, and html simultaneously, run in R console:
#{rmarkdown::render("./vignettes/import_transform.Rmd", rmarkdown::html_vignette(toc = TRUE, toc_dept = 4)); rmarkdown::render("./vignettes/import_transform.Rmd", rmarkdown::pdf_document(toc = TRUE, toc_depth = 4)); rmarkdown::render("./vignettes/import_transform.Rmd", rmarkdown::word_document(toc = TRUE, toc_dept = 4))}

## ----setup--------------------------------------------------------------------
library(gcplyr)

## ---- include = FALSE---------------------------------------------------------
print_df <- function(df, col.names = FALSE) {
  write.table(format(df, justify = "right"),
              row.names=FALSE, col.names = col.names, quote = F)
}

## -----------------------------------------------------------------------------
#This code just creates a series of block-shaped example files
#Don't worry about how it works - when working with real growth
#curves data, all these files would be created by the plate reader
temp_filenames <- 
  paste("Plate1-", 
        paste(example_widedata_noiseless$Time %/% 3600,
              formatC((example_widedata_noiseless$Time %% 3600) %/% 60, 
                      width = 2, flag = 0),
              formatC((example_widedata_noiseless$Time %% 3600) %% 60,
                      width = 2, flag = 0),
              sep = "_"), ".csv", sep = "")
for (i in 1:length(temp_filenames)) {
  temp_filenames[i] <- strsplit(temp_filenames[i], split = "\\\\")[[1]][
    length(strsplit(temp_filenames[i], split = "\\\\")[[1]])]
}
for (i in 1:length(temp_filenames)) {
  write.table(
    cbind(
      matrix(c("", "", "", "", "A", "B", "C", "D", "E", "F", "G", "H"), 
             nrow = 12),
      rbind(rep("", 12),
            matrix(c("Time", example_widedata_noiseless$Time[i], rep("", 10)), 
                   ncol = 12),
            rep("", 12),
            matrix(1:12, ncol = 12),
            matrix(
              example_widedata_noiseless[i, 2:ncol(example_widedata_noiseless)],
              ncol = 12))
    ), 
    file = temp_filenames[i], quote = FALSE, row.names = FALSE, sep = ",",
    col.names = FALSE)
}

## -----------------------------------------------------------------------------
#Here we print all the files we're going to read
list.files(pattern = "Plate1.*csv")

#Here we save them to the temp_filenames variable
temp_filenames <- list.files(pattern = "Plate1.*csv")

## -----------------------------------------------------------------------------
print_df(read.csv(temp_filenames[1], header = FALSE, colClasses = "character"))

## -----------------------------------------------------------------------------
#Now let's read it with import_blockmeasures
imported_blockdata <- import_blockmeasures(
  files = temp_filenames, startrow = 4)

head(imported_blockdata, c(6, 8))

## -----------------------------------------------------------------------------
#We can specify rows or columns by Excel-style letters too
imported_blockdata <- import_blockmeasures(
  files = temp_filenames,
  startrow = 4, startcol = "A")

## -----------------------------------------------------------------------------
print_df(read.csv(temp_filenames[1], header = FALSE, colClasses = "character"))

## -----------------------------------------------------------------------------
#Reading the blockcurves files with metadata included
imported_blockdata <- import_blockmeasures(
  files = temp_filenames,
  startrow = 4,
  metadata = list("time" = c(2, 3)))

head(imported_blockdata, c(6, 8))

## -----------------------------------------------------------------------------
#Reading the blockcurves files with metadata included
imported_blockdata <- import_blockmeasures(
  files = temp_filenames,
  startrow = 4,
  metadata = list("time" = c(2, "C")))

## -----------------------------------------------------------------------------
#This code just creates an example file with multiple blocks
#Don't worry about how it works - when working with real growth
#curves data, this would be created by the plate reader
write_blocks(read_blocks(files = temp_filenames,
                         startrow = 4,
                         metadata = list("time" = c(2, "C"))),
             file = "blocks_single.csv",
             output_format = "single",
             block_name_location = "file")

## -----------------------------------------------------------------------------
print_df(head(read.csv("blocks_single.csv", header = FALSE, 
                       colClasses = "character"),
              c(20, 8)))

## -----------------------------------------------------------------------------
imported_blockdata <- import_blockmeasures(
  "blocks_single.csv",
  startrow = seq(from = 3, to = 1155, by = 12),
  endrow = seq(from = 11, to = 1163, by = 12),
  startcol = 1, endcol = 13)

## -----------------------------------------------------------------------------
head(imported_blockdata, c(6, 8))

## -----------------------------------------------------------------------------
imported_blockdata <- import_blockmeasures(
  "blocks_single.csv",
  startrow = seq(from = 3, to = 1155, by = 12),
  endrow = seq(from = 11, to = 1163, by = 12),
  startcol = 1, endcol = 13,
  metadata = list("time" = list(seq(from = 2, to = 1154, by = 12), 2)))

## -----------------------------------------------------------------------------
head(imported_blockdata, c(6, 8))

## -----------------------------------------------------------------------------
#This code just creates a wide-shaped example file where the data doesn't
#start on the first row.
#Don't worry about how it works - when working with real growth
#curves data, this file would be created by the plate reader
temp_example_widedata <- example_widedata_noiseless
colnames(temp_example_widedata) <- paste("V", 1:ncol(temp_example_widedata),
                                         sep = "")
modified_example_widedata <-
  rbind(
    as.data.frame(matrix("", nrow = 4, ncol = ncol(example_widedata_noiseless))),
    colnames(example_widedata_noiseless),
    temp_example_widedata)
modified_example_widedata[1:2, 1:2] <- 
  c("Experiment name", "Start date", "Experiment_1", as.character(Sys.Date()))

write.table(modified_example_widedata, file = "widedata.csv", 
          row.names = FALSE, col.names = FALSE, sep = ",")
write.table(modified_example_widedata, file = "widedata2.csv", 
          row.names = FALSE, col.names = FALSE, sep = ",")

## -----------------------------------------------------------------------------
#Let's take a peek at what this file looks like
print_df(head(read.csv("widedata.csv", header = FALSE, 
                       colClasses = "character"), 
              c(10, 10)))

## -----------------------------------------------------------------------------
imported_widedata <- read_wides(files = "widedata.csv", startrow = 5)

## -----------------------------------------------------------------------------
head(imported_widedata, c(6, 10))

## -----------------------------------------------------------------------------
imported_widedata <- read_wides(files = "widedata.csv",
                                startrow = 5, startcol = "A")

## -----------------------------------------------------------------------------
#If we had multiple wide-shaped data files to import
imported_widedata <- read_wides(files = c("widedata.csv", "widedata2.csv"))

## -----------------------------------------------------------------------------
imported_widedata <- read_wides(files = "widedata.csv",
                                startrow = 5,
                                metadata = list("experiment_name" = c(1, 2),
                                                "start_date" = c(2, 2)))
head(imported_widedata, c(6, 8))

## -----------------------------------------------------------------------------
imported_widedata <- read_wides(files = "widedata.csv",
                                startrow = 5,
                                metadata = list("experiment_name" = c(1, "B"),
                                                "start_date" = c(2, "B")))

## -----------------------------------------------------------------------------
imported_blocks_now_tidy <- trans_wide_to_tidy(
  wides = imported_blockdata,
  id_cols = c("block_name", "time"))

imported_wides_now_tidy <- trans_wide_to_tidy(
  wides = imported_widedata,
  id_cols = c("file", "experiment_name", "start_date", "Time"))

print(head(imported_blocks_now_tidy), row.names = FALSE)

