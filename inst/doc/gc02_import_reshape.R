## ----global options, include = FALSE------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(root.dir = tempdir())

## ----setup--------------------------------------------------------------------
library(gcplyr)

## -----------------------------------------------------------------------------
temp_filenames <- make_example(vignette = 2, example = 1)

## -----------------------------------------------------------------------------
# Here we print all the files we're going to read
list.files(pattern = "Plate1.*csv")

# Here we save them to the temp_filenames variable
temp_filenames <- list.files(pattern = "Plate1.*csv")

## -----------------------------------------------------------------------------
print_df(read.csv(temp_filenames[1], header = FALSE, colClasses = "character"))

## -----------------------------------------------------------------------------
# Now let's read it with import_blockmeasures
imported_blockdata <- import_blockmeasures(
  files = temp_filenames, startrow = 4)

head(imported_blockdata, c(6, 8))

## -----------------------------------------------------------------------------
# We can specify rows or columns by Excel-style letters too
imported_blockdata <- import_blockmeasures(
  files = temp_filenames,
  startrow = 4, startcol = "A")

## -----------------------------------------------------------------------------
print_df(read.csv(temp_filenames[1], header = FALSE, colClasses = "character"))

## -----------------------------------------------------------------------------
# Reading the blockcurves files with metadata included
imported_blockdata <- import_blockmeasures(
  files = temp_filenames,
  startrow = 4,
  metadata = list("time" = c(2, 3)))

head(imported_blockdata, c(6, 8))

## -----------------------------------------------------------------------------
# Reading the blockcurves files with metadata included
imported_blockdata <- import_blockmeasures(
  files = temp_filenames,
  startrow = 4,
  metadata = list("time" = c(2, "C")))

## -----------------------------------------------------------------------------
make_example(vignette = 2, example = 2)

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
make_example(vignette = 2, example = 3)

## -----------------------------------------------------------------------------
# Let's take a peek at what this file looks like
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

