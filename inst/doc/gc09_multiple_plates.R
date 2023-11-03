## ----global options, include = FALSE------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(root.dir = tempdir())

## ----setup--------------------------------------------------------------------
library(gcplyr)

library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
mylist <- list("A", c(5, 6, 7), matrix(1:9, nrow = 3))
mylist

## -----------------------------------------------------------------------------
mylist[[1]]
mylist[[2]]
mylist[[3]]

## -----------------------------------------------------------------------------
mylist[[3]] <- mylist[[3]] + 7
mylist[[3]]

## -----------------------------------------------------------------------------
filenames_sep <- make_example(vignette = 9, example = 1, dir = "./example_data")
head(filenames_sep)

## -----------------------------------------------------------------------------
plates <- list(
  plate1 = import_blockmeasures(
    list.files(path = "./example_data/", pattern = "Plate1", full.names = TRUE),
    startrow = 4,
    metadata = list(Time = c(2, "C"))),
  plate2 = import_blockmeasures(
    list.files(path = "./example_data/", pattern = "Plate2", full.names = TRUE),
    startrow = 4,
    metadata = list(Time = c(2, "C"))))

## -----------------------------------------------------------------------------
filenames_mixed <- make_example(vignette = 9, example = 2, dir = "./example_data")
head(filenames_mixed)

## -----------------------------------------------------------------------------
plates <- import_blockmeasures(
    filenames_mixed,
    startrow = 4,
    metadata = list(Time = c(2, "C")),
    num_plates = 2)

## -----------------------------------------------------------------------------
make_example(vignette = 9, example = 3)

plates <- read_wides(files = c("widedata.csv", "widedata2.csv"),
           startrow = 5,
           metadata = list(Experiment = c(1, "B"),
                           Start_date = c(2, "B")))

## -----------------------------------------------------------------------------
tidy_plates <- 
  trans_wide_to_tidy(plates,
                     id_cols = c("file", "Experiment", "Start_date", "Time"))

## -----------------------------------------------------------------------------
tidy_plates_collapsed <- merge_dfs(tidy_plates, collapse = TRUE)

print_df(head(tidy_plates_collapsed), col.names = TRUE)

## -----------------------------------------------------------------------------
example_design1 <- make_design(
  pattern_split = ",", nrows = 8, ncols = 12,
  "Bacteria_strain" = make_designpattern(
    values = paste("Strain", 1:48),
    rows = 1:8, cols = 1:6, pattern = 1:48, byrow = TRUE),
  "Bacteria_strain" = make_designpattern(
    values = paste("Strain", 1:48),
    rows = 1:8, cols = 7:12, pattern = 1:48, byrow = TRUE))
example_design2 <- make_design(
  pattern_split = ",", nrows = 8, ncols = 12,
  "Bacteria_strain" = make_designpattern(
    values = paste("Strain", 49:96),
    rows = 1:8, cols = 1:6, pattern = 1:48, byrow = TRUE),
  "Bacteria_strain" = make_designpattern(
    values = paste("Strain", 49:96),
    rows = 1:8, cols = 7:12, pattern = 1:48, byrow = TRUE))

tidy_plates[[1]] <- merge_dfs(tidy_plates[[1]], example_design1)
tidy_plates[[2]] <- merge_dfs(tidy_plates[[2]], example_design2)

## -----------------------------------------------------------------------------
data_and_designs <- merge_dfs(tidy_plates, collapse = TRUE)
print_df(head(data_and_designs))

## -----------------------------------------------------------------------------
example_design <- make_design(
  pattern_split = ",", nrows = 8, ncols = 12,
  "Bacteria_strain" = make_designpattern(
    values = paste("Strain", 1:48),
    rows = 1:8, cols = 1:6, pattern = 1:48, byrow = TRUE),
  "Bacteria_strain" = make_designpattern(
    values = paste("Strain", 1:48),
    rows = 1:8, cols = 7:12, pattern = 1:48, byrow = TRUE))

data_and_designs <- merge_dfs(tidy_plates, example_design, collapse = TRUE)
print_df(head(data_and_designs))

