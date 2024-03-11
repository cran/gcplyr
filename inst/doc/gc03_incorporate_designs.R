## ----global options, include = FALSE------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(root.dir = tempdir())

## ----setup--------------------------------------------------------------------
library(gcplyr)

## -----------------------------------------------------------------------------
make_example(vignette = 3, example = 1, dir = ".")

## -----------------------------------------------------------------------------
print_df(read.csv("mydesign.csv", header = FALSE, colClasses = "character"))

## -----------------------------------------------------------------------------
my_design <- import_blockdesigns(files = "mydesign.csv", 
                                 block_names = "Treatment_numbers")
head(my_design, 20)

## -----------------------------------------------------------------------------
make_example(vignette = 3, example = 1, dir = ".")
make_example(vignette = 3, example = 2, dir = ".")

## -----------------------------------------------------------------------------
print_df(read.csv("mydesign.csv", header = FALSE, colClasses = "character"))
print_df(read.csv("mydesign2.csv", header = FALSE, colClasses = "character"))

## -----------------------------------------------------------------------------
my_design <- 
  import_blockdesigns(files = c("mydesign.csv", "mydesign2.csv"), 
                      block_names = c("Treatment_numbers", "Strain_letters"))
head(my_design, 20)

## -----------------------------------------------------------------------------
make_example(vignette = 3, example = 3, dir = ".")

#Print what the file looks like
print_df(read.csv("mydesign_sep.csv", header = FALSE, colClasses = "character"))

#Read in the designs
my_design <- 
  import_blockdesigns(files = c("mydesign_sep.csv"), 
                      block_names = c("Treatment_numbers", "Strain_letters"),
                      startrow = c(1, 11), endrow = c(9, 19))
head(my_design, 20)

## -----------------------------------------------------------------------------
make_example(vignette = 3, example = 4, dir = ".")

## -----------------------------------------------------------------------------
print_df(read.csv("mydesign_pasted.csv", header = FALSE, colClasses = "character")[, 1:10])

## -----------------------------------------------------------------------------
my_design <- 
  import_blockdesigns(files = "mydesign_pasted.csv", 
                      into = c("Treatment_numbers", "Strain_letters"),
                      sep = "_")
head(my_design, 20)

## -----------------------------------------------------------------------------
example_tidydata <- trans_wide_to_tidy(example_widedata_noiseless,
                                       id_cols = "Time")

## -----------------------------------------------------------------------------
example_design <- example_design_tidy
head(example_design_tidy)

## -----------------------------------------------------------------------------
ex_dat_mrg <- merge_dfs(example_tidydata, example_design)

head(ex_dat_mrg)

