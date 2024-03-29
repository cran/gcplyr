## ----global options, include = FALSE------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(root.dir = tempdir())

## ----setup--------------------------------------------------------------------
library(gcplyr)

## -----------------------------------------------------------------------------
my_design_blk <- make_design(
  output_format = "blocks",
  nrows = 8, ncols = 12, 
  Bacteria = list(c("Str1", "Str2", "Str3", "Str4", "Str5", "Str6"),
                  2:7,
                  2:11,
                  "123456",
                  FALSE)
)

## -----------------------------------------------------------------------------
my_design_blk

## -----------------------------------------------------------------------------
my_design_blk <- make_design(
  output_format = "blocks",
  nrows = 8, ncols = 12, 
  Bacteria = list(c("Str1", "Str2", "Str3", 
                    "Str4", "Str5", "Str6"),
                  2:7,
                  2:11,
                  "123056",
                  FALSE)
)
my_design_blk

## -----------------------------------------------------------------------------
my_design_blk <- make_design(
  output_format = "blocks",
  nrows = 8, ncols = 12, lookup_tbl_start = "A",
  Bacteria = list(
    c("Str1", "Str2", "Str3", "Str4", "Str5", "Str6"),
    2:7,
    2:11,
    "ABCDEF",
    FALSE)
)

## -----------------------------------------------------------------------------
my_design_blk <- make_design(
  output_format = "blocks",
  nrows = 8, ncols = 12,
  Bacteria = list(
    c("Str1", "Str2", "Str3", "Str4", "Str5", "Str6"),
    2:7,
    2:11,
    c(1,2,3,4,5,6),
    FALSE)
)

## -----------------------------------------------------------------------------
my_design_blk <- make_design(
  output_format = "blocks",
  nrows = 8, ncols = 12, lookup_tbl_start = "a",
  Bacteria = list(c("Str1", "Str2", "Str3", 
                    "Str4", "Str5", "Str6"),
                  2:7,
                  2:11,
                  "abcdef",
                  FALSE),
  Media = list(c("Med1", "Med2", "Med3",
                 "Med4", "Med5", "Med6",
                 "Med7", "Med8", "Med9",
                 "Med10", "Med11", "Med12"),
               2:7,
               2:11,
               "abcdefghij")
  )

my_design_blk

## -----------------------------------------------------------------------------
my_design_blk <- make_design(
  output_format = "blocks",
  nrows = 8, ncols = 12, lookup_tbl_start = "a",
  Bacteria = list(c("Str1", "Str2"),
                  2:7,
                  2:11,
                  "abaaabbbab",
                  FALSE),
  Media = list(c("Med1", "Med2", "Med3"),
               2:7,
               2:11,
               "aabbbc000abc"))

my_design_blk

## -----------------------------------------------------------------------------
my_design_blk <- make_design(
  output_format = "blocks",
  nrows = 8, ncols = 12, lookup_tbl_start = "a",
  Bacteria = mdp(
    values = c("Str1", "Str2", "Str3", 
               "Str4", "Str5", "Str6"),
    rows = 2:7, cols = 2:11, pattern = "abc0ef",
    byrow = FALSE),
  Media = mdp(
    values = c("Med1", "Med2", "Med3",
               "Med4", "Med5", "Med6",
               "Med7", "Med8", "Med9",
               "Med10", "Med11", "Med12"),
    rows = 2:7, cols = 2:11, pattern = "abcde0ghij"))

my_design_blk

## -----------------------------------------------------------------------------
my_design_tdy <- make_design(
  output_format = "tidy",
  nrows = 8, ncols = 12, lookup_tbl_start = "a",
  Bacteria = mdp(
    values = c("Str1", "Str2", "Str3", 
               "Str4", "Str5", "Str6"),
    rows = 2:7, cols = 2:11, pattern = "abc0ef",
    byrow = FALSE),
  Media = mdp(
    values = c("Med1", "Med2", "Med3",
               "Med4", "Med5", "Med6",
               "Med7", "Med8", "Med9",
               "Med10", "Med11", "Med12"),
    rows = 2:7, cols = 2:11, pattern = "abcde0ghij"))

head(my_design_tdy, 20)

## -----------------------------------------------------------------------------
#See the previous section where we created my_design_tdy
write.csv(x = my_design_tdy, file = "tidy_design.csv",
          row.names = FALSE)

## -----------------------------------------------------------------------------
# See the previous section where we created my_design_blk
write_blocks(my_design_blk, file = NULL)

# Let's see what the files look like
print_df(read.csv("Bacteria.csv", header = FALSE, colClasses = "character"))

print_df(read.csv("Media.csv", header = FALSE, colClasses = "character"))

## -----------------------------------------------------------------------------
# See the previous section where we created my_design_blk
write_blocks(my_design_blk, file = "Design.csv", output_format = "single")

# Let's see what the file looks like
print_df(read.csv("Design.csv", header = FALSE, colClasses = "character"))

