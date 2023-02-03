## ----global options, include = FALSE------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(root.dir = tempdir())

#Note: to knit to pdf, word, and html simultaneously, run in R console:
#{rmarkdown::render("./vignettes/preprocess_plot.Rmd", rmarkdown::html_vignette(toc = TRUE, toc_dept = 4)); rmarkdown::render("./vignettes/preprocess_plot.Rmd", rmarkdown::pdf_document(toc = TRUE, toc_depth = 4)); rmarkdown::render("./vignettes/preprocess_plot.Rmd", rmarkdown::word_document(toc = TRUE, toc_dept = 4))}

## ----setup--------------------------------------------------------------------
library(gcplyr)

library(dplyr)
library(ggplot2)
library(lubridate)

## ---- include = FALSE---------------------------------------------------------
print_df <- function(df, col.names = FALSE) {
  write.table(format(df, justify = "right"),
              row.names=FALSE, col.names = col.names, quote = F)
}

## -----------------------------------------------------------------------------
#This code was previously explained
#Here we're re-running it so it's available for us to work with
example_tidydata <- trans_wide_to_tidy(example_widedata_noiseless,
                                       id_cols = "Time")
example_design <- make_design(
  pattern_split = ",", nrows = 8, ncols = 12,
  "Bacteria_strain" = make_designpattern(
    values = paste("Strain", 1:48),
    rows = 1:8, cols = 1:6, pattern = 1:48, byrow = TRUE),
  "Bacteria_strain" = make_designpattern(
    values = paste("Strain", 1:48),
    rows = 1:8, cols = 7:12, pattern = 1:48, byrow = TRUE),
  "Phage" = make_designpattern(
    values = c("No Phage"), rows = 1:8, cols = 1:6, pattern = "1"),
  "Phage" = make_designpattern(
    values = c("Phage Added"), rows = 1:8, cols = 7:12, pattern = "1"))
ex_dat_mrg <- merge_dfs(example_tidydata, example_design)

## -----------------------------------------------------------------------------
example_data_and_designs_filtered <- filter(ex_dat_mrg, Well != "B1")
head(example_data_and_designs_filtered)

## -----------------------------------------------------------------------------
example_data_and_designs_filtered <- 
  filter(ex_dat_mrg, 
         Well != "B1", Bacteria_strain != "Strain 13")
head(example_data_and_designs_filtered)

## -----------------------------------------------------------------------------
ex_dat_mrg$Time <-
  paste(ex_dat_mrg$Time %/% 3600,
        formatC((ex_dat_mrg$Time %% 3600) %/% 60, 
                width = 2, flag = 0),
        formatC((ex_dat_mrg$Time %% 3600) %% 60,
                width = 2, flag = 0),
        sep = ":")

## -----------------------------------------------------------------------------
head(ex_dat_mrg)

## -----------------------------------------------------------------------------
#We have previously loaded lubridate, but if you haven't already then
#make sure to add the line:
#   library(lubridate)

ex_dat_mrg$Time <- time_length(hms(ex_dat_mrg$Time))

head(ex_dat_mrg)

## -----------------------------------------------------------------------------
#We have previously loaded ggplot2, but if you haven't already then
#make sure to add the line:
#    library(ggplot2)

#First, we'll reorder the Well levels so they plot in the correct order
ex_dat_mrg$Well <- 
  factor(ex_dat_mrg$Well,
         levels = paste(rep(LETTERS[1:8], each = 12), 1:12, sep = ""))

ggplot(data = ex_dat_mrg, aes(x = Time, y = Measurements)) +
  geom_line() +
  facet_wrap(~Well, nrow = 8, ncol = 12)

