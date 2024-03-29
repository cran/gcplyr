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
library(lubridate)

## -----------------------------------------------------------------------------
# This code was previously explained
# Here we're re-running it so it's available for us to work with
example_tidydata <- trans_wide_to_tidy(example_widedata_noiseless,
                                       id_cols = "Time")
ex_dat_mrg <- merge_dfs(example_tidydata, example_design_tidy)

## -----------------------------------------------------------------------------
example_data_and_designs_filtered <- 
  filter(ex_dat_mrg, 
         Well != "B1", Bacteria_strain != "Strain 13")
head(example_data_and_designs_filtered)

## -----------------------------------------------------------------------------
ex_dat_mrg <- make_example(vignette = 4, example = 1)

head(ex_dat_mrg)

## -----------------------------------------------------------------------------
# We have previously loaded lubridate, but if you haven't already then
# make sure to add the line:
#    library(lubridate)

ex_dat_mrg$Time <- time_length(hms(ex_dat_mrg$Time), unit = "hour")

head(ex_dat_mrg)

## -----------------------------------------------------------------------------
# We have previously loaded ggplot2, but if you haven't already then
# make sure to add the line:
#     library(ggplot2)

# First, we'll reorder the Well levels so they plot in the correct order
ex_dat_mrg$Well <- 
  factor(ex_dat_mrg$Well,
         levels = paste0(rep(LETTERS[1:8], each = 12), 1:12))

ggplot(data = ex_dat_mrg, aes(x = Time, y = Measurements)) +
  geom_line() +
  facet_wrap(~Well, nrow = 8, ncol = 12)

