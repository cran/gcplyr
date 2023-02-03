## ----global options, include = FALSE------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(root.dir = tempdir())

#Note: to knit to pdf, word, and html simultaneously, run in R console:
#{rmarkdown::render("./vignettes/process.Rmd", rmarkdown::html_vignette(toc = TRUE, toc_dept = 4)); rmarkdown::render("./vignettes/process.Rmd", rmarkdown::pdf_document(toc = TRUE, toc_depth = 4)); rmarkdown::render("./vignettes/process.Rmd", rmarkdown::word_document(toc = TRUE, toc_dept = 4))}

## ----setup--------------------------------------------------------------------
library(gcplyr)

library(dplyr)
library(ggplot2)

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
ex_dat_mrg$Well <- 
  factor(ex_dat_mrg$Well,
         levels = paste(rep(LETTERS[1:8], each = 12), 1:12, sep = ""))

## -----------------------------------------------------------------------------
ex_dat_mrg <- group_by(ex_dat_mrg, Well, Bacteria_strain, Phage)

head(ex_dat_mrg)

## -----------------------------------------------------------------------------
ex_dat_mrg <-
  mutate(ex_dat_mrg,
         Measurements_plus1 = Measurements+1)

head(ex_dat_mrg)

## -----------------------------------------------------------------------------
ex_dat_mrg <-
  mutate(ex_dat_mrg,
         Measurements_plus1 = Measurements+1,
         Measurements_plus2 = Measurements+2)

head(ex_dat_mrg)

## -----------------------------------------------------------------------------
ex_dat_mrg <- mutate(ex_dat_mrg,
                     deriv = calc_deriv(x = Time, y = Measurements))

## -----------------------------------------------------------------------------
sample_wells <- c("A1", "F1", "F10", "E11")

## ---- include = FALSE---------------------------------------------------------
#For computational speed, let's just keep the wells we'll be focusing on
# (this is hidden from readers bc from this point on we never print out
# the df anyway so there's no difference in the output by filtering here)
ex_dat_mrg <- dplyr::filter(ex_dat_mrg, Well %in% sample_wells)

## -----------------------------------------------------------------------------
#Now let's plot the derivative
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv)) +
  geom_line() +
  facet_wrap(~Well, scales = "free")

## -----------------------------------------------------------------------------
ex_dat_mrg <- mutate(ex_dat_mrg,
                     deriv_percap = calc_deriv(x = Time, y = Measurements,
                                        percapita = TRUE, blank = 0))

#Now let's plot the per-capita derivative
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv_percap)) +
  geom_line() +
  facet_wrap(~Well, scales = "free")

## -----------------------------------------------------------------------------
ex_dat_mrg <- mutate(ex_dat_mrg,
                     deriv_percap5 = calc_deriv(x = Time, y = Measurements, 
                                        percapita = TRUE, blank = 0,
                                        window_width_n = 5))

#Now let's plot the derivative
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv_percap5)) +
  geom_line() +
  facet_wrap(~Well, scales = "free")

## -----------------------------------------------------------------------------
ex_dat_mrg <- mutate(ex_dat_mrg,
                     deriv_percap5 = calc_deriv(x = Time, y = Measurements, 
                                        percapita = TRUE, blank = 0,
                                        window_width_n = 5, trans_y = "log"))

#Now let's plot the derivative
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv_percap5)) +
  geom_line() +
  facet_wrap(~Well, scales = "free")

## -----------------------------------------------------------------------------
ex_dat_mrg <- 
  mutate(ex_dat_mrg,
         deriv_percap_hr = calc_deriv(x = Time, y = Measurements,
                                      percapita = TRUE, blank = 0,
                                      window_width_n = 5, trans_y = "log",
                                      x_scale = 3600))

#Now let's plot the derivative in units of Abs/hour
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv_percap_hr)) +
  geom_line() +
  facet_wrap(~Well, scales = "free")

