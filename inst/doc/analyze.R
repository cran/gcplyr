## ----global options, include = FALSE------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(root.dir = tempdir())

#Note: to knit to pdf, word, and html simultaneously, run in R console:
#{rmarkdown::render("./vignettes/analyze.Rmd", rmarkdown::html_vignette(toc = TRUE, toc_dept = 4)); rmarkdown::render("./vignettes/analyze.Rmd", rmarkdown::pdf_document(toc = TRUE, toc_depth = 4)); rmarkdown::render("./vignettes/analyze.Rmd", rmarkdown::word_document(toc = TRUE, toc_dept = 4))}

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
ex_dat_mrg <- group_by(ex_dat_mrg, Well, Bacteria_strain, Phage)
ex_dat_mrg <-
  mutate(ex_dat_mrg,
         deriv = calc_deriv(x = Time, y = Measurements, x_scale = 3600),
         deriv_percap5 = calc_deriv(x = Time, y = Measurements, 
                                        percapita = TRUE, blank = 0,
                                        window_width_n = 5, trans_y = "log",
                                    x_scale = 3600))
sample_wells <- c("A1", "F1", "F10", "E11")

## ---- include = FALSE---------------------------------------------------------
#Here we're only keeping the wells that at one point or another in
#this vignette we visualize. This cuts down on vignette build time
#with no visual indication of the change
ex_dat_mrg <- dplyr::filter(ex_dat_mrg, Well %in% c("A1", "A7", "B4", "B10", 
                                                    "B5", "B11", "F1", "E11", 
                                                    "F10", "A4", "E2", "H8"))

## -----------------------------------------------------------------------------
#First, drop unneeded columns (optional)
ex_dat_mrg <- dplyr::select(ex_dat_mrg,
                            Time, Well, Measurements, Bacteria_strain, Phage,
                            deriv, deriv_percap5)
#Then, carry out grouping
ex_dat_mrg <- group_by(ex_dat_mrg, Bacteria_strain, Phage, Well)

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <- summarize(ex_dat_mrg,
                            max_dens = max(Measurements, na.rm = TRUE))
head(ex_dat_mrg_sum)

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <- summarize(ex_dat_mrg,
                            max_dens = max(Measurements, na.rm = TRUE),
                            max_time = Time[which.max(Measurements)])
head(ex_dat_mrg_sum)

## -----------------------------------------------------------------------------
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements)) +
  geom_line() +
  facet_wrap(~Well) +
  geom_hline(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells), 
             aes(yintercept = max_dens), lty = 2) +
  geom_vline(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells), 
             aes(xintercept = max_time), lty = 2)

## -----------------------------------------------------------------------------
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements)) +
  geom_line() +
  facet_wrap(~Well) +
  geom_point(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells), 
             aes(x = max_time, y = max_dens),
             size = 2, color = "red")

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <- summarize(ex_dat_mrg,
                            max_percap = max(deriv_percap5, na.rm = TRUE),
                            max_percap_time = Time[which.max(deriv_percap5)])
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv_percap5)) +
  geom_line() +
  facet_wrap(~Well) +
  geom_point(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells), 
             aes(x = max_percap_time, y = max_percap),
             size = 2, color = "red")

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <-
  summarize(ex_dat_mrg,
            auc = auc(x = Time, y = Measurements))
head(ex_dat_mrg_sum)

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <-
  summarize(ex_dat_mrg,
            first_peak_x = first_peak(x = Time, y = Measurements, return = "x"),
            first_peak_y = first_peak(x = Time, y = Measurements, return = "y"))

head(ex_dat_mrg_sum)

## -----------------------------------------------------------------------------
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells), 
       aes(x = Time, y = Measurements)) +
  geom_line() +
  facet_wrap(~Well, nrow = 8, ncol = 12) +
  geom_point(data = ex_dat_mrg_sum, 
             aes(x = first_peak_x, y = first_peak_y), 
             color = "red", size = 1.5)

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <-
  summarize(ex_dat_mrg,
            max_growth_rate = first_peak(x = Time, y = deriv_percap5, 
                                         return = "y"),
            lag_time = first_peak(x = Time, y = deriv_percap5, 
                                  return = "x"))

head(ex_dat_mrg_sum)

ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv_percap5)) +
  geom_line() +
  facet_wrap(~Well, scales = "free") +
  geom_point(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells),
             aes(x = lag_time, y = max_growth_rate),
             color = "red", size = 2)

## -----------------------------------------------------------------------------
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv)) +
  geom_line() +
  facet_wrap(~Well, scales = "free")

## -----------------------------------------------------------------------------
sample_wells <- c("A1", "A4", "E2", "F1")
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv)) +
  geom_line() +
  facet_wrap(~Well, scales = "free")

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <-
  summarize(ex_dat_mrg,
    diauxie_time = find_local_extrema(x = Time, y = deriv, return = "x",
                                   return_maxima = FALSE, return_minima = TRUE,
                                   window_width_n = 39)[2])

#Plot data with vertical line at detected diauxie
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv)) +
  geom_line() +
  facet_wrap(~Well, scales = "free") +
  geom_vline(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells), 
             aes(xintercept = diauxie_time), lty = 2)

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <-
  summarize(
    ex_dat_mrg,
    diauxie_time = find_local_extrema(x = Time, y = deriv, return = "x",
                                   return_maxima = FALSE, return_minima = TRUE,
                                   window_width_n = 39)[2],
    diauxie_idx = find_local_extrema(x = Time, y = deriv, return = "index",
                                   return_maxima = FALSE, return_minima = TRUE,
                                   window_width_n = 39)[2],
    diauxie_dens = Measurements[diauxie_idx])

#Plot data with a point at the moment of diauxic shift
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements)) +
  geom_line() +
  facet_wrap(~Well, scales = "free") +
  geom_point(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells), 
             aes(x = diauxie_time, y = diauxie_dens),
             size = 2, color = "red")

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <-
  summarize(
    ex_dat_mrg,
    diauxie_time = find_local_extrema(x = Time, y = deriv, return = "x",
                                   return_maxima = FALSE, return_minima = TRUE,
                                   window_width_n = 39)[2],
    diauxie_percap = max(deriv_percap5[Time >= diauxie_time], na.rm = TRUE),
    diauxie_percap_time = 
      Time[Time >= diauxie_time][
        which.max(deriv_percap5[Time >= diauxie_time])]
    )

#Plot data with a point at the moment of peak diauxic growth rate
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv_percap5)) +
  geom_line() +
  facet_wrap(~Well, scales = "free") +
  geom_point(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells), 
             aes(x = diauxie_percap_time, y = diauxie_percap),
             size = 2, color = "red")

## -----------------------------------------------------------------------------
sample_wells <- c("A7", "B10", "F10", "H8")

ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements)) +
  geom_line() +
  facet_wrap(~Well)

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <-
  summarize(
    ex_dat_mrg,
    extin_time = first_below(x = Time, y = Measurements, threshold = 0.15,
                             return = "x", return_endpoints = FALSE))
head(ex_dat_mrg_sum)

ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements)) +
  geom_line() +
  facet_wrap(~Well) +
  geom_vline(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells),
             aes(xintercept = extin_time), lty = 2)

## -----------------------------------------------------------------------------
sample_wells <- c("A1", "F1", "F10", "E11")
ex_dat_mrg_sum <-
  summarize(
    ex_dat_mrg,
    time_to_01 = find_threshold_crosses(x = Time, y = Measurements, 
                                        threshold = 0.1, return = "x", 
                                        return_endpoints = TRUE, 
                                        return_falling = FALSE)[1],
    time_to_05 = find_threshold_crosses(x = Time, y = Measurements, 
                                        threshold = 0.5, return = "x", 
                                        return_endpoints = TRUE, 
                                        return_falling = FALSE)[1])
head(ex_dat_mrg_sum)

ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements)) +
  geom_line() +
  facet_wrap(~Well) +
  geom_vline(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells),
             aes(xintercept = time_to_01), lty = 2) +
  geom_vline(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells),
             aes(xintercept = time_to_05), lty = 2)

