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
# This code was previously explained
# Here we're re-running it so it's available for us to work with
example_tidydata <- trans_wide_to_tidy(example_widedata_noiseless,
                                       id_cols = "Time")
ex_dat_mrg <- merge_dfs(example_tidydata, example_design_tidy)
ex_dat_mrg$Well <- 
  factor(ex_dat_mrg$Well,
         levels = paste(rep(LETTERS[1:8], each = 12), 1:12, sep = ""))
ex_dat_mrg$Time <- ex_dat_mrg$Time/3600 #Convert time to hours
ex_dat_mrg <-
  mutate(group_by(ex_dat_mrg, Well, Bacteria_strain, Phage),
         deriv = calc_deriv(x = Time, y = Measurements),
         deriv_percap5 = calc_deriv(x = Time, y = Measurements, 
                                        percapita = TRUE, blank = 0,
                                        window_width_n = 5, trans_y = "log"),
         doub_time = doubling_time(y = deriv_percap5))
sample_wells <- c("A1", "F1", "F10", "E11")
# Drop unneeded columns (optional, but makes things cleaner)
ex_dat_mrg <- dplyr::select(ex_dat_mrg,
                            Time, Well, Measurements, Bacteria_strain, Phage,
                            deriv, deriv_percap5)

## ----include = FALSE----------------------------------------------------------
# Here we're only keeping the wells that at one point or another in
# this vignette we visualize. This cuts down on vignette build time
# with no visual indication of the change
ex_dat_mrg <- dplyr::filter(ex_dat_mrg, Well %in% c("A1", "A7", "B4", "B10", 
                                                    "B5", "B11", "F1", "E11", 
                                                    "F10", "A4", "E2", "H8"))

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <- 
  summarize(group_by(ex_dat_mrg, Bacteria_strain, Phage, Well),
            lag_time = lag_time(y = Measurements, x = Time, 
                                deriv = deriv_percap5, blank = 0),
            max_percap = max_gc(deriv_percap5),
            max_percap_time = Time[which_max_gc(deriv_percap5)],
            max_percap_dens = Measurements[which_max_gc(deriv_percap5)],
            min_dens = min_gc(Measurements))
head(ex_dat_mrg_sum)

ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = log(Measurements))) +
  geom_point() +
  facet_wrap(~Well) +
  geom_abline(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells),
              color = "red",
              aes(slope = max_percap,
                  intercept = log(max_percap_dens) - max_percap*max_percap_time)) +
  geom_vline(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells),
             aes(xintercept = lag_time), lty = 2) +
  geom_hline(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells),
             aes(yintercept = log(min_dens)))


## -----------------------------------------------------------------------------
ex_dat_mrg_sum <- 
  summarize(group_by(ex_dat_mrg, Bacteria_strain, Phage, Well),
            min_dens = first_minima(Measurements, return = "y"),
            lag_time = lag_time(y = Measurements, x = Time, 
                                deriv = deriv_percap5, blank = 0, 
                                y0 = min_dens),
            max_percap = max_gc(deriv_percap5),
            max_percap_time = Time[which_max_gc(deriv_percap5)],
            max_percap_dens = Measurements[which_max_gc(deriv_percap5)])
head(ex_dat_mrg_sum)

ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = log(Measurements))) +
  geom_point() +
  facet_wrap(~Well) +
  geom_abline(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells),
              color = "red",
              aes(slope = max_percap,
                  intercept = log(max_percap_dens) - max_percap*max_percap_time)) +
  geom_vline(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells),
             aes(xintercept = lag_time), lty = 2) +
  geom_hline(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells),
             aes(yintercept = log(min_dens)))


## -----------------------------------------------------------------------------
ex_dat_mrg_sum <- 
  summarize(group_by(ex_dat_mrg, Bacteria_strain, Phage, Well),
            max_percap = max_gc(deriv_percap5, na.rm = TRUE),
            max_percap_time = extr_val(Time, which_max_gc(deriv_percap5)),
            doub_time = doubling_time(y = max_percap))
head(ex_dat_mrg_sum)

ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv_percap5)) +
  geom_line() +
  facet_wrap(~Well) +
  geom_point(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells), 
             aes(x = max_percap_time, y = max_percap),
             size = 2, color = "red") +
  coord_cartesian(ylim = c(-1, NA))

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <- 
  summarize(group_by(ex_dat_mrg, Bacteria_strain, Phage, Well),
            max_dens = max_gc(Measurements, na.rm = TRUE),
            max_time = extr_val(Time, which_max_gc(Measurements)))
head(ex_dat_mrg_sum)

ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements)) +
  geom_line() +
  facet_wrap(~Well) +
  geom_point(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells), 
             aes(x = max_time, y = max_dens),
             size = 2, color = "red")

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <-
  summarize(group_by(ex_dat_mrg, Bacteria_strain, Phage, Well),
            auc = auc(x = Time, y = Measurements))
head(ex_dat_mrg_sum)

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <- 
  summarize(group_by(ex_dat_mrg, Bacteria_strain, Phage, Well),
            min_dens = min_gc(Measurements, na.rm = TRUE),
            min_time = extr_val(Time, which_min_gc(Measurements)))
head(ex_dat_mrg_sum)

ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements)) +
  geom_line() +
  facet_wrap(~Well) +
  geom_point(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells), 
             aes(x = min_time, y = min_dens),
             size = 2, color = "red")

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <- 
  summarize(group_by(ex_dat_mrg, Bacteria_strain, Phage, Well),
            min_dens = first_minima(y = Measurements, x = Time, return = "y"),
            min_time = first_minima(y = Measurements, x = Time, return = "x"))
head(ex_dat_mrg_sum)

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <- 
  summarize(group_by(ex_dat_mrg, Bacteria_strain, Phage, Well),
            above_01 = first_above(y = Measurements, x = Time, 
                                   threshold = 0.1, return = "x"))
head(ex_dat_mrg_sum)

ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements)) +
  geom_line() +
  facet_wrap(~Well) +
  geom_vline(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells), 
             aes(xintercept = above_01), lty = 2, color = "red")

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <- 
  summarize(group_by(ex_dat_mrg, Bacteria_strain, Phage, Well),
            percap_above_1 = first_above(y = deriv_percap5, x = Time, 
                                   threshold = 1, return = "x"))
head(ex_dat_mrg_sum)

ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv_percap5)) +
  geom_line() +
  facet_wrap(~Well) +
  geom_vline(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells),
             aes(xintercept = percap_above_1), lty = 2, color = "red") +
  coord_cartesian(ylim = c(-1, NA))

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <- 
  summarize(group_by(ex_dat_mrg, Bacteria_strain, Phage, Well),
            mid_point = first_above(y = Measurements, x = Time, return = "x",
                                    threshold = max_gc(Measurements)/2),
            infl_point = extr_val(Time, which_max_gc(deriv)))
head(ex_dat_mrg_sum)

ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements)) +
  geom_line() +
  facet_wrap(~Well) +
  geom_vline(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells),
             aes(xintercept = mid_point), lty = 2, color = "red") +
  geom_vline(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells),
             aes(xintercept = infl_point), lty = 2, color = "blue")

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <-
  summarize(group_by(ex_dat_mrg, Bacteria_strain, Phage, Well),
            centr_x = centroid_x(x = Time, y = Measurements),
            centr_y = centroid_y(x = Time, y = Measurements))
head(ex_dat_mrg_sum)

ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements)) +
  geom_line() +
  facet_wrap(~Well) +
  geom_point(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells), 
             aes(x = centr_x, y = centr_y))

## -----------------------------------------------------------------------------
nophage_wells <- c("A1", "A4", "E2", "F1")
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% nophage_wells),
       aes(x = Time, y = Measurements)) +
  geom_line() +
  facet_wrap(~Well, scales = "free")

## -----------------------------------------------------------------------------
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% nophage_wells),
       aes(x = Time, y = deriv)) +
  geom_line() +
  facet_wrap(~Well, scales = "free")

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <-
  summarize(group_by(ex_dat_mrg, Bacteria_strain, Phage, Well),
    diauxie_time = find_local_extrema(x = Time, y = deriv, return = "x",
                                   return_maxima = FALSE, return_minima = TRUE,
                                   window_width_n = 39)[2],
    diauxie_idx = find_local_extrema(x = Time, y = deriv, return = "index",
                                   return_maxima = FALSE, return_minima = TRUE,
                                   window_width_n = 39)[2],
    diauxie_dens = extr_val(Measurements, diauxie_idx))
head(ex_dat_mrg_sum)

# Plot data with a point at the moment of diauxic shift
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% nophage_wells),
       aes(x = Time, y = Measurements)) +
  geom_line() +
  facet_wrap(~Well, scales = "free") +
  geom_point(data = dplyr::filter(ex_dat_mrg_sum, Well %in% nophage_wells), 
             aes(x = diauxie_time, y = diauxie_dens),
             size = 2, color = "red")

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <-
  summarize(
    group_by(ex_dat_mrg, Bacteria_strain, Phage, Well),
    diauxie_time = 
      find_local_extrema(x = Time, y = deriv, return = "x",
                         return_maxima = FALSE, return_minima = TRUE,
                         window_width_n = 39)[2],
    diauxie_percap = max_gc(deriv_percap5[Time >= diauxie_time]),
    diauxie_percap_time = 
      extr_val(Time[Time >= diauxie_time],
               which_max_gc(deriv_percap5[Time >= diauxie_time]))
  )
head(ex_dat_mrg_sum)

# Plot data with a point at the moment of peak diauxic growth rate
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% nophage_wells),
       aes(x = Time, y = deriv_percap5)) +
  geom_line() +
  facet_wrap(~Well, scales = "free") +
  geom_point(data = dplyr::filter(ex_dat_mrg_sum, Well %in% nophage_wells), 
             aes(x = diauxie_percap_time, y = diauxie_percap),
             size = 2, color = "red")

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <-
  summarize(group_by(ex_dat_mrg, Bacteria_strain, Phage, Well),
            first_maxima_x = first_maxima(x = Time, y = Measurements, 
                                          return = "x"),
            first_maxima_y = first_maxima(x = Time, y = Measurements, 
                                          return = "y"))
head(ex_dat_mrg_sum)

ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells), 
       aes(x = Time, y = Measurements)) +
  geom_line() +
  facet_wrap(~Well) +
  geom_point(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells), 
             aes(x = first_maxima_x, y = first_maxima_y), 
             color = "red", size = 1.5)

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <-
  summarize(
    group_by(ex_dat_mrg, Bacteria_strain, Phage, Well),
    extin_time = first_below(x = Time, y = Measurements, threshold = 0.15,
                             return = "x", return_endpoints = FALSE))
head(ex_dat_mrg_sum)

phage_wells <- c("A7", "B10", "F10", "H8")
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% phage_wells),
       aes(x = Time, y = Measurements)) +
  geom_line() +
  facet_wrap(~Well) +
  geom_vline(data = dplyr::filter(ex_dat_mrg_sum, Well %in% phage_wells),
             aes(xintercept = extin_time), lty = 2)

