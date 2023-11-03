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
library(tidyr)

## -----------------------------------------------------------------------------
# This code was previously explained
# Here we're re-running it so it's available for us to work with
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

## -----------------------------------------------------------------------------
ex_dat_mrg <- make_example(vignette = 7, example = 1)

# Plot with a log y-axis
ggplot(data = dplyr::filter(ex_dat_mrg, noise == "Yes"),
       aes(x = Time, y = Measurements)) +
  geom_point() +
  geom_line(data = dplyr::filter(ex_dat_mrg, noise == "No"),
            lty = 2, color = "red") +
  facet_wrap(~Well) +
  scale_y_continuous(trans = "log10")

## -----------------------------------------------------------------------------
ex_dat_mrg <- 
  mutate(group_by(ex_dat_mrg, Well, Bacteria_strain, Phage, noise),
         deriv_2 = calc_deriv(x = Time, y = Measurements),
         derivpercap_2 = calc_deriv(x = Time, y = Measurements,
                                   percapita = TRUE, blank = 0))

# Plot derivative
ggplot(data = dplyr::filter(ex_dat_mrg, noise == "Yes"),
       aes(x = Time, y = deriv_2)) +
  geom_point() +
  geom_line(data = dplyr::filter(ex_dat_mrg, noise == "No"),
            lty = 2, color = "red") +
  facet_wrap(~Well, scales = "free_y")

# Plot per-capita derivative
ggplot(data = dplyr::filter(ex_dat_mrg, noise == "Yes"),
       aes(x = Time, y = derivpercap_2)) +
  geom_point() +
  geom_line(data = dplyr::filter(ex_dat_mrg, noise == "No"),
            lty = 2, color = "red") +
  facet_wrap(~Well, scales = "free_y")

## -----------------------------------------------------------------------------
ex_dat_mrg <- 
  mutate(group_by(ex_dat_mrg, Well, Bacteria_strain, Phage, noise),
         deriv_5 = calc_deriv(x = Time, y = Measurements,
                            window_width_n = 5),
         derivpercap_5 = calc_deriv(x = Time, y = Measurements,
                                   percapita = TRUE, blank = 0,
                                   window_width_n = 5),
         deriv_9 = calc_deriv(x = Time, y = Measurements,
                            window_width_n = 9),
         derivpercap_9 = calc_deriv(x = Time, y = Measurements,
                                   percapita = TRUE, blank = 0,
                                   window_width_n = 9))
#Reshape our data for plotting purposes
ex_dat_mrg_wide <- 
  pivot_longer(ex_dat_mrg, cols = starts_with("deriv"),
               names_to = c("deriv", "window_width_n"), names_sep = "_")
ex_dat_mrg_wide <- 
  pivot_wider(ex_dat_mrg_wide, names_from = deriv, values_from = value)
                               
#Plot derivative
ggplot(data = dplyr::filter(ex_dat_mrg_wide, noise == "Yes"),
       aes(x = Time, y = deriv)) +
  geom_line(aes(color = window_width_n), lwd = 1, alpha = 0.75) +
  facet_wrap(~Well, scales = "free_y") +
  geom_line(data = dplyr::filter(ex_dat_mrg_wide, 
                                 noise == "No", window_width_n == 2),
            lty = 2, color = "red") +
  scale_color_grey(start = 0.7, end = 0.1)
       
#Plot per-capita derivative
ggplot(data = dplyr::filter(ex_dat_mrg_wide, noise == "Yes"),
       aes(x = Time, y = derivpercap)) +
  geom_line(aes(color = window_width_n), lwd = 1, alpha = 0.75) +
  facet_wrap(~Well, scales = "free_y") +
  geom_line(data = dplyr::filter(ex_dat_mrg_wide,
                                 noise == "No", window_width_n == 5),
            lty = 2, color = "red") +
  scale_color_grey(start = 0.7, end = 0.1) +
  ylim(NA, 5)

## -----------------------------------------------------------------------------
ex_dat_mrg <-
  mutate(group_by(ex_dat_mrg, Well, Bacteria_strain, Phage, noise),
         movavg_1 = Measurements,
         movavg_5 = smooth_data(x = Time, y = Measurements,
              sm_method = "moving-average", window_width_n = 5),
         movavg_9 = smooth_data(x = Time, y = Measurements,
              sm_method = "moving-average", window_width_n = 9))

#Reshape our data for plotting purposes
ex_dat_mrg_wide <- 
  pivot_longer(ex_dat_mrg, cols = starts_with("movavg"),
               names_prefix = "movavg_", names_to = "window_width_n")

#Plot data
ggplot(data = dplyr::filter(ex_dat_mrg_wide, noise == "Yes"),
       aes(x = Time, y = value)) +
  geom_line(aes(color = window_width_n), lwd = 1, alpha = 0.75) +
  facet_wrap(~Well, scales = "free_y") +
  geom_line(data = dplyr::filter(ex_dat_mrg_wide,
                                 noise == "No", window_width_n == 1),
            lty = 2, color = "red") +
  scale_color_grey(start = 0.7, end = 0.1) +
  scale_y_log10() +
  ggtitle("moving-average")

## -----------------------------------------------------------------------------
ex_dat_mrg <-
  mutate(group_by(ex_dat_mrg, Well, Bacteria_strain, Phage, noise),
         movmed_1 = Measurements,
         movmed_5 = 
           smooth_data(x = Time, y = Measurements,
                       sm_method = "moving-median", window_width_n = 5),
         movmed_9 = 
           smooth_data(x = Time, y = Measurements,
                       sm_method = "moving-median", window_width_n = 9))

#Reshape our data for plotting purposes
ex_dat_mrg_wide <- 
  pivot_longer(ex_dat_mrg, cols = starts_with("movmed"),
               names_prefix = "movmed_", names_to = "window_width_n")

#Plot data
ggplot(data = dplyr::filter(ex_dat_mrg_wide, noise == "Yes"),
       aes(x = Time, y = value)) +
  geom_line(aes(color = window_width_n), lwd = 1, alpha = 0.75) +
  facet_wrap(~Well, scales = "free_y") +
  geom_line(data = dplyr::filter(ex_dat_mrg_wide,
                                 noise == "No", window_width_n == 1),
            lty = 2, color = "red") +
  scale_color_grey(start = 0.7, end = 0.1) +
  scale_y_log10() +
  ggtitle("moving-median")

## -----------------------------------------------------------------------------
ex_dat_mrg <-
  mutate(group_by(ex_dat_mrg, Well, Bacteria_strain, Phage, noise),
         loess_0 = Measurements,
         loess_15 = smooth_data(x = Time, y = Measurements,
                                 sm_method = "loess", span = .15, degree = 1),
         loess_35 = smooth_data(x = Time, y = Measurements,
                                 sm_method = "loess", span = .35, degree = 1))

#Reshape our data for plotting purposes
ex_dat_mrg_wide <- 
  pivot_longer(ex_dat_mrg, cols = starts_with("loess"),
               names_prefix = "loess_", names_to = "span")

#Plot data
ggplot(data = dplyr::filter(ex_dat_mrg_wide, noise == "Yes"),
       aes(x = Time, y = value)) +
  geom_line(aes(color = as.factor(as.numeric(span)/100)), lwd = 1, alpha = 0.75) +
  facet_wrap(~Well, scales = "free_y") +
  geom_line(data = dplyr::filter(ex_dat_mrg_wide,
                                 noise == "No", span == 0),
            lty = 2, color = "red") +
  scale_color_grey(name = "span", start = 0.7, end = 0.1) +
  scale_y_log10() +
  ggtitle("loess")

## -----------------------------------------------------------------------------
ex_dat_mrg <-
  mutate(group_by(ex_dat_mrg, Well, Bacteria_strain, Phage, noise),
         gam_97 = Measurements,
         gam_15 = smooth_data(x = Time, y = Measurements,
                                  sm_method = "gam", k = 15),
         gam_8 = smooth_data(x = Time, y = Measurements,
                                 sm_method = "gam", k = 8))

#Reshape our data for plotting purposes
ex_dat_mrg_wide <- 
  pivot_longer(ex_dat_mrg, cols = starts_with("gam"),
               names_prefix = "gam_", names_to = "k")

#Plot data
ggplot(data = dplyr::filter(ex_dat_mrg_wide, noise == "Yes"),
       aes(x = Time, y = value)) +
  geom_line(aes(color = as.factor(as.numeric(k))), lwd = 1, alpha = 0.75) +
  facet_wrap(~Well, scales = "free_y") +
  geom_line(data = dplyr::filter(ex_dat_mrg_wide,
                                 noise == "No", k == 97),
            lty = 2, color = "red") +
  scale_color_grey(name = "span", start = 0.1, end = 0.7) +
  scale_y_log10() +
  ggtitle("gam")

## -----------------------------------------------------------------------------
ex_dat_mrg <-
  mutate(group_by(ex_dat_mrg, Well, Bacteria_strain, Phage, noise),
         smoothed_no = Measurements,
         sm_med3 = 
           smooth_data(x = Time, y = Measurements,
                       sm_method = "moving-median", window_width_n = 3),
         #Note that for the second round, we're using the 
         #first smoothing as the input y
         smoothed_yes = 
           smooth_data(x = Time, y = sm_med3,
                       sm_method = "moving-average", window_width_n = 3))

#Reshape our data for plotting purposes
ex_dat_mrg_wide <- 
  pivot_longer(ex_dat_mrg, cols = starts_with("smoothed"),
               names_to = "smoothed", names_prefix = "smoothed_")

#Plot data
ggplot(data = dplyr::filter(ex_dat_mrg_wide, noise == "Yes"),
       aes(x = Time, y = value, color = smoothed)) +
  geom_line(lwd = 1, alpha = 0.75) +
  scale_color_grey(start = 0.7, end = 0.1) +
  facet_wrap(~Well, scales = "free_y") +
  geom_line(data = dplyr::filter(ex_dat_mrg_wide,
                                 noise == "No", smoothed == "no"),
            lty = 2, color = "red") +
  scale_y_log10() +
  ggtitle("median then average smoothing")

## -----------------------------------------------------------------------------
# Note here that we're calculating derivatives of the smoothed column generated
#  in the previous section by combining moving median and moving average smoothing
ex_dat_mrg <- 
  mutate(group_by(ex_dat_mrg, Well, Bacteria_strain, Phage, noise),
         smderiv_0 = calc_deriv(x = Time, y = Measurements),
         smderivpercap_0 = calc_deriv(x = Time, y = Measurements,
                                       percapita = TRUE, blank = 0),
         smderiv_3 = calc_deriv(x = Time, y = smoothed_yes, window_width_n = 3),
         smderivpercap_3 = calc_deriv(x = Time, y = smoothed_yes, percapita = TRUE, 
                                    blank = 0, window_width_n = 3))

#Reshape our data for plotting purposes
ex_dat_mrg_wide <- 
  pivot_longer(ex_dat_mrg, cols = starts_with("smderiv"),
               names_to = c("deriv", "window_width_n"), names_sep = "_")
ex_dat_mrg_wide <- 
  pivot_wider(ex_dat_mrg_wide, names_from = deriv, values_from = value)

#Plot derivative
ggplot(data = dplyr::filter(ex_dat_mrg_wide, noise == "Yes"),
       aes(x = Time, y = smderiv, color = window_width_n)) +
  geom_line(lwd = 1, alpha = 0.75) +
  scale_color_grey(start = 0.7, end = 0.1) +
  facet_wrap(~Well, scales = "free_y") +
  geom_line(data = dplyr::filter(ex_dat_mrg_wide,
                                 noise == "No", window_width_n == 0),
            lty = 2, color = "red")

#Plot per-capita derivative
ggplot(data = dplyr::filter(ex_dat_mrg_wide, noise == "Yes"),
       aes(x = Time, y = smderivpercap, color = window_width_n)) +
  geom_line(lwd = 1, alpha = 0.75) +
  scale_color_grey(start = 0.7, end = 0.1) +
  facet_wrap(~Well, scales = "free_y") +
  geom_line(data = dplyr::filter(ex_dat_mrg_wide,
                                 noise == "No", window_width_n == 3),
            lty = 2, color = "red") +
  ylim(NA, 5)

## -----------------------------------------------------------------------------
#Plot density
ggplot(data = dplyr::filter(ex_dat_mrg, noise == "Yes"),
       aes(x = Time, y = smoothed_yes)) +
  geom_point() +
  facet_wrap(~Well, scales = "free_y") +
  scale_y_log10()

# Plot per-capita derivative
ggplot(data = dplyr::filter(ex_dat_mrg, noise == "Yes"),
       aes(x = Time, y = derivpercap_5)) +
  geom_point() +
  facet_wrap(~Well, scales = "free_y")

## -----------------------------------------------------------------------------
#Plot density
ggplot(data = dplyr::filter(ex_dat_mrg, noise == "Yes", smoothed_yes > 0.01),
       aes(x = Time, y = smoothed_yes)) +
  geom_point() +
  facet_wrap(~Well, scales = "free_y") +
  geom_hline(yintercept = 0.01, lty = 2) +
  scale_y_log10()

# Plot per-capita derivative
ggplot(data = dplyr::filter(ex_dat_mrg, noise == "Yes", smoothed_yes > 0.01),
       aes(x = Time, y = derivpercap_5)) +
  geom_point() +
  facet_wrap(~Well, scales = "free_y")

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <-
  summarize(group_by(ex_dat_mrg, Well, Bacteria_strain, Phage, noise),
            max_growth_rate = max(derivpercap_5[smoothed_yes > 0.01], 
                                  na.rm = TRUE))
head(ex_dat_mrg_sum)

