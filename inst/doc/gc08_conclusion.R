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
ex_dat_mrg <- group_by(ex_dat_mrg, Well, Bacteria_strain, Phage)

ex_dat_mrg <-
  mutate(ex_dat_mrg,
         smoothed_med3 = 
           smooth_data(x = Time, y = Measurements,
                       sm_method = "moving-median", window_width_n = 3),
         # Note that for the second round, we're using the 
         # first smoothing as the input y
         smoothed = 
           smooth_data(x = Time, y = smoothed_med3,
                       sm_method = "moving-average", window_width_n = 3))

## -----------------------------------------------------------------------------
sim_dat_tdy <- make_example(vignette = 8, example = 1)

## -----------------------------------------------------------------------------
sim_dat_tdy <- mutate(group_by(sim_dat_tdy, Well),
                      percap_deriv = calc_deriv(y = Measurements, x = time,
                                                percapita = TRUE, blank = 0))

# Plot the growth in our wells
ggplot(data = filter(sim_dat_tdy, Well != "averaged"), 
       aes(x = time, y = Measurements, group = Well)) +
  geom_line(alpha = 0.1) +
  geom_line(data = filter(sim_dat_tdy, Well == "averaged"), color = "red") +
  scale_y_continuous(trans = "log10")

## -----------------------------------------------------------------------------
# Summarize our data
sim_dat_sum <- summarize(group_by(sim_dat_tdy, Well),
                         max_growth_rate = max(percap_deriv, na.rm = TRUE))

# Plot the maximum per-capita growth rates of each well
#  Add a red line for the max growth rate of the "average well"
#  Add a dashed line for the average growth rate of all the wells
ggplot(data = filter(sim_dat_sum, Well != "averaged"), 
       aes(x = max_growth_rate)) +
  geom_histogram() +
  geom_vline(data = filter(sim_dat_sum, Well == "averaged"), 
             aes(xintercept = max_growth_rate), color = "red") +
  geom_vline(xintercept = 
               mean(filter(sim_dat_sum, Well != "averaged")$max_growth_rate),
             lty = 2)

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <-
  summarize(dplyr::filter(ex_dat_mrg, Phage == "No Phage"), 
            auc = auc(x = Time, y = smoothed))

## -----------------------------------------------------------------------------
set.seed(123)
antibiotic_dat <- 
  data.frame(Bacteria_strain = paste("Strain", 1:48),
             Antibiotic_resis = 
               ex_dat_mrg_sum$auc[
                 match(paste("Strain", 1:48), 
                       ex_dat_mrg_sum$Bacteria_strain)] * 
               runif(48, 0.5, 1.5) < mean(ex_dat_mrg_sum$auc))

head(antibiotic_dat)

## -----------------------------------------------------------------------------
growth_and_antibiotics <- 
  merge_dfs(ex_dat_mrg_sum, antibiotic_dat)
head(growth_and_antibiotics)

ggplot(data = growth_and_antibiotics, 
       aes(x = Antibiotic_resis, y = auc)) +
  geom_point()

