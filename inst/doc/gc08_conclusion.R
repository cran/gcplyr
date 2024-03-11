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
#  as well as the 'average' well
ggplot(data = sim_dat_sum, 
       aes(x = Well == "averaged", y = max_growth_rate)) +
  geom_point(alpha = 0.5, position = position_jitter(width = 0.1)) +
  ylim(0.01, 0.03)

## -----------------------------------------------------------------------------
# This code was previously explained
# Here we're re-running it so it's available for us to work with
example_tidydata <- trans_wide_to_tidy(example_widedata_noiseless,
                                       id_cols = "Time")
ex_dat_mrg <- merge_dfs(example_tidydata, example_design_tidy)
ex_dat_mrg_sum <-
  summarize(group_by(dplyr::filter(ex_dat_mrg, Phage == "No Phage"),
                     Well, Bacteria_strain, Phage),
            auc = auc(x = Time, y = Measurements))

## -----------------------------------------------------------------------------
antibiotic_dat <- make_example(vignette = 8, example = 2)

head(antibiotic_dat)

## -----------------------------------------------------------------------------
growth_and_antibiotics <- 
  merge_dfs(ex_dat_mrg_sum, antibiotic_dat)
head(growth_and_antibiotics)

ggplot(data = growth_and_antibiotics, 
       aes(x = Antibiotic_resis, y = auc)) +
  geom_point()

