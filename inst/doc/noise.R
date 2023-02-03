## ----global options, include = FALSE------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(root.dir = tempdir())

#Note: to knit to pdf, word, and html simultaneously, run in R console:
#{rmarkdown::render("./vignettes/noise.Rmd", rmarkdown::html_vignette(toc = TRUE, toc_dept = 4)); rmarkdown::render("./vignettes/noise.Rmd", rmarkdown::pdf_document(toc = TRUE, toc_depth = 4)); rmarkdown::render("./vignettes/noise.Rmd", rmarkdown::word_document(toc = TRUE, toc_dept = 4))}

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

sample_wells <- c("A1", "F1", "F10", "E11")

## -----------------------------------------------------------------------------
#This is the data we've been working with previously
noiseless_data <- 
  trans_wide_to_tidy(example_widedata_noiseless, id_cols = "Time")
#This is the same data but with simulated noise added
noisy_data <- trans_wide_to_tidy(example_widedata, id_cols = "Time")
#We'll add some identifiers and then merge them together
noiseless_data <- mutate(noiseless_data, noise = "No")
noisy_data <- mutate(noisy_data, noise = "Yes")
ex_dat_mrg <- merge_dfs(noisy_data, noiseless_data)
ex_dat_mrg <- merge_dfs(ex_dat_mrg, example_design)

ex_dat_mrg$Well <- 
  factor(ex_dat_mrg$Well,
         levels = paste(rep(LETTERS[1:8], each = 12), 1:12, sep = ""))

#For computational speed, let's just keep the wells we'll be focusing on
# (for your own analyses, you should skip this step and continue using
# all of your data)
ex_dat_mrg <- dplyr::filter(ex_dat_mrg, Well %in% sample_wells)

#Plot with a linear y-axis
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements, color = noise)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~Well)

#Plot with a log y-axis
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements, color = noise)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~Well) +
  scale_y_continuous(trans = "log10")

## -----------------------------------------------------------------------------
ex_dat_mrg <- 
  mutate(group_by(ex_dat_mrg, Well, Bacteria_strain, Phage, noise),
         deriv = calc_deriv(x = Time, y = Measurements, x_scale = 3600),
         deriv_percap = calc_deriv(x = Time, y = Measurements, x_scale = 3600,
                                   percapita = TRUE, blank = 0))

#Plot derivative
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv, color = noise)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~Well, scales = "free_y")

#Plot per-capita derivative
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv_percap, color = noise)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~Well, scales = "free_y")

## -----------------------------------------------------------------------------
ex_dat_mrg <- 
  mutate(group_by(ex_dat_mrg, Well, Bacteria_strain, Phage, noise),
         deriv5 = calc_deriv(x = Time, y = Measurements, x_scale = 3600,
                            window_width_n = 5),
         deriv_percap5 = calc_deriv(x = Time, y = Measurements, x_scale = 3600,
                                   percapita = TRUE, blank = 0,
                                   window_width_n = 5),
         deriv9 = calc_deriv(x = Time, y = Measurements, x_scale = 3600,
                            window_width_n = 9),
         deriv_percap9 = calc_deriv(x = Time, y = Measurements, x_scale = 3600,
                                   percapita = TRUE, blank = 0,
                                   window_width_n = 9))

#Plot derivative 5
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv, color = noise)) +
  geom_point(alpha = 0.75, size = 0.75) +
  geom_line(linewidth = 1.25, alpha = 0.5, aes(y = deriv5)) +
  facet_wrap(~Well, scales = "free_y") +
  ggtitle("window_width_n = 5")

#Plot derivative 9
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv, color = noise)) +
  geom_point(alpha = 0.75, size = 0.75) +
  geom_line(linewidth = 1.25, alpha = 0.5, aes(y = deriv9)) +
  facet_wrap(~Well, scales = "free_y") +
  ggtitle("window_width_n = 5")

#Plot per-capita derivative 5
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv_percap, color = noise)) +
  geom_point(alpha = 0.75, size = 0.75) +
  geom_line(linewidth = 1.25, alpha = 0.5, aes(y = deriv_percap5)) +
  facet_wrap(~Well, scales = "free_y") +
  ggtitle("window_width_n = 5") +
  ylim(NA, 10)

#Plot per-capita derivative 9
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv_percap, color = noise)) +
  geom_point(alpha = 0.75, size = 0.75) +
  geom_line(linewidth = 1.25, alpha = 0.5, aes(y = deriv_percap9)) +
  facet_wrap(~Well, scales = "free_y") +
  ggtitle("window_width_n = 9") +
  ylim(NA, 10)

## -----------------------------------------------------------------------------
ex_dat_mrg <-
  mutate(group_by(ex_dat_mrg, Well, Bacteria_strain, Phage, noise),
         smoothed5 = smooth_data(x = Time, y = Measurements,
              sm_method = "moving-average", window_width_n = 5),
         smoothed9 = smooth_data(x = Time, y = Measurements,
              sm_method = "moving-average", window_width_n = 9))

#What does the smoothed data look like compared to the 'true' noiseless data?
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements, color = noise)) +
  geom_point(alpha = 0.75, size = 0.75) +
  geom_line(linewidth = 1.25, alpha = 0.5, aes(y = smoothed5)) +
  facet_wrap(~Well, scales = "free_y") +
  ggtitle("window_width_n = 5") +
  scale_y_log10()

#What does the smoothed data look like compared to the 'true' noiseless data?
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements, color = noise)) +
  geom_point(alpha = 0.75, size = 0.75) +
  geom_line(linewidth = 1.25, alpha = 0.5, aes(y = smoothed9)) +
  facet_wrap(~Well, scales = "free_y") +
  ggtitle("window_width_n = 9") +
  scale_y_log10()

## -----------------------------------------------------------------------------
ex_dat_mrg <-
  mutate(group_by(ex_dat_mrg, Well, Bacteria_strain, Phage, noise),
         smoothed5 = 
           smooth_data(x = Time, y = Measurements,
                       sm_method = "moving-median", window_width_n = 5),
         smoothed9 = 
           smooth_data(x = Time, y = Measurements,
                       sm_method = "moving-median", window_width_n = 9))

#What does the smoothed data look like compared to the 'true' noiseless data?
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements, color = noise)) +
  geom_point(alpha = 0.75, size = 0.75) +
  geom_line(linewidth = 1.25, alpha = 0.5, aes(y = smoothed5)) +
  facet_wrap(~Well, scales = "free_y") +
  ggtitle("window_width_n = 5") +
  scale_y_log10()

#What does the smoothed data look like compared to the 'true' noiseless data?
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements, color = noise)) +
  geom_point(alpha = 0.75, size = 0.75) +
  geom_line(linewidth = 1.25, alpha = 0.5, aes(y = smoothed9)) +
  facet_wrap(~Well, scales = "free_y") +
  ggtitle("window_width_n = 9") +
  scale_y_log10()

## -----------------------------------------------------------------------------
ex_dat_mrg <-
  mutate(group_by(ex_dat_mrg, Well, Bacteria_strain, Phage, noise),
         smoothed15 = smooth_data(x = Time, y = Measurements,
                                 sm_method = "loess", span = .15, degree = 1),
         smoothed35 = smooth_data(x = Time, y = Measurements,
                                 sm_method = "loess", span = .35, degree = 1))

#What does the smoothed data look like compared to the 'true' noiseless data?
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements, color = noise)) +
  geom_point(alpha = 0.75, size = 0.75) +
  geom_line(linewidth = 1.25, alpha = 0.5, aes(y = smoothed15)) +
  facet_wrap(~Well, scales = "free_y") +
  ggtitle("span = 0.15") +
  scale_y_log10()

#What does the smoothed data look like compared to the 'true' noiseless data?
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements, color = noise)) +
  geom_point(alpha = 0.75, size = 0.75) +
  geom_line(linewidth = 1.25, alpha = 0.5, aes(y = smoothed35)) +
  facet_wrap(~Well, scales = "free_y") +
  ggtitle("span = 0.35") +
  scale_y_log10()

## -----------------------------------------------------------------------------
ex_dat_mrg <-
  mutate(group_by(ex_dat_mrg, Well, Bacteria_strain, Phage, noise),
         smoothed15 = smooth_data(x = Time, y = Measurements,
                                  sm_method = "gam", k = 15),
         smoothed8 = smooth_data(x = Time, y = Measurements,
                                 sm_method = "gam", k = 8))

#What does the smoothed data look like compared to the 'true' noiseless data?
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements, color = noise)) +
  geom_point(alpha = 0.75, size = 0.75) +
  geom_line(linewidth = 1.25, alpha = 0.5, aes(y = smoothed15)) +
  facet_wrap(~Well, scales = "free_y") +
  ggtitle("k = 15") +
  scale_y_log10()

#What does the smoothed data look like compared to the 'true' noiseless data?
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements, color = noise)) +
  geom_point(alpha = 0.75, size = 0.75) +
  geom_line(linewidth = 1.25, alpha = 0.5, aes(y = smoothed8)) +
  facet_wrap(~Well, scales = "free_y") +
  ggtitle("k = 8") +
  scale_y_log10()

## -----------------------------------------------------------------------------
ex_dat_mrg <-
  mutate(group_by(ex_dat_mrg, Well, Bacteria_strain, Phage, noise),
         smoothed_med3 = 
           smooth_data(x = Time, y = Measurements,
                       sm_method = "moving-median", window_width_n = 3),
         #Note that for the second round, we're using the 
         #first smoothing as the input y
         smoothed = 
           smooth_data(x = Time, y = smoothed_med3,
                       sm_method = "moving-average", window_width_n = 3))

#What does the smoothed data look like compared to the 'true' noiseless data?
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements, color = noise)) +
  geom_point(alpha = 0.75, size = 0.75) +
  geom_line(linewidth = 1.25, alpha = 0.5, aes(y = smoothed)) +
  facet_wrap(~Well, scales = "free_y") +
  ggtitle("median then average smoothing") +
  scale_y_log10()

## -----------------------------------------------------------------------------
#Note here that we're calculating derivatives of the smoothed column generated
# in the previous section by combining moving median and moving average smoothing
ex_dat_mrg <- 
  mutate(group_by(ex_dat_mrg, Well, Bacteria_strain, Phage, noise),
         deriv_raw = calc_deriv(x = Time, y = Measurements, x_scale = 3600),
         deriv_percap_raw = calc_deriv(x = Time, y = Measurements,
                                       x_scale = 3600, percapita = TRUE,
                                       blank = 0),
         deriv = calc_deriv(x = Time, y = smoothed, x_scale = 3600),
         deriv_percap = calc_deriv(x = Time, y = smoothed, x_scale = 3600,
                                   percapita = TRUE, blank = 0),
         deriv3 = calc_deriv(x = Time, y = smoothed, x_scale = 3600,
                            window_width_n = 3),
         deriv_percap3 = calc_deriv(x = Time, y = smoothed, x_scale = 3600,
                                   percapita = TRUE, blank = 0,
                                   window_width_n = 3))

#Plot derivative of smoothed data
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv_raw, color = noise)) +
  geom_point(alpha = 0.75, size = 0.75) +
  geom_line(linewidth = 1.25, alpha = 0.5, aes(y = deriv)) +
  facet_wrap(~Well, scales = "free_y") +
  ggtitle("smoothed raw data")

#Plot derivative of smoothed data with smoothing during calc_deriv
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv_raw, color = noise)) +
  geom_point(alpha = 0.75, size = 0.75) +
  geom_line(linewidth = 1.25, alpha = 0.5, aes(y = deriv3)) +
  facet_wrap(~Well, scales = "free_y") +
  ggtitle("smoothed data and calc_deriv window_width_n = 3")

#Plot per-capita derivative of smoothed data
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv_percap_raw, color = noise)) +
  geom_point(alpha = 0.75, size = 0.75) +
  geom_line(linewidth = 1.25, alpha = 0.5, aes(y = deriv_percap)) +
  facet_wrap(~Well, scales = "free_y") +
  ggtitle("smoothed raw data") +
  ylim(NA, 10)

#Plot per-capita derivative of smoothed data with smoothing during calc_deriv
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv_percap_raw, color = noise)) +
  geom_point(alpha = 0.75, size = 0.75) +
  geom_line(linewidth = 1.25, alpha = 0.5, aes(y = deriv_percap3)) +
  facet_wrap(~Well, scales = "free_y") +
  ggtitle("smoothed data and calc_deriv window_width_n = 3") +
  ylim(NA, 10)

## -----------------------------------------------------------------------------
#Plot per-capita derivative of smoothed data with smoothing during calc_deriv
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = deriv_percap_raw, color = noise)) +
  geom_point(alpha = 0.75, size = 0.75) +
  geom_line(linewidth = 1.25, alpha = 0.5, aes(y = deriv_percap3)) +
  facet_wrap(~Well, scales = "free_y") +
  ggtitle("smoothed data and calc_deriv window_width_n = 3") +
  ylim(NA, 10)

## -----------------------------------------------------------------------------
ggplot(data = dplyr::filter(ex_dat_mrg, Well %in% sample_wells),
       aes(x = Time, y = Measurements, color = noise)) +
  geom_point(alpha = 0.75, size = 0.75) +
  geom_line(linewidth = 1.25, alpha = 0.5, aes(y = smoothed)) +
  facet_wrap(~Well, scales = "free_y")

## -----------------------------------------------------------------------------
for (my_well in sample_wells) {
  #Title
  title <- cowplot::ggdraw() + 
    cowplot::draw_label(paste("Well", my_well), 
                        fontface = "bold", x = 0, hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 7))
  
  #Save x and y limits for all plots so they're all on the same axes
  xdat <- dplyr::filter(ex_dat_mrg, Well == my_well)$Time
  ydat <- dplyr::filter(ex_dat_mrg, Well == my_well)$deriv_percap3
  xlims <- c(min(xdat[is.finite(xdat)], na.rm = TRUE),
             max(xdat[is.finite(xdat)], na.rm = TRUE))
  ylims <- c(min(ydat[is.finite(ydat)], na.rm = TRUE),
             max(ydat[is.finite(ydat)], na.rm = TRUE))
  
  #Plot unfiltered data
  p1 <- ggplot(data = dplyr::filter(ex_dat_mrg, Well == my_well),
               aes(x = Time, y = deriv_percap3, color = noise)) +
    geom_point(alpha = 0.5) + facet_wrap(~Well, scales = "free") +
    ggtitle("all data") +
    xlim(xlims[1], xlims[2]) + ylim(ylims[1], ylims[2])
  
  #Plot data with filters for density
  p2 <- ggplot(data = dplyr::filter(ex_dat_mrg, 
                                    Well == my_well, smoothed > 0.001),
               aes(x = Time, y = deriv_percap3, color = noise)) +
    geom_point(alpha = 0.5) + facet_wrap(~Well, scales = "free") +
    ggtitle("data where Abs > 0.001") +
    xlim(xlims[1], xlims[2]) + ylim(ylims[1], ylims[2])
  p3 <- ggplot(data = dplyr::filter(ex_dat_mrg, 
                                    Well == my_well, smoothed > 0.005),
               aes(x = Time, y = deriv_percap3, color = noise)) +
    geom_point(alpha = 0.5) + facet_wrap(~Well, scales = "free") +
    ggtitle("data where Abs > 0.005") +
    xlim(xlims[1], xlims[2]) + ylim(ylims[1], ylims[2])
  p4 <- ggplot(data = dplyr::filter(ex_dat_mrg, 
                                    Well == my_well, smoothed > 0.01),
               aes(x = Time, y = deriv_percap3, color = noise)) +
    geom_point(alpha = 0.5) + facet_wrap(~Well, scales = "free") +
    ggtitle("data where Abs > 0.01") +
    xlim(xlims[1], xlims[2]) + ylim(ylims[1], ylims[2])
  
  print(cowplot::plot_grid(title, cowplot::plot_grid(p1, p2, p3, p4, ncol = 2),
                           ncol = 1, rel_heights = c(0.1, 1)))
}

## -----------------------------------------------------------------------------
ex_dat_mrg_sum <-
  summarize(group_by(ex_dat_mrg, Well, Bacteria_strain, Phage, noise),
            max_growth_rate = max(deriv_percap3[smoothed > 0.01], 
                                  na.rm = TRUE))
head(ex_dat_mrg_sum)

## -----------------------------------------------------------------------------
ggplot(data = dplyr::filter(ex_dat_mrg, 
                            Well %in% sample_wells, smoothed >= 0.01),
       aes(x = Time, y = deriv_percap3, color = noise)) +
  geom_point() +
  facet_wrap(~Well, scales = "free") +
  ggtitle("data where smoothed density > 0.01") +
  geom_hline(data = dplyr::filter(ex_dat_mrg_sum, Well %in% sample_wells), 
             aes(yintercept = max_growth_rate, color = noise), lty = 2)

