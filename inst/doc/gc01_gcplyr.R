## ----global options, include = FALSE------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(root.dir = tempdir())

#To re-build all pdf vignettes (only ones that are git checked-in), 
# copy-paste the code below into console (Ctrl+Enter does not work):
#for (fil in list.files(path = "./vignettes/", pattern = ".+Rmd")) {rmarkdown::render(paste0("./vignettes/", fil), rmarkdown::pdf_document(toc = TRUE, toc_depth = 4))}

#To re-build all vignettes where the rmd was changed more recently than the pdf
# copy-paste the code below into console (Ctrl+Enter does not work):
#for (fil in list.files(path = "./vignettes/", pattern = ".+Rmd")) {if(file.info(paste0("./vignettes/", fil))$mtime > file.info(paste0("./vignettes/", sub(x = fil, "\\.Rmd", ".pdf")))$mtime) {rmarkdown::render(paste0("./vignettes/", fil), rmarkdown::pdf_document(toc = TRUE, toc_depth = 4))}}
#To do so for all vignettes, copy-paste the code below into console
# (Ctrl+Enter does not work):
#for (fil in list.files(path = "./vignettes/", pattern = ".+Rmd")) {rmarkdown::render(paste0("./vignettes/", fil), rmarkdown::html_vignette(toc = TRUE, toc_dept = 4)); rmarkdown::render(paste0("./vignettes/", fil), rmarkdown::pdf_document(toc = TRUE, toc_depth = 4)); rmarkdown::render(paste0("./vignettes/", fil), rmarkdown::word_document(toc = TRUE, toc_dept = 4))}
#
#Get timings for all html builds (copy-paste not Ctrl+Enter)
#{filtimes <- data.frame(fil = list.files(path = "./vignettes/", pattern = ".+Rmd")); filtimes$times <- NA; for (filindex in 1:nrow(filtimes)) {fil <- filtimes$fil[filindex]; start <- Sys.time(); rmarkdown::render(paste0("./vignettes/", fil), rmarkdown::html_vignette(toc = TRUE, toc_dept = 4)); filtimes$times[filindex] <- difftime(Sys.time(), start, units = "mins")}; print(filtimes)}

## ----setup--------------------------------------------------------------------
library(gcplyr)

library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
#For the purposes of this demo, we have to create our example data and
# design files. Normally, the data file would be created by a plate reader, and
# the design file would be created by you, the user

#Generate our example data file, widedata.csv
make_example(vignette = 1, example = 1)

#Generate our example design files, Bacteria_strain.csv and Phage.csv
make_example(vignette = 1, example = 2)

# Read in our data 
data_wide <- read_wides(files = "widedata.csv")

# Transform our data to be tidy-shaped
data_tidy <- 
  trans_wide_to_tidy(wides = data_wide, id_cols = c("file", "Time"))

# Convert our time into hours
data_tidy$Time <- as.numeric(data_tidy$Time)/3600

# Import our designs
designs <- import_blockdesigns(files = c("Bacteria_strain.csv", "Phage.csv"))

# Merge our designs and data
data_merged <- merge_dfs(data_tidy, designs)

#Set up the Well column so they plot in the correct order
data_merged$Well <- 
  factor(data_merged$Well,
         levels = paste0(rep(LETTERS[1:8], each = 12), 1:12))

#Plot the data
ggplot(data = data_merged, aes(x = Time, y = Measurements)) +
  geom_line() +
  facet_wrap(~Well, nrow = 8, ncol = 12)

# Voila! 8 lines of code and all your data is imported & plotted!

# Calculate the per-capita growth rate over time in each well
data_merged <- mutate(
  group_by(data_merged, Well),
  percap_deriv = calc_deriv(y = Measurements, x = Time, percapita = TRUE, 
                            blank = 0, window_width_n = 5))

# Calculate four common metrics of bacterial growth:
#  the lag time, saving it to a column named lag_time
#  the maximum growth rate, saving it to a column named max_percap
#  the maximum density, saving it to a column named max_dens
#  the area-under-the-curve, saving it to a column named 'auc'
data_sum <- summarize(
  group_by(data_merged, Well, Bacteria_strain, Phage),
  lag_time = lag_time(x = Time, y = Measurements, deriv = percap_deriv,
                      blank = 0),
  max_percap = max(percap_deriv, na.rm = TRUE),
  max_dens = max(Measurements),
  auc = auc(y = Measurements, x = as.numeric(Time)))

# Print some of the values
head(data_sum)

#Set up the Well column so they plot in the correct order
data_sum$Well <- factor(data_sum$Well, 
                        levels = paste0(rep(LETTERS[1:8], each = 12), 1:12))

#Plot lag time
ggplot(data = data_sum) +
  geom_text(aes(label = round(lag_time, 2), x = 1, y = 1)) +
  facet_wrap(~ Well, ncol = 12) +
  labs(title = "Lag time by well") +
  theme(axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank())
#Plot growth rate
ggplot(data = data_sum) +
  geom_text(aes(label = round(max_percap, 2), x = 1, y = 1)) +
  facet_wrap(~ Well, ncol = 12) +
  labs(title = "Maximum growth rate by well") +
  theme(axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank())
#Plot Maximum density
ggplot(data = data_sum) +
  geom_text(aes(label = round(max_dens, 2), x = 1, y = 1)) +
  facet_wrap(~ Well, ncol = 12) +
  labs(title = "Maximum density by well") +
  theme(axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank())
#Plot AUC
ggplot(data = data_sum) +
  geom_text(aes(label = round(auc, 2), x = 1, y = 1)) +
  facet_wrap(~ Well, ncol = 12) +
  labs(title = "Area under the curve by well") +
  theme(axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank())

