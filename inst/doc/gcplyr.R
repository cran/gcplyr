## ----global options, include = FALSE------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(root.dir = tempdir())

#Note: to knit to pdf, word, and html simultaneously, run in R console:
#{rmarkdown::render("./vignettes/gcplyr.Rmd", rmarkdown::html_vignette(toc = TRUE, toc_dept = 4)); rmarkdown::render("./vignettes/gcplyr.Rmd", rmarkdown::pdf_document(toc = TRUE, toc_depth = 4)); rmarkdown::render("./vignettes/gcplyr.Rmd", rmarkdown::word_document(toc = TRUE, toc_dept = 4))}

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

## ---- include = FALSE---------------------------------------------------------
print_df <- function(df, col.names = FALSE) {
  write.table(format(df, justify = "right"),
              row.names=FALSE, col.names = col.names, quote = F)
}

## ---- include = FALSE---------------------------------------------------------
#This code just creates a wide-shaped example file
#Don't worry about how it works - when working with real growth
#curves data, this file would be created by the plate reader
write.csv(example_widedata_noiseless, file = "widedata.csv", row.names = FALSE)

#This code just creates our block designs
example_design <- make_design(
  output_format = "blocks",
  pattern_split = ",", nrows = 8, ncols = 12,
  "Bacteria_strain" = make_designpattern(
    values = paste("Strain", 1:48),
    rows = 1:8, cols = 1:6,
    pattern = 1:48,
    byrow = TRUE),
  "Bacteria_strain" = make_designpattern(
    values = paste("Strain", 1:48),
    rows = 1:8, cols = 7:12,
    pattern = 1:48,
    byrow = TRUE),
  "Phage" = make_designpattern(
    values = c("No Phage"),
    rows = 1:8, cols = 1:6,
    pattern = "1"),
  "Phage" = make_designpattern(
    values = c("Phage Added"),
    rows = 1:8, cols = 7:12,
    pattern = "1"))
write_blocks(example_design, file = NULL)

## -----------------------------------------------------------------------------
#Read in our data
# (our plate reader data is saved in "widedata.csv")
data_wide <- read_wides(files = "widedata.csv")

#Transform our data to be tidy-shaped
data_tidy <- 
  trans_wide_to_tidy(wides = data_wide, id_cols = c("file", "Time"))

#Import our designs
# (saved in the files Bacteria_strain.csv and Phage.csv)
designs <- import_blockdesigns(files = c("Bacteria_strain.csv", "Phage.csv"))

#Merge our designs and data
data_merged <- merge_dfs(data_tidy, designs)

#Plot the data
ggplot(data = data_merged,
       aes(x = as.numeric(Time), y = Measurements, color = Well)) +
  geom_line(aes(lty = Phage)) + 
  guides(color = "none")

#Voila! 8 lines of code and all your data is imported & plotted!

#Calculate the per-capita growth rate over time in each well
data_merged <- mutate(
  group_by(data_merged, Well),
  percap_deriv = calc_deriv(y = Measurements, x = Time, percapita = TRUE, 
                            blank = 0, window_width_n = 5, x_scale = 3600))

#Calculate two common metrics of bacterial growth:
# the maximum growth rate, saving it to a column named max_percap
# the area-under-the-curve, saving it to a column named 'auc'
data_sum <- summarize(
  group_by(data_merged, Well, Bacteria_strain, Phage),
  max_percap = max(percap_deriv, na.rm = TRUE),
  auc = auc(y = Measurements, x = as.numeric(Time)))

#Print some of the max growth rates and auc's
head(data_sum)

#Plot the results for max growth rate and area under the curve in presence vs absence of phage
ggplot(data = data_sum,
       aes(x = max_percap, y = auc, color = Phage)) +
  geom_point()

