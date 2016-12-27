## Preparotory work for Programming Assignment 3
##
setwd(file.path("~/Documents/Peter_Data/Arbeiten-Werkzeuge/Programmieren/R",
      "/Coursera/rprog_data_ProgAssignment3-data", fsep = .Platform$file.sep))
library(readr) # using the newer readr packages by Hadley Wickham

# Use the better read_csv (readr-package) instead the standard read.csv
outcome <- read_csv("outcome-of-care-measures.csv", 
                     col_types = cols(.default = "c"))
# Do file inspecting (note: just one command suficient)
outcome
outcome[, 11] <- as.numeric(outcome[[11]])
# You may get a warning about NAs introduced; that is okay
hist(outcome1[[11]])

# -----------------------------------------------------------------------------
# the call to read_csv are different than to read.csv 
# especially in printing and subsetting see: )
# beause it uses the tibble-packages with a trimmed down version of data.frame
# This has several advantages. 
# See: "?tibble-packages" but also: "vignette("tibble")
# The command "library(readr)" loads the tibble-packages as well into memory
#
# outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
## Do some file inspecting
# head(outcome)
# ncol(outcome)
# names(outcome)
## Draw plot
# outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs introduced; that is okay
# hist(outcome[, 11])