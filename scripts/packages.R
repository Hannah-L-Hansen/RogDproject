#Gets necessary packages

library(readxl)
library(dplyr)
library(ggplot2)
#library(biogas)

# Load latest biogas package functions
ff <- list.files("../biogas functions", full.names = TRUE)
for (i in ff) source(i)

