# prepare MIlestones data

#Clear existing data and graphics
rm(list=ls())
graphics.off()
# load libaries
library(readxl)
library(dplyr)
library(Hmisc)
# import data
dataset <- readxl::read_xlsx(
  path = "inst/extdata/MAR 4.0 Milestones All Data.xlsx"
)

