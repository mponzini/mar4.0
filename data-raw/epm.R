# prepare EPM data

#Clear existing data and graphics
rm(list=ls())
graphics.off()
# load libaries
library(readxl)
library(dplyr)
library(Hmisc)
library(data.table)
# import data
mar_network <- paste0(
  "S:/MIND/IDDRC Cores/",
  "Core F_Biostatistics Bioinformatics and Research Design (BBRD)/",
  "VandeWater_MARAutism/Request_97/MAR4"
)

dataset <- readxl::read_xlsx(
  path = paste0(mar_network, "/Data/MAR 4.0 EPM All Data.xlsx")
)

