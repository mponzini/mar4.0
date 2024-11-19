# prepare Social Dyads data

#Clear existing data and graphics
rm(list=ls())
graphics.off()
# load libaries
library(readxl)
library(dplyr)
library(Hmisc)
# import data\
mar_network <- paste0(
  "S:/MIND/IDDRC Cores/",
  "Core F_Biostatistics Bioinformatics and Research Design (BBRD)/",
  "VandeWater_MARAutism/Request_97/MAR4"
)

dataset <- readxl::read_xlsx(
  path = paste0(mar_network, "/Data/MAR 4.0 SD All Data.xlsx")
)

dataset <- dataset |>
  # create total vars
  dplyr::mutate(
    # total social play
    `Total Duration Play` = rowSums(
      dplyr::select(.,tidyr::contains("Total duration Play"))
    )
  )

sd <- dataset

sd_juv <- sd |>
  dplyr::filter(
    `Testing Timepoint` == 'Juvenile'
  )

sd_adult <- sd |>
  dplyr::filter(
    `Testing Timepoint` == 'Adult'
  )
