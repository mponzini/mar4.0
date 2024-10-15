# prepare Social Dyads data

#Clear existing data and graphics
rm(list=ls())
graphics.off()
# load libaries
library(readxl)
library(dplyr)
library(Hmisc)
# import data
dataset <- readxl::read_xlsx(
  path = "inst/extdata/MAR 4.0 SD All Data.xlsx"
)

dataset <- dataset |>
  # create total vars
  dplyr::mutate(
    # total social play
    `Total Duration Play` = rowSums(
      dplyr::select(.,tidyr::contains("Total duration Play"))
    )
  )
