# prepare Weight data

#Clear existing data and graphics
rm(list=ls())
graphics.off()
# load libaries
library(readxl)
library(dplyr)
library(tidyr)
library(Hmisc)
library(data.table)
# import data
mar_network <- paste0(
  "S:/MIND/IDDRC Cores/",
  "Core F_Biostatistics Bioinformatics and Research Design (BBRD)/",
  "VandeWater_MARAutism/Request_97/MAR4"
)

dataset <- readxl::read_xlsx(
  path = paste0(mar_network, "/Data/MAR 4.0 Weights All Data.xlsx")
)

# reduce to variables of interest
dataset <- dataset |>
  dplyr::select(-c(Cohort, DOB))

# convert to long format
dataset <- dataset |>
  dplyr::mutate(
    # convert weights to numeric
    dplyr::across(
      .cols = c(`3`:`22`),
      ~ as.numeric(.x) |> suppressWarnings()
    )
  ) |>
  # pivot longer
  tidyr::pivot_longer(
    cols = c(`3`:`22`),
    values_to = "Weights",
    names_to = "Week"
  ) |>
  dplyr::mutate(
    # strip letters and symbols from Week, convert to numeric
    Week = gsub("[^0-9]", "", Week) |> as.numeric()
  )

labels <- list(
  `Animal ID` = "Animal ID",
  `Litter ID` = "Litter ID",
  Sex = "Sex",
  Treatment = "MAR Group",
  Week = "Week",
  Weights = "Weight"
)




weights <- tibble::tibble(dataset)
usethis::use_data(weights, overwrite = TRUE)
