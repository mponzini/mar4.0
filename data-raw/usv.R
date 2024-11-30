# prepare USV data

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
  path = paste0(mar_network, "/Data/MAR 4.0 USV All Data.xlsx"),
  range = "A1:BE373"
)

## pivot to long format ##
pnd_string <- c("PND4_", "PND8_", "PND12_")

# variables to pivot
usv_count <- paste0(pnd_string, "Count")
usv_length <- paste0(pnd_string, "Call Length (s)")
usv_slope <- paste0(pnd_string, "Slope (kHz/s)")
usv_sinu <- paste0(pnd_string, "Sinuosity")
usv_power <- paste0(pnd_string, "Mean Power (dB/Hz)")
exclude <- paste0(pnd_string, "Exclude?")

# reduce to variables of interest
dataset <- dataset |>
  dplyr::select(
    #identifiers and characteristics
    Cohort:Track,
    # Count variables
    dplyr::all_of(usv_count),
    # Length variables
    dplyr::all_of(usv_length),
    # slope variables
    dplyr::all_of(usv_slope),
    # sinu variables
    dplyr::all_of(usv_sinu),
    # power variables
    dplyr::all_of(usv_power),
    # exclusion variables
    dplyr::all_of(exclude)
  )

# pivot at once using data.table::melt()
dataset_long <- dataset |>
  dplyr::select(
    Cohort:Track,
    dplyr::all_of(
      c(
        exclude, usv_count, usv_length, usv_slope, usv_sinu, usv_power
      )
    )
  ) |>
  data.table::as.data.table() |>
  data.table::melt(
    measure = list(
      exclude, usv_count, usv_length, usv_slope, usv_sinu, usv_power
    ),
    value.name = c(
      "Exclude", "Count", "Call Length", "Slope", "Sinuosity", "Mean Power"
    ),
    variable.name = "PND"
  ) |>
  dplyr::mutate(
    PND = dplyr::case_when(
      PND == 1 ~ 4,
      PND == 2 ~ 8,
      PND == 3 ~ 12
    )
  ) |>
  dplyr::arrange(`Animal ID`, DOB) |>
  # replace '-' with NA and convert to numeric
  dplyr::mutate(
    # replace "-"
    dplyr::across(
      .cols = where(is.character),
      ~ dplyr::na_if(.x , "-")
    ),
    # convert to numeric
    dplyr::across(
      .cols = c(Count:`Mean Power`),
      ~ as.numeric(.x)
    )
  )



usv <- tibble::tibble(dataset)
usethis::use_data(usv, overwrite = TRUE)
