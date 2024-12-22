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
  # exclude cross fostered rodents for analysis
  mar4.0::exclude_cross_fostered() |>
  # clean data
  dplyr::mutate(
    # convert durations to numeric
    dplyr::across(
      .cols = c(tidyr::starts_with("Total duration Play"),
                tidyr::starts_with("Total number play")),
      ~ as.numeric(.x) |> suppressWarnings()
    ),
    # total social play
    `Total duration Play` = rowSums(
      dplyr::across(.cols = tidyr::starts_with("Total duration Play"))
    ),
    `Total number Play` = rowSums(
      dplyr::across(.cols = tidyr::starts_with("Total number Play"))
    )
  ) |>
  # reduce to necessary variables
  dplyr::select(
    `Animal ID`, `Litter ID`, Sex, Treat, `Testing Timepoint`,
    # total durations
    `Total duration Play`,
    `Total duration Non-social (Total)`,
    `Total duration Social Investigation (Total)`,
    `Total duration Grooming Self`,
    # total number
    `Total number Play`,
    `Total number Non-social (Total)`,
    `Total number Social Investigation (Total)`,
    `Total number Grooming Self`
  )

sd_juv <- dataset |>
  dplyr::filter(
    `Testing Timepoint` == 'Juvenile'
  )

sd_adult <- dataset |>
  dplyr::filter(
    `Testing Timepoint` == 'Adult'
  )


usethis::use_data(sd_juv, overwrite = TRUE)
usethis::use_data(sd_adult, overwrite = TRUE)
