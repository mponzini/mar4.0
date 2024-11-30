# prepare MIlestones data

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
  path = paste0(mar_network, "/Data/MAR 4.0 Milestones All Data.xlsx")
)

# minor fixes
dataset <- dataset |>
  # correct spelling of 'Reflex'
  dplyr::rename(
    `PND4_Righting Reflex Avg` = `PND4_Righting Relfex Avg`,
    `PND8_Righting Reflex Avg` = `PND8_Righting Relfex Avg`,
    `PND12_Righting Reflex Avg` = `PND12_Righting Relfex Avg`
  )

## pivot variables of interest to long format ##

# milestones: Body Length, Tail Length,  Belly Temp, Body Weight,
#   Righting Reflex Avg, Circle Traverse, Negative Geotaxis, Cliff Avoidance,
#   Bar Holding
exclude <- paste0(c("PND4_", "PND8_", "PND12_"), "Exclude?")
body_len <- paste0(c("PND4_", "PND8_", "PND12_"), 'Body Length')
tail_len <- paste0(c("PND4_", "PND8_", "PND12_"), 'Tail length')
belly_temp <- paste0(c("PND4_", "PND8_", "PND12_"), 'Belly Temp')
body_wt <- paste0(c("PND4_", "PND8_", "PND12_"), 'Body Weight')
righting_reflex <- paste0(c("PND4_", "PND8_", "PND12_"), 'Righting Reflex Avg')
circ_traverse <- paste0(c("PND4_", "PND8_", "PND12_"), 'Circle Traverse')
neg_geo <- paste0(c("PND4_", "PND8_", "PND12_"), 'Negative Geotaxis')
cliff_avoid <- paste0(c("PND4_", "PND8_", "PND12_"), 'Cliff Avoidance')
startle <- paste0(c("PND4_", "PND8_", "PND12_"), "Startle?")
grasp <- paste0(c("PND4_", "PND8_", "PND12_"), "Forelimb Grasp?")

# pivot all in one step using data.table::melt()
dataset_long <- dataset |>
  dplyr::select(
    Cohort:Track,
    dplyr::all_of(
      c(
        exclude, body_len, tail_len, belly_temp, body_wt, righting_reflex,
        circ_traverse, neg_geo, cliff_avoid, startle, grasp
      )
    )
  ) |>
  data.table::as.data.table() |>
  data.table::melt(
    measure = list(exclude, body_len, tail_len, belly_temp, body_wt,
                   righting_reflex, circ_traverse, neg_geo, cliff_avoid,
                   startle, grasp),
    value.name = c(
      "Exclude", "Body Length", "Tail Length", "Belly Temp", "Body Weight",
      "Righting Reflex", "Circle Traverse", "Negative Geotaxis",
      "Cliff Avoidance", "Startle", "Forelimb Grasp"
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
      .cols = c(`Body Length`:`Cliff Avoidance`),
      ~ as.numeric(.x)
    )
  )


milestones <- tibble::tibble(dataset_long)
usethis::use_data(milestones, overwrite = TRUE)
