# prepare Novel Objects data

#Clear existing data and graphics
rm(list=ls())
graphics.off()
# load libaries
library(readxl)
library(dplyr)
library(Hmisc)
library(mar4.0)
# import data
mar_network <- paste0(
  "S:/MIND/IDDRC Cores/",
  "Core F_Biostatistics Bioinformatics and Research Design (BBRD)/",
  "VandeWater_MARAutism/Request_97/MAR4"
)

dataset <- readxl::read_xlsx(
  path = paste0(mar_network, "/Data/MAR4.0 NOR All Data.xlsx")
)

## variable of interest: Discrimination Index
nor <- dataset |>
  # exclude cross fostered rodents for analysis
  mar4.0::exclude_cross_fostered() |>
  dplyr::mutate(
    `Discirmination Index` = as.numeric(`Discirmination Index`),
    Treat = relevel(factor(Treat),ref="Adjuvant+Saline")
  )|>
  dplyr::select(
    Cohort, `Animal ID`, Sex, `Litter ID`, Treat, `Discirmination Index`,
    `Trial Type`, `Non Participant?`
  ) |>
  dplyr::filter(
    stringr::str_detect(`Trial Type`, 'Test'),
    `Non Participant?` == 'N'
  ) |>
  dplyr::mutate(
    `Discirmination Index` = as.numeric(`Discirmination Index`),
    Treat = factor(
      Treat,
      levels = c(
        'Adjuvant+Saline','LDHA+LDHB+CRMP1+STIP1', 'CRMP1+CRMP2', 'CRMP1+GDA',
        'STIP1+NSE'
      )
    )
  )

## over half of data is NA for discrimination index
usethis::use_data(nor, overwrite = TRUE)
