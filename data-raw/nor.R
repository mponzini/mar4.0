# prepare Novel Objects data

#Clear existing data and graphics
rm(list=ls())
graphics.off()
# load libaries
library(readxl)
library(dplyr)
library(Hmisc)
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
  dplyr::mutate(
    `Discirmination Index` = as.numeric(`Discirmination Index`),
    Treat = relevel(factor(Treat),ref="Adjuvant+Saline")
  )|>
  dplyr::select(
    Cohort, `Animal ID`, Sex, `Litter ID`, Treat, `Discirmination Index`
  )

## over half of data is NA for discrimination index
usethis::use_data(nor,overwrite = T)
