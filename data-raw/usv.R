# prepare USV data

#Clear existing data and graphics
rm(list=ls())
graphics.off()
# load libaries
library(readxl)
library(dplyr)
library(Hmisc)
# import data
dataset <- readxl::read_xlsx(
  path = "inst/extdata/MAR 4.0 USV All Data.xlsx"
)

# reduce to variables of interest
dataset <- dataset |>
  dplyr::select(
    #identifiers and characteristics
    `Animal ID`, `Litter ID`, Sex, Treatment...6,
    # PND 4 variables

    # PND 8 variables

    # PND 12 variables

    # exclusion variables
    `PND4_Exclude?`, `PND8_Exclude?`, `PND12_Exclude?`
  )

labels <- c(
  dataset$`Animal ID` = "Animal ID",
  dataset$`Litter ID` = "Litter ID",
  dataset$Sex = "Sex",
  dataset$Treatment...6 = "MAR Group",

  dataset$`PND4_Exclude?` = "PND 4 - Exclusion",
  dataset$`PND8_Exclude?` = "PND 8 - Exclusion",
  dataset$`PND12_Exclude?` = "PND 12 - Exclusion"
)


usv <- tibble::tibble(dataset)
usethis::use_data(usv, overwrite = TRUE)
