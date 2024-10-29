# prepare Novel Objects data

#Clear existing data and graphics
rm(list=ls())
graphics.off()
# load libaries
library(readxl)
library(dplyr)
library(Hmisc)
# import data
dataset <- readxl::read_xlsx(
  path = "inst/extdata/MAR4.0 NOR All Data.xlsx"
)


## variable of interest: Discrimination Index
nor <- dataset |>
  dplyr::select(
    Cohort, `Animal ID`, Sex, `Litter ID`, Treat, `Discirmination Index`
  ) |>
  dplyr::mutate(
    discrim_index = as.numeric(`Discirmination Index`),
    Treat = factor(
      Treat,
      levels = c(

      )
    )
  )

## over half of data is NA for discrimination index
