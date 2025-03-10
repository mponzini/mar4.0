# https://www.datanovia.com/en/lessons/mixed-anova-in-r/#two-way-mixed
library(mar4.0)

# update summary statistics Tx*Sex*PND
# Example using Count, repeat for each Outcome measurement
summary_tbls <- function(
    dataset, outcome
) {

  tbl_male <- dataset |>
    dplyr::filter(Sex == "M") |>
    dplyr::select(Treatment, PND, dplyr::all_of(outcome)) |>
    gtsummary::tbl_strata(
      strata = PND,
      .tbl_fun = ~ .x |>
        gtsummary::tbl_summary(
          by = Treatment,
          type = where(is.numeric) ~ "continuous2",
          statistic = gtsummary::all_continuous2() ~ c(
            "{mean} ({sd})", "{median} ({p25}, {p75})", "{min} - {max}"
          )
        ),
      .combine_with = "tbl_stack"
    ) |>
    gtsummary::modify_spanning_header(
      gtsummary::all_stat_cols() ~ "Males"
    )

  tbl_female <- dataset |>
    dplyr::filter(Sex == "F") |>
    dplyr::select(Treatment, PND, dplyr::all_of(outcome)) |>
    gtsummary::tbl_strata(
      strata = PND,
      .tbl_fun = ~ .x |>
        gtsummary::tbl_summary(
          by = Treatment,
          type = where(is.numeric) ~ "continuous2",
          statistic = gtsummary::all_continuous2() ~ c(
            "{mean} ({sd})", "{median} ({p25}, {p75})", "{min} - {max}"
          )
        ) |> gtsummary::add_overall(),
      .combine_with = "tbl_stack"
    ) |>
    gtsummary::modify_spanning_header(
      gtsummary::all_stat_cols() ~ "Females"
    )

  ans <- list(
    "males" = tbl_male,
    "females" = tbl_female
  )

  return(ans)
}



# make sure we code variables as factors (update in data prep script?)
usv_fct <- usv |>
  dplyr::mutate(
    Treatment = factor(Treatment, levels = c(
      "Adjuvant+Saline", "CRMP1+CRMP2", "CRMP1+GDA",
      "LDHA+LDHB+CRMP1+STIP1", "STIP1+NSE"
    )),
    Sex = factor(Sex, levels = c("M", "F")),
    Litter = factor(`Litter ID`),
    Animal = factor(`Animal ID`)
  )

# need to reduce to complete cases
usv_fct_compete <- usv_fct |>
  dplyr::filter(
    dplyr::if_all(
      .cols = c(Animal, Litter, Treatment, Sex, PND, Count),
      ~ !is.na(.x)
    )
  )
# use this one
mix_aov <- rstatix::anova_test(
  dv = Count,
  wid = Animal,
  between = c(Treatment, Sex),
  within = PND,
  data = usv_fct_compete,
  type = 3
)
