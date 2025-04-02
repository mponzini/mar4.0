#' @title Developmental Milestons Time-to-event analysis
#'
#' @param dataset Data.frame
#' @param var_list Vector of milestone variables with censoring.
#' Default = c("Negative Geotaxis", "Cliff Avoidance", "Circle Traverse")
#'
#' @export

time_to_event <- function(
    dataset,
    var_list = c("Negative Geotaxis", "Cliff Avoidance", "Circle Traverse")
) {

  surv_models <- lapply(
    var_list,
    function(x) {
      data <- dataset |>
        dplyr::select(
          `Animal ID`, `Litter ID`, PND, Treatment, Sex,
          dplyr::all_of(x)
        ) |>
        na.omit() |>
        dplyr::mutate(
          censor = ifelse(!!sym(x) < 30, 1, 0)
        ) |>
        dplyr::rename(outcome = x)

      pnd_data <- split(data, data$PND)

      tmp <- lapply(
        pnd_data,
        function(x) {
          coxph(Surv(outcome, censor) ~ Treatment + Sex + `Litter ID`, data = x)
        }
      )
      return(tmp)
    }
  )
  names(surv_models) <- var_list

  return(surv_models)

}

# test <- time_to_event(dataset = data, var_list = c("Negative Geotaxis", "Circle Traverse", "Cliff Avoidance"))
# test |> View()
