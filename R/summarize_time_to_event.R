#' @title Summarize the Time-to-event results
#'
#' @param result_list A list containing the time-to-event results for each
#' measurement at each PND
#'
#' @export

summarize_time_to_event <- function(result_list) {

  var_list <- names(result_list)

  surv_tbls <- lapply(
    var_list,
    function(x) {
      gtsummary::tbl_stack(
        tbls = list(
          result_list[[x]][["4"]] |>
            gtsummary::tbl_regression(exponentiate = TRUE) |>
            gtsummary::add_global_p(keep = TRUE),
          result_list[[x]][["8"]] |>
            gtsummary::tbl_regression(exponentiate = TRUE) |>
            gtsummary::add_global_p(keep = TRUE),
          result_list[[x]][["12"]] |>
            gtsummary::tbl_regression(exponentiate = TRUE) |>
            gtsummary::add_global_p(keep = TRUE)
        ),
        group_header = c("PND 4", "PND 8", "PND 12")
      )
    }
  )

  names(surv_tbls) <- paste(var_list)

  return(surv_tbls)

}


