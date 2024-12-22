#' @title Exclude Cross-Fostered Animals
#'
#' @param dataset Data frame of the data
#' @param subj_id Character identifying the subject ID variable
#' @param cross_fostered Vector of cross-fostered animals
#'
#' @export
#'

exclude_cross_fostered <- function(
    dataset,
    subj_id = 'Animal ID',
    cross_fostered = c("9_31_1", "9_31_2", "9_31_3", "9_31_4")
){
  dataset |>
    dplyr::filter(!(!!sym(subj_id) %in% cross_fostered))
}
