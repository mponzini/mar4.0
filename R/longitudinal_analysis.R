#' Analyze Longitudinal MAR data
#'
#' @param dataset A [data.frame()] containing the outcome of interest in long format
#' @param outcome Name of the outcome variable
#'
#' @returns What does this return?
#'
#' @export
#'
#'


longitudinal_analysis <- function(dataset, outcome){
  outcome_class <- class(dataset[[outcome]])

  if(outcome_class %in% c("numeric")){
    longitudinal_continuous(dataset = dataset, outcome == outcome)
  } else{
    longitudinal_binary(dataset = dataset, outcome = outcome)
  }
}
