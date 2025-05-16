#' Convert Contrast Table to p-value Data Frame for Plotting
#'
#' Converts a pairwise contrast result table into a format usable by
#' `ggpubr::stat_pvalue_manual()` to display statistical comparisons
#' on ggplot2 bar plots.
#'
#' @param contrast_table A data frame with a `contrast` column like
#'   "(A) - (B)" and a `p.value` column.
#' @param y_start Starting value for the y.position of annotations.
#' @param y_step Step size to increment y.position to avoid overlaps.
#'
#' @return A data frame with columns: `group1`, `group2`, `y.position`,
#'   `p.adj`, and `p.signif`.
#' @export
#'
#' @examples
#' contrast_df <- data.frame(
#'   contrast = c("(A) - (B)", "(B) - (C)"),
#'   p.value = c(0.04, 0.001)
#' )
#' get_significant_p_value(contrast_df)

get_significant_p_value <- function(contrast_table,
                                    y_start = 0.05,
                                    y_step = 0.05) {
  contrast_table <- contrast_table |> filter(p.value < 0.1)
  # Split contrasts into group1 and group2
  groups_cleaned <- gsub("[()]", "", contrast_table$contrast)
  groups_split <- strsplit(groups_cleaned, " - ")

  y_positions <- seq(y_start, by = y_step, length.out = nrow(contrast_table))

  # Convert numeric p-values to significance stars
  stars <- function(p) {
    ifelse(p < 0.001, "***",
    ifelse(p < 0.01, "**",
    ifelse(p < 0.05, "*",
    ifelse(p < 0.1, ".", "ns"))))
  }

  p_values <- data.frame(
    group1 = sapply(groups_split, `[`, 1),
    group2 = sapply(groups_split, `[`, 2),
    y.position = y_positions,
    p.adj = contrast_table$p.value,
    p.signif = stars(contrast_table$p.value),
    stringsAsFactors = FALSE
  )

  return(p_values)
}
