#' Convert Contrast Table to p-value Data Frame for Plotting (Grouped by PND if available)
#'
#' @param contrast_table A data frame with a `contrast` column like
#'   "(A) - (B)", a `p.adj` column, and optionally a `PND` column.
#' @param y_start Starting value for the y.position of annotations (default 0.05).
#' @param y_step Step size to increment y.position to avoid overlaps (default 0.05).
#'
#' @return A data frame with columns: `group1`, `group2`, `y.position`,
#'   `p.adj`, `p.signif`, and optionally `PND`.
#' @export
get_significant_p_value <- function(
    contrast_table,
    y_start = 0.05,
    y_step = 0.05
) {
  # Only keep significant comparisons
  contrast_table <- dplyr::filter(contrast_table, p.adj < 0.05)

  # Parse group1/group2 from contrast
  groups_cleaned <- gsub("[()]", "", contrast_table$contrast)
  groups_split <- strsplit(groups_cleaned, " - ")

  # If PND exists, group by PND for y.position
  if ("PND" %in% colnames(contrast_table)) {
    contrast_table$group1 <- sapply(groups_split, `[`, 1)
    contrast_table$group2 <- sapply(groups_split, `[`, 2)

    p_values <- contrast_table |>
      dplyr::group_by(PND) |>
      dplyr::mutate(
        y.position = y_start + (dplyr::row_number() - 1) * y_step,
        p.signif = stars(.data$p.adj)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(PND, group1, group2, y.position, p.adj, p.signif)
  } else {
    y_positions <- seq(y_start, by = y_step, length.out = nrow(contrast_table))
    p_values <- data.frame(
      group1 = sapply(groups_split, `[`, 1),
      group2 = sapply(groups_split, `[`, 2),
      y.position = y_positions,
      p.adj = contrast_table$p.adj,
      p.signif = stars(contrast_table$p.adj),
      stringsAsFactors = FALSE
    )
  }

  return(p_values)
}


#' Adjust p-values by group (e.g., PND) using specified method
#'
#' @param data Data frame containing p-values and group column
#' @param p_col Name of the column with raw p-values (default: "p.value")
#' @param group_col Name of the grouping column (default: "PND")
#' @param method Adjustment method (default: "hochberg")
#'
#' @return Data frame with an added column p.adj (adjusted p-values)
#' @export
adjust_p_by_pnd <- function(
    data,
    p_col = "p.value",
    group_col = "PND",
    contrast_col = "contrast",
    control = "Adjuvant+Saline",
    method = "hochberg"
) {
  stopifnot(all(c(p_col, contrast_col) %in% names(data)))
  group_ok <- !is.null(group_col) && group_col %in% names(data)

  out <- data %>%
    dplyr::mutate(
      is_control_contrast = stringr::str_detect(.data[[contrast_col]], "Adjuvant\\+Saline") &
        !stringr::str_detect(.data[[contrast_col]], "^\\(Adjuvant\\+Saline\\) - \\(Adjuvant\\+Saline\\)$")
    )

  if (group_ok) {
    out <- out %>%
      dplyr::group_by(.data[[group_col]], is_control_contrast) %>%
      dplyr::mutate(
        p.adj = p.adjust(.data[[p_col]], method = method)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(!!sym(group_col), !!sym(contrast_col))
  } else {
    out <- out %>%
      dplyr::group_by(is_control_contrast) %>%
      dplyr::mutate(
        p.adj = p.adjust(.data[[p_col]], method = method)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(!!sym(contrast_col))
  }
  out
}

