#' Create Barplot of Means with 95% CI and Significance Annotations
#'
#' @param dataset A data.frame with input data.
#' @param variable The outcome variable (string).
#' @param treatment The treatment/group variable (string).
#' @param strata Optional stratification variable for faceting (string).
#' @param contrast_p Optional contrast table with `contrast` and `p.value` columns.
#' @param xlab Label for x-axis. Defaults to treatment.
#' @param ylab Label for y-axis. Defaults to variable.
#'
#' @return A ggplot object.
#' @export
create_meanplot <- function(
    dataset,
    variable,
    treatment = "Treatment",
    strata = NULL,
    contrast_p = NULL,
    xlab = treatment,
    ylab = variable
) {
  var_sym <- rlang::sym(variable)
  treatment_sym <- rlang::sym(treatment)

  # Compute summary: mean ± 95% CI
  if (is.null(strata)) {
    summary_data <- dataset |>
      dplyr::group_by(!!treatment_sym) |>
      dplyr::summarise(
        mean = mean(!!var_sym, na.rm = TRUE),
        se = stats::sd(!!var_sym, na.rm = TRUE) / sqrt(dplyr::n()),
        .groups = "drop"
      )
  } else {
    strata_sym <- rlang::sym(strata)
    summary_data <- dataset |>
      dplyr::group_by(!!treatment_sym, !!strata_sym) |>
      dplyr::summarise(
        mean = mean(!!var_sym, na.rm = TRUE),
        se = stats::sd(!!var_sym, na.rm = TRUE) / sqrt(dplyr::n()),
        .groups = "drop"
      )
  }

  summary_data <- summary_data |>
    dplyr::mutate(
      ci_lower = mean - 1.96 * se,
      ci_upper = mean + 1.96 * se
    )

  # Build base plot
  p <- ggplot2::ggplot(summary_data, ggplot2::aes(x = !!treatment_sym, y = mean)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
      width = 0.2
    ) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # Facet if needed
  if (!is.null(strata)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(!!strata_sym))
  }

  # Add significance bars if provided
  if (!is.null(contrast_p)) {
    # Use helper to convert to plotting format
    pval_df <- get_significant_p_value(
      contrast_table = contrast_p,
      y_start = max(summary_data$ci_upper, na.rm = TRUE) * 1.05,
      y_step = (max(summary_data$ci_upper, na.rm = TRUE) -
        min(summary_data$ci_lower, na.rm = TRUE)) * 0.1
    )
    print(pval_df)
    p <- p +
      ggpubr::stat_pvalue_manual(
        pval_df,
        label = "p.signif",
        tip.length = 0.01,
        bracket.size = 0.6
      ) +
      ggplot2::scale_y_continuous(
        expand = c(0, 0), limits = c(
          min(summary_data$ci_lower, na.rm = TRUE) * 0.95,
        max(pval_df$y.position)*1.05))
  }

  return(p)
}
