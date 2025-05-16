#' Create Barplots (Mean ± SE) Using Raw Data
#'
#' @title Create Barplots
#' @param dataset Data.frame of analytic data
#' @param variable Outcome variable to plot
#' @param treatment Treatment variable name to summarize `variable` by
#' @param strata Optional variable to stratify figure by
#' @param xlab Character string to label x-axis. Defaults to `treatment`
#' @param ylab Character string to label y-axis. Defaults to `variable`
#' @export
create_barplot <- function(
    dataset,
    variable,
    treatment = "Treatment",
    strata = NULL,
    xlab = treatment,
    ylab = variable
) {
  var_sym <- rlang::sym(variable)
  treatment_sym <- rlang::sym(treatment)

  if (is.null(strata)) {
    summary_data <- dataset |>
      dplyr::group_by(!!treatment_sym) |>
      dplyr::summarise(
        mean_value = mean(!!var_sym, na.rm = TRUE),
        se = stats::sd(!!var_sym, na.rm = TRUE) / sqrt(dplyr::n()),
        .groups = "drop"
      )
  } else {
    strata_sym <- rlang::sym(strata)
    summary_data <- dataset |>
      dplyr::group_by(!!treatment_sym, !!strata_sym) |>
      dplyr::summarise(
        mean_value = mean(!!var_sym, na.rm = TRUE),
        se = stats::sd(!!var_sym, na.rm = TRUE) / sqrt(dplyr::n()),
        .groups = "drop"
      )
  }

  # Get unique treatment levels
  treatment_levels <- unique(summary_data[[treatment]])
  max_y <- max(summary_data$mean_value + summary_data$se, na.rm = TRUE) * 1.5

  # Assign unique patterns
  pattern_options <- c("none", "stripe", "crosshatch", "circle", "pch")
  pattern_values <- setNames(pattern_options[seq_along(treatment_levels)], treatment_levels)

  # Assign random angles
  set.seed(123)  # for reproducibility
  angle_pool <- seq(0, 135, by = 15)
  angle_values <- setNames(sample(angle_pool, length(treatment_levels), replace = TRUE), treatment_levels)

  # Assign grey pattern fill for all
  pattern_fill_values <- setNames(rep("grey50", length(treatment_levels)), treatment_levels)

  # Assign white bar base fill for all
  fill_values <- setNames(rep("white", length(treatment_levels)), treatment_levels)

  # Start building plot
  p <- ggplot2::ggplot(summary_data, ggplot2::aes(x = !!treatment_sym, y = mean_value)) +
    ggpattern::geom_col_pattern(
      ggplot2::aes(
        pattern = !!treatment_sym,
        pattern_angle = !!treatment_sym,
        pattern_fill = !!treatment_sym,
        fill = !!treatment_sym
      ),
      pattern_colour = "black",
      colour = "black",
      pattern_density = 0.35,
      pattern_spacing = 0.05,
      width = 0.6,
      show.legend = TRUE
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_value - se, ymax = mean_value + se),
      width = 0.5
    ) +
    ggpattern::scale_pattern_manual(values = pattern_values) +
    ggpattern::scale_pattern_fill_manual(values = pattern_fill_values) +
    ggpattern::scale_pattern_angle_manual(values = angle_values) +
    ggplot2::scale_fill_manual(values = fill_values) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::guides(
      fill = "none",
      pattern = ggplot2::guide_legend(override.aes = list(
        fill = "white",
        colour = "black",
        pattern_fill = "grey50",
        pattern_colour = "black",
        pattern_density = 0.35,
        pattern_spacing = 0.05,
        pattern_angle = 45
      ))
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, max_y))

  # Add faceting if applicable
  if (!is.null(strata)) {
    strata_sym <- rlang::sym(strata)
    p <- p + ggplot2::facet_wrap(ggplot2::vars(!!strata_sym))
  }

  # Add reference line for "Adjuvant+Saline" if it exists
  if ("Adjuvant+Saline" %in% summary_data[[treatment]]) {
    ref_value <- summary_data |>
      dplyr::filter(.data[[treatment]] == "Adjuvant+Saline") |>
      dplyr::pull(mean_value)

    p <- p + ggplot2::geom_hline(
      yintercept = ref_value,
      linetype = "dashed",
      color = "red"
    )
  }

  return(p)
}
