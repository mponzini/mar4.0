#' plot: barplot using raw data: mean +/- SE
#' @title Create Barplots
#' @param dataset Data.frame of analytic data
#' @param variable Outcome variable to plot
#' @param treatment Treatment variable name to summarize `variable`  by
#' @param strata Option variable to stratify figure by
#' @param xlab Character string to label x-axis. Defaults to `treatment`
#' @param ylab Character string to label y-axis. Defaults to `variable`
#'
#' @export

create_barplot <- function(
    dataset, variable, treatment = 'Treatment', strata = NULL,
    xlab = treatment, ylab = variable
) {
  summary_data <- dataset |>
    dplyr::summarise(
      N = dplyr::n(),
      Mean = mean(.data[[variable]], na.rm = TRUE),
      SD = sd(.data[[variable]], na.rm = TRUE),
      SE = SD / sqrt(N),
      CI.Lower = Mean - (qt(0.975, N - 1) * SE),
      CI.Upper = Mean + (qt(0.975, N - 1) * SE),
      .by = dplyr::any_of(c(treatment, strata))
    )

  ref_data <- summary_data |>
    dplyr::filter(.data[[treatment]] == "Adjuvant+Saline") |>
    dplyr::select(
      Mean, dplyr::any_of(c(strata))
    )

  if (!is.null(strata)) {
    plot <- summary_data |>
      ggplot2::ggplot() +
      ggplot2::aes(
        x = .data[[treatment]],
        y = Mean,
        ymin = CI.Lower,
        ymax = CI.Upper,
        fill = .data[[treatment]]
      ) +
      ggplot2::geom_col(width = 0.75) +
      ggplot2::geom_errorbar(width = 0.33) +
      ggplot2::geom_hline(
        data = ref_data,
        ggplot2::aes(
          yintercept = summary_data |>
            dplyr::filter(.data[[treatment]] == "Adjuvant+Saline") |>
            dplyr::pull(Mean)
        ),
        linetype = "dashed"
      ) +
      ggplot2::facet_wrap(. ~ .data[[strata]]) +
      ggplot2::labs(
        x = xlab,
        y = ylab,
        fill = xlab
      ) +
      ggplot2::theme_classic()
  } else {
    plot <- summary_data |>
      ggplot2::ggplot() +
      ggplot2::aes(
        x = .data[[treatment]],
        y = Mean,
        ymin = CI.Lower,
        ymax = CI.Upper,
        fill = .data[[treatment]]
      ) +
      ggplot2::geom_col(width = 0.75) +
      ggplot2::geom_errorbar(width = 0.33) +
      ggplot2::geom_hline(
        data = ref_data,
        ggplot2::aes(
          yintercept = summary_data |>
            dplyr::filter(.data[[treatment]] == "Adjuvant+Saline") |>
            dplyr::pull(Mean)
        ),
        linetype = "dashed"
      ) +
      ggplot2::labs(
        x = xlab,
        y = ylab,
        fill = xlab
      ) +
      ggplot2::theme_classic()
  }

  return(plot)
}
