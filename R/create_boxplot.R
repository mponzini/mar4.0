#' Create Boxplots with Group Means ± CI and Significance Bars
#'
#' @title Create Boxplots with Significance Annotations
#' @param dataset Data.frame of analytic data
#' @param variable Outcome variable to plot
#' @param treatment Treatment variable name to summarize `variable` by
#' @param strata Optional variable to stratify figure by
#' @param contrast_p Data frame of contrast results (from emmeans)
#' @param xlab Character string to label x-axis. Defaults to `treatment`
#' @param ylab Character string to label y-axis. Defaults to `variable`
#' @export
create_boxplot <- function(
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

  # Basic ggplot
  p <- ggplot2::ggplot(dataset, ggplot2::aes(x = !!treatment_sym, y = !!var_sym)) +
    ggplot2::geom_boxplot(width = 0.5, outlier.shape = NA, fill = "grey90", color = "black") +
    ggplot2::stat_summary(
      fun = mean, fun.min = function(x) mean(x) - 1.96 * sd(x)/sqrt(length(x)),
      fun.max = function(x) mean(x) + 1.96 * sd(x)/sqrt(length(x)),
      geom = "pointrange", color = "yellow", size = 0.3
    ) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  # Add faceting if requested
  if (!is.null(strata)) {
    strata_sym <- rlang::sym(strata)
    p <- p + ggplot2::facet_wrap(ggplot2::vars(!!strata_sym))
  }

  # Add significance bars if contrast table is provided
  if (!is.null(contrast_p)) {
    # Generate annotation table
    max_y <- max(dataset[[variable]], na.rm = TRUE)
    p_values <- get_significant_p_value(
      contrast_table = contrast_p,
      y_start = max_y * 1.05,
      y_step = max_y / 50
    )

    suppressMessages(
      p <- p + ggpubr::stat_pvalue_manual(
        p_values,
        label = "p.signif",
        tip.length = 0.01,
        bracket.size = 0.6
      ) +
        ggplot2::scale_y_continuous(expand = c(0.01, 0), limits = c(0, max(p_values$y.position) * 1.1))
    )
  }

  return(p)
}
