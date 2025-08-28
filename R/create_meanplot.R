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
    strata = "Sex",
    contrast_p = NULL,
    contrast_p_sex = NULL,
    xlab = treatment,
    ylab = variable
) {
  # Rename variable to y_plot for safe handling
  dataset <- dataset |>
    dplyr::rename(y_plot = !!rlang::sym(variable)) |>
    dplyr::mutate(y_plot = as.numeric(as.character(.data$y_plot))) |>
    dplyr::filter(!is.na(y_plot)) |>
    dplyr::mutate(
      !!treatment :=
        factor(
          .data[[treatment]],
          levels = c("LDHA/B+CRMP1+STIP1", "CRMP1+CRMP2", "CRMP1+GDA", "STIP1+NSE", "Control"),
          ordered = TRUE,
          labels = c("LDHA/B+CRMP1+STIP1", "CRMP1+CRMP2", "CRMP1+GDA", "STIP1+NSE", "Control")
        ),
      !!strata :=
        factor(.data[[strata]],
               levels = c("M", "F"),
               ordered = TRUE,
               labels = c("M", "F"))
    ) #|>
    # dplyr::filter(PND == 12)

  has_PND <- "PND" %in% names(dataset)
  # has_PND <- F
  # Plot
  plot <- ggpubr::ggerrorplot(
    dataset,
    x = treatment,
    y = "y_plot",
    desc_stat = "mean_ci",
    color = strata,
    shape = strata,
    palette = c("#2B6CB0", "#E53E3E"),
    position = ggplot2::position_dodge(0.5),
    xlab = xlab,
    ylab = ylab,
    error.plot = "errorbar",
    add = "mean",
    add.params = list(size = 0.3),
    width = 0.5
  ) +
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))

  # Conditionally add facet_wrap if 'PND' column exists
  if (has_PND) {
    plot <- plot + ggplot2::facet_wrap(~PND, nrow = 1, labeller = label_both)
  }

  # Calculate summary data for error bars
  if (has_PND) {
    summary_data <- dataset |>
      dplyr::group_by(.data[[treatment]], .data[[strata]], PND) |>
      dplyr::summarise(
        mean = mean(.data$y_plot, na.rm = TRUE),
        se = stats::sd(.data$y_plot, na.rm = TRUE) / sqrt(dplyr::n()),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        lower = mean - 1.96 * se,
        upper = mean + 1.96 * se
      )
  } else {
    summary_data <- dataset |>
      dplyr::group_by(.data[[treatment]], .data[[strata]]) |>
      dplyr::summarise(
        mean = mean(.data$y_plot, na.rm = TRUE),
        se = stats::sd(.data$y_plot, na.rm = TRUE) / sqrt(dplyr::n()),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        lower = mean - 1.96 * se,
        upper = mean + 1.96 * se
      )
  }

  if (has_PND) {
    y_step_vec <- summary_data |>
      dplyr::mutate(y_step = (max(upper, na.rm = TRUE) - min(lower, na.rm = TRUE)) * 0.07) |>
      dplyr::group_by(PND) |>
      dplyr::summarise(
        y_step = max(y_step, (max(upper, na.rm = TRUE) - min(lower, na.rm = TRUE)) * 0.1),
        .groups = "drop"
      )
  } else {
    y_step_vec <- summary_data |>
      dplyr::mutate(y_step = (max(upper, na.rm = TRUE) - min(lower, na.rm = TRUE)) * 0.07) |>
      dplyr::summarise(
        y_step = max(y_step, (max(upper, na.rm = TRUE) - min(lower, na.rm = TRUE)) * 0.1)
      )
  }

  summary_data |>
    dplyr::mutate(
      !!rlang::sym(treatment) :=
        factor(
          .data[[treatment]],
          levels = sort(unique(as.character(.data[[treatment]]))),
          ordered = TRUE
        )
    )

  # Step 3: Add p-values across treatment if provided
  if (!is.null(contrast_p) && nrow(dplyr::filter(contrast_p, p.adj < 0.05)) > 0) {
    pval_df_treat <- get_significant_p_value(
      contrast_table = contrast_p,
      y_start = NA,
      y_step = NA
    )
    if (has_PND && "PND" %in% colnames(pval_df_treat)) {
      pval_df_treat <- pval_df_treat |>
        dplyr::left_join(y_step_vec, by = "PND") |>
        dplyr::group_by(PND) |>
        dplyr::mutate(
          y.position = max(summary_data$upper[summary_data$PND == unique(PND)], na.rm = TRUE) +
            (dplyr::row_number()) * y_step
        ) |>
        dplyr::ungroup()
      pval_df_treat$PND <- factor(pval_df_treat$PND, levels = c(4, 8, 12), ordered = TRUE)
    } else {
      pval_df_treat <- pval_df_treat |>
        dplyr::mutate(
          y.position = max(summary_data$upper, na.rm = TRUE) +
            (dplyr::row_number()) * y_step_vec$y_step[1]
        )
    }

    pval_df_treat <- pval_df_treat |>
      dplyr::mutate(
        xmin = .data$group1,
        xmax = .data$group2,
        groups = purrr::pmap(list(group1 = .data$group1, group2 = .data$group2), c),
        .y. = "Treatment",
        p = .data$p.adj
      )

    plot <- plot +
      ggprism::add_pvalue(
        pval_df_treat,
        label = "p.signif",
        tip.length = 0.003,
        facet = if (has_PND) "PND" else NULL
      )
  }

  # Calculate upper for y.position for each treatment
  if (has_PND) {
    summary_data_ <- summary_data |>
      dplyr::group_by(PND, .data[[treatment]]) |>
      dplyr::summarise(upper = max(.data$upper, na.rm = TRUE), .groups = "drop")
  } else {
    summary_data_ <- summary_data |>
      dplyr::group_by(.data[[treatment]]) |>
      dplyr::summarise(upper = max(.data$upper, na.rm = TRUE), .groups = "drop")
  }

  # Step 4: Add p-values across sex if provided
  if (!is.null(contrast_p_sex) && nrow(dplyr::filter(contrast_p_sex, p.adj < 0.05)) > 0) {
    pval_df_sex <- contrast_p_sex |>
      dplyr::mutate(
        !!rlang::sym(treatment) :=
          factor(
            .data[[treatment]],
            levels = sort(unique(as.character(.data[[treatment]]))),
            ordered = TRUE
          )
      ) |>
      dplyr::mutate(
        group1 = "M",
        group2 = "F",
        p.signif = stars(.data$p.adj),
        Treatment = factor(Treatment, levels = levels(summary_data_$Treatment), ordered = TRUE)
      ) |>
      dplyr::left_join(summary_data_, by = if (has_PND) c("PND", "Treatment" = treatment) else c("Treatment" = treatment)) |>
      dplyr::left_join(y_step_vec, by = if (has_PND) "PND" else character()) |>
      dplyr::mutate(
        y.position = .data$upper + if (has_PND) y_step * 0.3 else y_step_vec$y_step[1],
        xmin = .data$Treatment,
        xmax = .data$Treatment
      ) |>
      dplyr::mutate(
        xmin = as.numeric(factor(Treatment)) - 0.12,
        xmax = as.numeric(factor(Treatment)) + 0.12
      ) |>
      dplyr::filter(p.adj < 0.05)

    if (has_PND && "PND" %in% colnames(pval_df_sex)) {
      pval_df_sex$PND <- factor(pval_df_sex$PND, levels = c(4, 8, 12), ordered = TRUE)
    }

    plot <- plot +
      ggprism::add_pvalue(
        pval_df_sex,
        xmin = "xmin",
        xmax = "xmax",
        label = "p.signif",
        y.position = "y.position",
        tip.length = 0.003,
        facet = if (has_PND) "PND" else NULL
      )
  }

  return(plot)
}




