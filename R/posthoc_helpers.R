calculate_se <- function(x1, n1, x2, n2){
  sqrt((x1 / n1) * (1 - (x1 / n1)) / n1 + (x2 / n2) * (1 - (x2 / n2)) / n2)
}

prop_ci <- function(x1, n1, x2, n2, conf.level = 0.95) {
  prop_diff <- (x1 / n1) - (x2 / n2)
  se_diff <- calculate_se(x1, n1, x2, n2)
  z_score <- qnorm(1 - (1 - conf.level) / 2)
  lower <- round(prop_diff - z_score * se_diff, 3)
  upper <- round(prop_diff + z_score * se_diff, 3)
  ci <- paste0("[", lower, ", ", upper, "]")
  return(ci)
}

format_pval_with_star <- function(p_values, threshold = 0.05, digits = 3) {
  formatted <- formatC(round(p_values, digits), format = "f", digits = digits)
  ifelse(p_values < threshold, paste0(formatted, "*"), formatted)
}

#' @title Posthoc Test Helper Function
#'
#' @param dataset Data frame of the data
#' @param var Response variable
#' @param control_group Level of the control group
#'
#' @returns Return the table of posthoc estimation table
#'
#' @export
#'
run_posthoc_test_cat_h1 <- function(data, var, p_list_3, p_list_2, control_group = "Adjuvant+Saline") {
  results_list <- list()

  for (pnd in unique(data$PND)) {
    subset_data <- data %>%
      filter(!is.na(PND), PND == pnd, !is.na(.data[[var]])) %>%
      mutate(across(c(PND, Treatment, Animal), as.factor),
             Treatment = factor(Treatment, levels = unique(Treatment)))

    if (!(control_group %in% unique(subset_data$Treatment))) next

    contingency_table <- table(subset_data$Treatment, subset_data[[var]])

    # **Skip the iteration if all groups have the same outcome (only one column in contingency table)**
    if (ncol(contingency_table) == 1) next

    p_values_inter <- c()
    p_values_add <- c()
    comparisons <- c()
    prop_differences <- c()
    ci_values <- c()
    se <- c()
    treatment_groups <- setdiff(rownames(contingency_table), control_group)

    for (treatment in treatment_groups) {
      test_table <- contingency_table[c(control_group, treatment), , drop = FALSE]
      test_result <- if (any(test_table < 5)) fisher.test(test_table) else chisq.test(test_table)

      # Calculate proportions
      x1 <- test_table[1, 2]  # Count of success in control group
      n1 <- sum(test_table[1, ])  # Total in control group
      x2 <- test_table[2, 2]  # Count of success in treatment group
      n2 <- sum(test_table[2, ])  # Total in treatment group
      prop_diff <- round((x2 / n2) - (x1 / n1), 3)  # Treatment - Control
      se_diff <- calculate_se(x1, n1, x2, n2)
      ci_diff <- prop_ci(x2, n2, x1, n1)

      comparisons <- c(comparisons, paste(treatment, "vs", control_group))
      se <- c(se,se_diff)
      prop_differences <- c(prop_differences, as.character(prop_diff))
      ci_values <- c(ci_values, as.character(ci_diff))
    }

    p_values_inter <- p_list_3 |> filter(PND == pnd) |> dplyr::slice_head(n = 4) |> pull(p.value)
    adjusted_p_values_inter <- p.adjust(p_values_add, method = "hochberg")
    p_values_add <-   p_list_2 |> filter(PND == pnd) |> dplyr::slice_head(n = 4) |> pull(p.value)
    adjusted_p_values_add <- p.adjust(p_values_add, method = "hochberg")

    baseline_result <- data.frame(
      PND = as.character(pnd),
      Group_Comparison = comparisons,
      Proportion_Difference = prop_differences,
      SE = round(se,3),
      Confidence_Interval = ci_values,
      p_inter = p_values_inter |> format_pval_with_star(),
      p_inter_adj = adjusted_p_values_inter |> format_pval_with_star(),
      p_add = p_values_add |> format_pval_with_star(),
      p_add_adj = adjusted_p_values_add |> format_pval_with_star()
    )

    results_list[[as.character(pnd)]] <- baseline_result
  }

  final_results <- bind_rows(results_list)
  if (nrow(final_results) > 0) {
    return(final_results)
  } else {
    return("No valid comparisons due to lack of data.")
  }
}

#' @title Posthoc Test Helper Function
#'
#' @param dataset Data frame of the data
#' @param var Response variable
#' @param control_group Level of the control group
#'
#' @returns Return the table of posthoc estimation table
#'
#' @export
#'
run_posthoc_test_cat_h2 <- function(data, var, p_list_3, p_list_2, control_group = "Adjuvant+Saline") {
  results_list <- list()

  for (pnd in unique(data$PND)) {
    subset_data <- data %>%
      filter(!is.na(PND), PND == pnd, !is.na(.data[[var]])) %>%
      mutate(across(c(PND, Treatment, Animal), as.factor),
             Treatment = factor(Treatment, levels = unique(Treatment)))

    if (!(control_group %in% unique(subset_data$Treatment))) next

    contingency_table <- table(subset_data$Treatment, subset_data[[var]])

    # **Skip the iteration if all groups have the same outcome (only one column in contingency table)**
    if (ncol(contingency_table) == 1) next

    p_values_inter <- c()
    p_values_add <- c()
    comparisons <- c()
    prop_differences <- c()
    ci_values <- c()
    se <- c()
    treatment_groups <- setdiff(rownames(contingency_table), control_group)

    for (i in 1:(length(treatment_groups) - 1)) {
      for (j in (i + 1):length(treatment_groups)) {
        test_table <- contingency_table[c(treatment_groups[i], treatment_groups[j]), , drop = FALSE]
        test_result <- if (any(test_table < 5)) fisher.test(test_table) else chisq.test(test_table)

        # Calculate proportions
        x1 <- test_table[1, 2]  # Count of success in group 1
        n1 <- sum(test_table[1, ])  # Total count in group 1
        x2 <- test_table[2, 2]  # Count of success in group 2
        n2 <- sum(test_table[2, ])  # Total count in group 2
        prop_diff <- round((x1 / n1) - (x2 / n2), 3)
        se_diff <- calculate_se(x1, n1, x2, n2)
        ci_diff <- prop_ci(x1, n1, x2, n2)

        comparisons <- c(comparisons, paste(treatment_groups[i], "vs", treatment_groups[j]))
        se <- c(se,se_diff)
        prop_differences <- c(prop_differences, as.character(prop_diff))  # Convert to character
        ci_values <- c(ci_values, as.character(ci_diff))  # Convert to character
      }
    }

    p_values_inter <- p_list_3 |> filter(PND == pnd) |> dplyr::slice_tail(n = 6) |> pull(p.value)
    adjusted_p_values_inter <- p.adjust(p_values_add, method = "hochberg")
    p_values_add <-   p_list_2 |> filter(PND == pnd) |> dplyr::slice_tail(n = 6) |> pull(p.value)
    adjusted_p_values_add <- p.adjust(p_values_add, method = "hochberg")

    baseline_result <- data.frame(
      PND = as.character(pnd),
      Group_Comparison = comparisons,
      Proportion_Difference = prop_differences,
      SE = round(se,3),
      Confidence_Interval = ci_values,
      p_inter = p_values_inter |> format_pval_with_star(),
      p_inter_adj = adjusted_p_values_inter |> format_pval_with_star(),
      p_add = p_values_add |> format_pval_with_star(),
      p_add_adj = adjusted_p_values_add |> format_pval_with_star()
    )

    results_list[[as.character(pnd)]] <- baseline_result
  }

  final_results <- bind_rows(results_list)
  if (nrow(final_results) > 0) {
    return(final_results)
  } else {
    return("No valid comparisons due to lack of data.")
  }
}


#' @title Posthoc Test Helper Function
#'
#' @param dataset Data frame of the data
#' @param var Response variable
#' @param control_group Level of the control group
#'
#' @returns Return the table of posthoc estimation table
#'
#' @export
#'
run_posthoc_test_num_h1 <- function(data, var, p_list_3, p_list_2, control_group = "Adjuvant+Saline") {

  p_values_inter =
  data <- data %>%
    mutate(across(c(PND, Treatment, Animal), as.factor))

  final_results_list <- list()

  for (var_ in var) {

    results_list <- list()

    for (pnd in unique(data$PND)) {

      subset_data <- data %>%
        filter(PND == pnd & !is.na(.[[var_]]))

      if (length(unique(subset_data$Treatment)) > 1) {
        dunnett_raw <- DescTools::DunnettTest(
          x = subset_data[[var_]] |> unlist(),
          g = subset_data$Treatment |> unlist()
        )[[control_group]] |>
          as.data.frame() |>
          rownames_to_column("Comparison")

        df <- length(unlist(subset_data[[var_]])) - length(unique(subset_data$Treatment))
        t_crit <- qt(1 - 0.05 / 2, df)
        dunnett_raw <- dunnett_raw |>
          mutate(
            SE = (upr.ci - lwr.ci) / (2 * t_crit)
          )

        p_values_inter <- c(p_list_3 |> filter(PND == pnd) |> dplyr::slice_head(n = 4) |> pull(p.value))
        adjusted_p_values_inter <- p.adjust(p_values_add, method = "hochberg")
        p_values_add <-   c(p_list_2 |> filter(PND == pnd) |> dplyr::slice_head(n = 4) |> pull(p.value))
        adjusted_p_values_add <- p.adjust(p_values_add, method = "hochberg")
        dunnett_result <- dunnett_raw |>
          mutate(
            Variable = var_,
            PND = as.character(pnd),
            Mean_Difference = round(diff, 3),
            SE = round(SE,3),
            Confidence_Interval = sprintf("[%.3f, %.3f]", round(lwr.ci, 3), round(upr.ci, 3)),
            p_inter = p_values_inter |> format_pval_with_star(),
            p_inter_adj = adjusted_p_values_inter |> format_pval_with_star(),
            p_add = p_values_add |> format_pval_with_star(),
            p_add_adj = adjusted_p_values_add |> format_pval_with_star()
          ) |>
          select(Variable, PND, Group_Comparison = Comparison, Mean_Diff = Mean_Difference, SE, Confidence_Interval, p_inter, p_inter_adj, p_add, p_add_adj)

        results_list[[as.character(pnd)]] <- dunnett_result
      }
    }

    if (length(results_list) > 0) {
      final_results_list[[var_]] <- bind_rows(results_list)
    }
  }

  final_results <- bind_rows(final_results_list, .id = "Variable")

  if (nrow(final_results) > 0) {
    return(final_results )
  } else {
    return("No valid comparisons due to lack of data.")
  }
}

#' @title Posthoc Test Helper Function
#'
#' @param dataset Data frame of the data
#' @param var Response variable
#' @param control_group Level of the control group
#'
#' @returns Return the table of posthoc estimation table
#'
#' @export
#'
run_posthoc_test_num_h2 <- function(data, var, p_list_3, p_list_2, exclude_control = "Adjuvant+Saline") {

  data <- data %>%
    mutate(across(c(PND, Treatment, Animal), as.factor))

  final_results_list <- list()

  for (var_ in var) {

    results_list <- list()

    for (pnd in unique(data$PND)) {

      subset_data <- data %>%
        filter(PND == pnd & Treatment != exclude_control & !is.na(.[[var_]]))

      if (length(unique(subset_data$Treatment)) > 1) {

        tukey_raw <- TukeyHSD(aov(as.formula(paste0("`", var_, "` ~ Treatment")), data = subset_data))$Treatment |>
          as.data.frame() |>
          rownames_to_column("Comparison")

        df <- length(unlist(subset_data[[var_]])) - length(unique(subset_data$Treatment))
        t_crit <- qt(1 - 0.05 / 2, df)
        tukey_raw <- tukey_raw |>
          mutate(
            SE = (upr - lwr) / (2 * t_crit)
          )

        p_values_inter <- c(p_list_3 |> filter(PND == pnd) |> dplyr::slice_tail(n = 6) |> pull(p.value))
        adjusted_p_values_inter <- p.adjust(p_values_add, method = "hochberg")
        p_values_add <-   c(p_list_2 |> filter(PND == pnd) |> dplyr::slice_tail(n = 6) |> pull(p.value))
        adjusted_p_values_add <- p.adjust(p_values_add, method = "hochberg")

        tukey_result <- tukey_raw |>
          mutate(
            Variable = var_,
            PND = as.character(pnd),
            Mean_Difference = round(diff, 3),
            SE = round(SE,3),
            Confidence_Interval = sprintf("[%.3f, %.3f]", round(lwr, 3), round(upr, 3)),
            p_inter = p_values_inter |> format_pval_with_star(),
            p_inter_adj = adjusted_p_values_inter |> format_pval_with_star(),
            p_add = p_values_add |> format_pval_with_star(),
            p_add_adj = adjusted_p_values_add |> format_pval_with_star()
          ) |>
          select(Variable, PND, Group_Comparison = Comparison, Mean_Diff = Mean_Difference, SE, Confidence_Interval, p_inter, p_inter_adj, p_add, p_add_adj)

        results_list[[as.character(pnd)]] <- tukey_result
      }
    }

    if (length(results_list) > 0) {
      final_results_list[[var_]] <- bind_rows(results_list)
    }
  }

  final_results <- bind_rows(final_results_list, .id = "Variable")

  if (nrow(final_results) > 0) {
    return(final_results )
  } else {
    return("No valid comparisons due to lack of data.")
  }
}
