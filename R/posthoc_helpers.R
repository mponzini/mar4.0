#' @title Posthoc Test Helper Function
#'
#' @param dataset Data frame of the data
#' @param var Response variable
#' @param control_group Level of the control group
#'
#' @export
#'


prop_ci <- function(x1, n1, x2, n2, conf.level = 0.95) {
  prop_diff <- (x1 / n1) - (x2 / n2)
  se_diff <- sqrt((x1 / n1) * (1 - (x1 / n1)) / n1 + (x2 / n2) * (1 - (x2 / n2)) / n2)
  z_score <- qnorm(1 - (1 - conf.level) / 2)
  lower <- round(prop_diff - z_score * se_diff, 3)
  upper <- round(prop_diff + z_score * se_diff, 3)
  ci <- paste0("[", lower, ", ", upper, "]")
  return(ci)
}

run_posthoc_test_cat_h1 <- function(data, var, control_group = "Adjuvant+Saline") {
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

    p_values <- c()
    comparisons <- c()
    prop_differences <- c()
    ci_values <- c()

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
      ci_diff <- prop_ci(x2, n2, x1, n1)

      p_values <- c(p_values, test_result$p.value)
      comparisons <- c(comparisons, paste(treatment, "vs", control_group))
      prop_differences <- c(prop_differences, as.character(prop_diff))
      ci_values <- c(ci_values, as.character(ci_diff))
    }

    adjusted_p_values <- p.adjust(p_values, method = "holm")

    baseline_result <- data.frame(
      PND = as.character(pnd),
      Group_Comparison = comparisons,
      Proportion_Difference = prop_differences,
      Confidence_Interval = ci_values,
      P_Value = ifelse(adjusted_p_values < 0.05,
                       paste0(formatC(round(adjusted_p_values, 3), format = "f", digits = 3), " *"),
                       formatC(round(adjusted_p_values, 3), format = "f", digits = 3))
    )

    results_list[[as.character(pnd)]] <- baseline_result
  }

  final_results <- bind_rows(results_list)
  if (nrow(final_results) > 0) {
    return(final_results |> flextable() |> set_table_properties(layout = "autofit"))
  } else {
    return("No valid comparisons due to lack of data.")
  }
}
run_posthoc_test_cat_h2 <- function(data, var, control_group = "Adjuvant+Saline") {
  results_list <- list()

  for (pnd in unique(data$PND)) {
    subset_data <-  data %>%
      filter(!is.na(PND), PND == pnd, Treatment != control_group, !is.na(.data[[var]])) %>%
      mutate(across(c(PND, Treatment, Animal), as.factor),
             Treatment = factor(Treatment, levels = unique(Treatment)))

    if (length(unique(subset_data$Treatment)) <= 1) next

    contingency_table <- table(subset_data$Treatment, subset_data[[var]])

    # **Skip the iteration if all groups have the same outcome (only one column in contingency table)**
    if (ncol(contingency_table) == 1) next

    p_values <- c()
    comparisons <- c()
    prop_differences <- c()
    ci_values <- c()

    treatment_groups <- rownames(contingency_table)

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
        ci_diff <- prop_ci(x1, n1, x2, n2)

        p_values <- c(p_values, test_result$p.value)
        comparisons <- c(comparisons, paste(treatment_groups[i], "vs", treatment_groups[j]))
        prop_differences <- c(prop_differences, as.character(prop_diff))  # Convert to character
        ci_values <- c(ci_values, as.character(ci_diff))  # Convert to character
      }
    }

    adjusted_p_values <- p.adjust(p_values, method = "holm")

    treatment_result <- data.frame(
      PND = as.character(pnd),
      Group_Comparison = comparisons,
      Proportion_Difference = prop_differences,
      Confidence_Interval = ci_values,
      P_Value = ifelse(adjusted_p_values < 0.05,
                       paste0(formatC(round(adjusted_p_values, 3), format = "f", digits = 3), " *"),
                       formatC(round(adjusted_p_values, 3), format = "f", digits = 3))
    )

    results_list[[as.character(pnd)]] <- treatment_result
  }

  final_results <- bind_rows(results_list)
  if (nrow(final_results) > 0) {
    return(final_results |> flextable() |> set_table_properties(layout = "autofit"))
  } else {
    return("No valid comparisons due to lack of data.")
  }
}

run_posthoc_test_num_h1 <- function(data, vars, control_group = "Adjuvant+Saline") {

  data <- data %>%
    mutate(across(c(PND, Treatment, Animal), as.factor))

  final_results_list <- list()

  for (var in vars) {

    results_list <- list()

    for (pnd in unique(data$PND)) {

      subset_data <- data %>%
        filter(PND == pnd & !is.na(.[[var]]))

      if (length(unique(subset_data$Treatment)) > 1) {
        dunnett_raw <- DescTools::DunnettTest(
          x = subset_data[[var]] |> unlist(),
          g = subset_data$Treatment |> unlist()
        )[[control_group]] |>
          as.data.frame() |>
          rownames_to_column("Comparison")

        adjusted_p_values <- p.adjust(dunnett_raw$pval, method = "bonferroni")

        dunnett_result <- dunnett_raw |>
          mutate(
            Variable = var,
            PND = as.character(pnd),
            Effect_Size = round(diff, 3),
            Confidence_Interval = sprintf("[%.3f, %.3f]", round(lwr.ci, 3), round(upr.ci, 3)),
            P_Value = ifelse(adjusted_p_values < 0.05,
                             paste0(formatC(round(adjusted_p_values, 3), format = "f", digits = 3), " *"),
                             formatC(round(adjusted_p_values, 3), format = "f", digits = 3))
          ) |>
          select(Variable, PND, Group_Comparison = Comparison, Effect_Size, Confidence_Interval, P_Value)

        results_list[[as.character(pnd)]] <- dunnett_result
      }
    }

    if (length(results_list) > 0) {
      final_results_list[[var]] <- bind_rows(results_list)
    }
  }

  final_results <- bind_rows(final_results_list, .id = "Variable")

  if (nrow(final_results) > 0) {
    return(final_results |> flextable() |> set_table_properties(layout = "autofit"))
  } else {
    return("No valid comparisons due to lack of data.")
  }
}
run_posthoc_test_num_h2 <- function(data, vars, exclude_control = "Adjuvant+Saline") {

  data <- data %>%
    mutate(across(c(PND, Treatment, Animal), as.factor))

  final_results_list <- list()

  for (var in vars) {

    results_list <- list()

    for (pnd in unique(data$PND)) {

      subset_data <- data %>%
        filter(PND == pnd & Treatment != exclude_control & !is.na(.[[var]]))

      if (length(unique(subset_data$Treatment)) > 1) {

        tukey_raw <- TukeyHSD(aov(as.formula(paste0("`", var, "` ~ Treatment")), data = subset_data))$Treatment |>
          as.data.frame() |>
          rownames_to_column("Comparison")

        adjusted_p_values <- p.adjust(tukey_raw$`p adj`, method = "bonferroni")

        tukey_result <- tukey_raw |>
          mutate(
            Variable = var,
            PND = as.character(pnd),
            Effect_Size = round(diff, 3),
            Confidence_Interval = sprintf("[%.3f, %.3f]", round(lwr, 3), round(upr, 3)),
            P_Value = ifelse(adjusted_p_values < 0.05,
                             paste0(formatC(round(adjusted_p_values, 3), format = "f", digits = 3), " *"),
                             formatC(round(adjusted_p_values, 3), format = "f", digits = 3))
          ) |>
          select(Variable, PND, Group_Comparison = Comparison, Effect_Size, Confidence_Interval, P_Value)

        results_list[[as.character(pnd)]] <- tukey_result
      }
    }

    if (length(results_list) > 0) {
      final_results_list[[var]] <- bind_rows(results_list)
    }
  }

  final_results <- bind_rows(final_results_list, .id = "Variable")

  if (nrow(final_results) > 0) {
    return(final_results |> flextable() |> set_table_properties(layout = "autofit"))
  } else {
    return("No valid comparisons due to lack of data.")
  }
}
