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

# Convert numeric p-values to significance stars
stars <- function(p) {
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*",
                       ifelse(p < 0.1, ".", "ns"))))
}

#' @title Posthoc Test Helper Function
#'
#' @param df Data frame of the data
#' @param metric The column name and header label you want to use
#'
#' @returns Return the table of posthoc estimation table
#'
#' @export
#'
make_posthoc_flextable <- function(
    df,
    metric = "Mean_Diff"
) {
  library(dplyr)
  library(tidyr)
  library(flextable)
  library(stringr)

  # Step 1: Format the data
  tbl_cleaned <- df %>%
    select(Variable, PND, Group_Comparison, !!sym(metric), SE, p_inter_adj) %>%
    rename(
      Metric_Col = !!sym(metric),
      p_value = p_inter_adj
    ) %>%
    mutate(
      Metric_Col = round(as.numeric(Metric_Col), 3),
      SE = round(as.numeric(SE), 3)
    ) %>%
    mutate(across(c(Metric_Col, SE, p_value), as.character))

  # Step 2: Reshape to wide format
  wide_tbl <- tbl_cleaned %>%
    pivot_longer(cols = c(Metric_Col, SE, p_value), names_to = "Metric", values_to = "Value") %>%
    mutate(PND_label = paste0("PND=", PND)) %>%
    unite("PND_Metric", Metric, PND_label, sep = "@") %>%
    mutate(
      Group1 = str_trim(str_extract(Group_Comparison, "^[^-]+")),
      Group2 = str_trim(str_extract(Group_Comparison, "[^-]+$"))
    ) %>%
    select(Variable, Group1, Group2, PND_Metric, Value) %>%
    pivot_wider(names_from = PND_Metric, values_from = Value)

  # Step 3: Prepare header rows
  col_names <- names(wide_tbl)
  # Replace "Metric_Col" in header with the metric name you provided
  header2 <- c("Variable", "Group 1", "Group 2",
               gsub("Metric_Col", metric, str_remove(col_names[-(1:3)], "@.*")))

  header_df <- data.frame(
    col_keys = col_names,
    line2 = header2,
    stringsAsFactors = FALSE
  )

  # Step 4: Construct top row ("Group Info" and "PND=x")
  pnd_labels <- str_extract(col_names[-(1:3)], "PND=\\d+")
  pnd_levels <- unique(pnd_labels)  # preserves order
  pnd_counts <- table(factor(pnd_labels, levels = pnd_levels))

  colwidths <- c(3, as.numeric(pnd_counts))
  top_labels <- c("Group Info", names(pnd_counts))

  # Step 5: Create flextable with 2 header rows
  ft <- flextable(wide_tbl) %>%
    set_header_df(mapping = header_df, key = "col_keys") %>%
    add_header_row(values = top_labels, colwidths = colwidths) %>%
    theme_vanilla() %>%
    autofit()

  return(ft)
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
    treatment_groups <- sort(treatment_groups)

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

      comparisons <- c(comparisons, paste0(treatment, "-", control_group))
      se <- c(se,se_diff)
      prop_differences <- c(prop_differences, as.character(prop_diff))
      ci_values <- c(ci_values, as.character(ci_diff))
    }

    p_values_inter <- p_list_3 |> filter(PND == pnd) |> dplyr::slice_head(n = 4) |> pull(p.value)
    adjusted_p_values_inter <- p.adjust(p_values_inter, method = "hochberg")
    p_values_add <-   p_list_2 |> filter(PND == pnd) |> dplyr::slice_head(n = 4) |> pull(p.value)
    adjusted_p_values_add <- p.adjust(p_values_add, method = "hochberg")

    baseline_result <- data.frame(
      Variable = var,
      PND = as.character(pnd),
      Group_Comparison = comparisons,
      `Prop_Diff` = as.numeric(prop_differences),
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
    treatment_groups <- sort(treatment_groups)

    for (i in 1:(length(treatment_groups))) {
      for (j in (i + 1):length(treatment_groups)) {
        if (j <= length(treatment_groups) & i <= (length(treatment_groups) - 1)){
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

          comparisons <- c(comparisons, paste0(treatment_groups[i], "-", treatment_groups[j]))
          se <- c(se,se_diff)
          prop_differences <- c(prop_differences, as.character(prop_diff))  # Convert to character
          ci_values <- c(ci_values, as.character(ci_diff))  # Convert to character
        }
      }
    }

    p_values_inter <- p_list_3 |> arrange(contrast) |> filter(PND == pnd) |> dplyr::slice_tail(n = 6) |> pull(p.value)
    adjusted_p_values_inter <- p.adjust(p_values_inter, method = "hochberg")
    p_values_add <-   p_list_2 |> arrange(contrast) |> filter(PND == pnd) |> dplyr::slice_tail(n = 6) |> pull(p.value)
    adjusted_p_values_add <- p.adjust(p_values_add, method = "hochberg")

    baseline_result <- data.frame(
      Variable = var,
      PND = as.character(pnd),
      Group_Comparison = comparisons,
      Prop_Diff = as.numeric(prop_differences),
      SE = round(se,3),
      Confidence_Interval = ci_values,
      p_inter = p_values_inter |> format_pval_with_star(),
      p_inter_adj = adjusted_p_values_inter |> format_pval_with_star(),
      p_add = p_values_add |> format_pval_with_star(),
      p_add_adj = adjusted_p_values_add |> format_pval_with_star()
    )

    results_list[[as.character(pnd)]] <- baseline_result
  }
stars
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
        adjusted_p_values_inter <- p.adjust(p_values_inter, method = "hochberg")
        p_values_add <-   c(p_list_2 |> filter(PND == pnd) |> dplyr::slice_head(n = 4) |> pull(p.value))
        adjusted_p_values_add <- p.adjust(p_values_add, method = "hochberg")
        dunnett_result <- dunnett_raw |>
          mutate(
            Variable = var_,
            PND = as.character(pnd),
            Mean_Diff = round(diff, 3),
            SE = round(SE,3),
            Confidence_Interval = sprintf("[%.3f, %.3f]", round(lwr.ci, 3), round(upr.ci, 3)),
            p_inter = p_values_inter |> format_pval_with_star(),
            p_inter_adj = adjusted_p_values_inter |> format_pval_with_star(),
            p_add = p_values_add |> format_pval_with_star(),
            p_add_adj = adjusted_p_values_add |> format_pval_with_star()
          ) |>
          select(Variable, PND, Group_Comparison = Comparison, Mean_Diff = Mean_Diff, SE, Confidence_Interval, p_inter, p_inter_adj, p_add, p_add_adj)

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
        adjusted_p_values_inter <- p.adjust(p_values_inter, method = "hochberg")
        p_values_add <-   c(p_list_2 |> filter(PND == pnd) |> dplyr::slice_tail(n = 6) |> pull(p.value))
        adjusted_p_values_add <- p.adjust(p_values_add, method = "hochberg")

        tukey_result <- tukey_raw |>
          mutate(
            Variable = var_,
            PND = as.character(pnd),
            Mean_Diff = round(diff, 3),
            SE = round(SE,3),
            Confidence_Interval = sprintf("[%.3f, %.3f]", round(lwr, 3), round(upr, 3)),
            p_inter = p_values_inter |> format_pval_with_star(),
            p_inter_adj = adjusted_p_values_inter |> format_pval_with_star(),
            p_add = p_values_add |> format_pval_with_star(),
            p_add_adj = adjusted_p_values_add |> format_pval_with_star()
          ) |>
          select(Variable, PND, Group_Comparison = Comparison, Mean_Diff = Mean_Diff, SE, Confidence_Interval, p_inter, p_inter_adj, p_add, p_add_adj)

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
