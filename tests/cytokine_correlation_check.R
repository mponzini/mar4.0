# End-to-end check for vignettes/Cytokine-Behavior-Correlation.Rmd
# Runs all chunks (sans rmarkdown) and prints diagnostics.

suppressPackageStartupMessages({
  library(devtools)
  devtools::load_all(".", quiet = TRUE)
  library(dplyr); library(tidyr); library(purrr); library(tibble)
  library(stringr); library(ggplot2); library(ggcorrplot)
  library(ppcor); library(broom); library(flextable)
})

cat("\n================ 1. DATA SANITY ================\n")
cat("brain rows:", nrow(brain), " cols:", ncol(brain), "\n")
cat("brain Sample (first 6):", utils::head(brain$Sample), "\n")
cat("brain Treatment levels:\n"); print(sort(unique(brain$Treatment)))
cat("\nBehavior datasets:\n")
for (nm in c("epm","nor","sd_juv","sd_adult","usv","milestones","weights")) {
  d <- get(nm)
  cat(sprintf("  %-10s  nrow=%4d  cols=%2d\n", nm, nrow(d), ncol(d)))
}

# --------- config ----------
sig_cytokines <- c("IL_6","IL_1b","TNFa","IFNg","IL_10","IL_17a")
groups_to_analyze <- c(
  "zControl",
  "All",
  "CRMP1+CRMP2",
  "CRMP1+GDA",
  "STIP1+NSE"
)
min_n <- 10

cat("\nCytokine columns present in brain? ")
print(sig_cytokines %in% names(brain))
cat("\nTreatment labels in brain that match groups_to_analyze? ")
print(groups_to_analyze %in% unique(brain$Treatment))

# --------- brain_prep ----------
cat("\n================ 2. BRAIN PREP ================\n")
brain_parsed <- brain |>
  dplyr::mutate(
    Section_letter = stringr::str_sub(Sample, -1, -1),
    `Animal ID`    = stringr::str_sub(Sample, 1, -2),
    `Litter ID`    = Dam,
    Treat          = Treatment
  ) |>
  dplyr::select(
    `Animal ID`, `Litter ID`, Sex, Treat, Section_letter,
    dplyr::all_of(sig_cytokines)
  )
cat("Section letters:\n"); print(table(brain_parsed$Section_letter))
cat("Distinct animals per section:\n")
print(brain_parsed |> dplyr::count(Section_letter, `Animal ID`) |>
        dplyr::count(Section_letter, name = "n_animals"))

brain_sections <- split(brain_parsed, brain_parsed$Section_letter)
section_labels <- sort(names(brain_sections))
cat("section_labels:", section_labels, "\n")

# --------- behavior summaries ----------
cat("\n================ 3. BEHAVIOR SUMMARIES ================\n")
epm_s <- epm |>
  dplyr::mutate(`Time Spend Open Arms (%)` = 1 - Preference) |>
  dplyr::select(
    `Animal ID`,`Litter ID`,Sex,Treat,
    `Open Arms`,`Closed Arms`,`Enter Open Arms`,
    `Time Spend Open Arms (%)`,`SUM CHECK`
  )
cat("epm_s classes:\n"); print(sapply(epm_s, class))

nor_s <- nor |>
  dplyr::group_by(`Animal ID`,`Litter ID`,Sex,Treat) |>
  dplyr::summarise(
    `NOR Discrimination Index` = mean(`Discirmination Index`, na.rm = TRUE),
    .groups = "drop")

sd_juv_s <- sd_juv |>
  dplyr::select(
    `Animal ID`,`Litter ID`,Sex,Treat,
    dplyr::starts_with("Total duration"),
    dplyr::starts_with("Total number")) |>
  dplyr::rename_with(
    ~ paste0(.x," (juv)"),
    c(dplyr::starts_with("Total duration"),
      dplyr::starts_with("Total number")))

sd_adult_s <- sd_adult |>
  dplyr::select(
    `Animal ID`,`Litter ID`,Sex,Treat,
    dplyr::starts_with("Total duration"),
    dplyr::starts_with("Total number")) |>
  dplyr::rename_with(
    ~ paste0(.x," (adult)"),
    c(dplyr::starts_with("Total duration"),
      dplyr::starts_with("Total number")))

usv_s <- usv |>
  dplyr::rename(Treat = Treatment) |>
  dplyr::group_by(`Animal ID`,`Litter ID`,Sex,Treat) |>
  dplyr::summarise(
    `USV Count (total)`      = sum(Count, na.rm = TRUE),
    `USV Call Length (mean)` = mean(`Call Length`, na.rm = TRUE),
    `USV Mean Power (mean)`  = mean(`Mean Power`, na.rm = TRUE),
    .groups = "drop")

ms_s <- milestones |>
  dplyr::rename(Treat = Treatment) |>
  dplyr::group_by(`Animal ID`,`Litter ID`,Sex,Treat) |>
  dplyr::summarise(
    dplyr::across(
      c(`Body Length`,`Body Weight`,`Righting Reflex`,
        `Circle Traverse`,`Negative Geotaxis`,`Cliff Avoidance`),
      ~ mean(.x, na.rm = TRUE)),
    .groups = "drop")

wt_s <- weights |>
  dplyr::rename(Treat = Treatment) |>
  dplyr::group_by(`Animal ID`,`Litter ID`,Sex,Treat) |>
  dplyr::summarise(
    `Weight slope (g/wk)` = if (sum(!is.na(Weights)) >= 3) {
      unname(coef(stats::lm(Weights ~ Week))[2])
    } else { NA_real_ },
    .groups = "drop")

beh_list <- list(epm=epm_s,nor=nor_s,sd_juv=sd_juv_s,sd_adult=sd_adult_s,
                 usv=usv_s,ms=ms_s,wt=wt_s)

for (nm in names(beh_list)) {
  cat(sprintf("\n%s_s  nrow=%d\n", nm, nrow(beh_list[[nm]])))
  cat("  cols:\n"); print(sapply(beh_list[[nm]], class))
}

beh_list_clean <- purrr::map(beh_list,
  ~ dplyr::select(.x, -dplyr::any_of(c("Sex","Treat","Litter ID"))))

# --------- merge ----------
cat("\n================ 4. MERGE ================\n")
wide_by_section <- purrr::map(
  brain_sections,
  function(brain_sec) {
    purrr::reduce(
      c(list(dplyr::select(brain_sec, -Section_letter)), beh_list_clean),
      ~ dplyr::left_join(.x, .y, by = "Animal ID"))
  })

for (sec in names(wide_by_section)) {
  w <- wide_by_section[[sec]]
  cat(sprintf("wide_by_section[[%s]]  rows=%d  cols=%d\n",
              sec, nrow(w), ncol(w)))
}
cat("\nColumn classes in wide_by_section[['A']]:\n")
print(sapply(wide_by_section[[1]], class))

cyto_vars <- sig_cytokines
beh_vars <- wide_by_section[[1]] |>
  dplyr::select(-dplyr::all_of(c("Animal ID","Litter ID","Sex","Treat")),
                -dplyr::all_of(cyto_vars)) |>
  dplyr::select(dplyr::where(is.numeric)) |>
  names()
cat("\nbeh_vars (numeric only):\n"); print(beh_vars)

cat("\nTreat value counts per section:\n")
for (sec in names(wide_by_section)) {
  cat(" section", sec, ":\n")
  print(table(wide_by_section[[sec]]$Treat, useNA = "ifany"))
}

# --------- run_cor + within_group_cor ----------
cat("\n================ 5. CORRELATIONS ================\n")
run_cor <- function(cx, bx, data, min_n = 10) {
  d <- data |>
    dplyr::select(dplyr::all_of(c(cx,bx))) |>
    tidyr::drop_na()
  if (!is.numeric(d[[1]]) || !is.numeric(d[[2]])) {
    return(tibble::tibble(n = nrow(d), rho = NA_real_, p = NA_real_))
  }
  if (nrow(d) < min_n) {
    return(tibble::tibble(n = nrow(d), rho = NA_real_, p = NA_real_))
  }
  ct <- suppressWarnings(stats::cor.test(d[[1]], d[[2]], method = "spearman"))
  tibble::tibble(n = nrow(d), rho = unname(ct$estimate), p = ct$p.value)
}

pairs_by_group <- purrr::map_dfr(
  section_labels,
  function(sec) {
    wide <- wide_by_section[[sec]]
    purrr::map_dfr(groups_to_analyze, function(grp) {
      d_grp <- wide |> dplyr::filter(Treat == grp)
      tidyr::expand_grid(cytokine = cyto_vars, behavior = beh_vars) |>
        dplyr::mutate(
          section = sec, group = grp,
          res = purrr::map2(cytokine, behavior,
                            ~ run_cor(.x, .y, d_grp, min_n = min_n))) |>
        tidyr::unnest(res)
    })
  }) |>
  dplyr::group_by(section, group) |>
  dplyr::mutate(fdr = stats::p.adjust(p, method = "BH")) |>
  dplyr::ungroup() |>
  dplyr::arrange(section, group, fdr, p)

cat("pairs_by_group dim:", dim(pairs_by_group), "\n")
cat("rows with usable n (>= min_n):", sum(pairs_by_group$n >= min_n, na.rm = TRUE), "\n")
cat("rows with non-NA rho        :", sum(!is.na(pairs_by_group$rho)), "\n")
cat("FDR < 0.10                  :", sum(pairs_by_group$fdr < 0.10, na.rm = TRUE), "\n")
cat("FDR < 0.05                  :", sum(pairs_by_group$fdr < 0.05, na.rm = TRUE), "\n")

cat("\nSummary per (section, group):\n")
print(pairs_by_group |>
        dplyr::group_by(section, group) |>
        dplyr::summarise(
          n_pairs    = dplyr::n(),
          n_usable   = sum(!is.na(rho)),
          n_sig_fdr  = sum(fdr < 0.05, na.rm = TRUE),
          max_n      = suppressWarnings(max(n, na.rm = TRUE)),
          .groups    = "drop"), n = 30)

cat("\nHead of pairs_by_group sorted by FDR:\n")
print(utils::head(pairs_by_group |> dplyr::arrange(fdr, p), 15))

# --------- partial correlation ----------
cat("\n================ 6. PARTIAL CORR ================\n")
empty_partial <- tibble::tibble(
  section=character(),group=character(),cytokine=character(),behavior=character(),
  pcor=numeric(),p_partial=numeric(),n_partial=integer())

partial_candidates <- pairs_by_group |>
  dplyr::filter(!is.na(p), fdr < 0.10)
cat("partial_candidates rows:", nrow(partial_candidates), "\n")

if (nrow(partial_candidates) == 0) {
  partial_by_group <- empty_partial
} else {
  partial_by_group <- partial_candidates |>
    dplyr::mutate(
      pc = purrr::pmap(
        list(cytokine, behavior, group, section),
        function(cx, bx, grp, sec) {
          d <- wide_by_section[[sec]] |>
            dplyr::filter(Treat == grp) |>
            dplyr::select(dplyr::all_of(c(cx, bx, "Sex"))) |>
            dplyr::mutate(Sex = as.integer(factor(Sex))) |>
            tidyr::drop_na()
          if (nrow(d) < min_n) {
            return(tibble::tibble(pcor=NA_real_, p_partial=NA_real_,
                                  n_partial=nrow(d)))
          }
          r <- tryCatch(
            ppcor::pcor.test(d[[cx]], d[[bx]], d$Sex, method = "spearman"),
            error = function(e) NULL)
          if (is.null(r)) {
            return(tibble::tibble(pcor=NA_real_, p_partial=NA_real_,
                                  n_partial=nrow(d)))
          }
          tibble::tibble(pcor=r$estimate, p_partial=r$p.value, n_partial=nrow(d))
        })) |>
    tidyr::unnest(pc) |>
    dplyr::select(section, group, cytokine, behavior, pcor, p_partial, n_partial)
}
cat("partial_by_group rows:", nrow(partial_by_group), "\n")

# --------- results table ----------
cat("\n================ 7. RESULTS TABLE ================\n")
partial_for_join <- if (all(c("pcor","p_partial","n_partial") %in% names(partial_by_group))) {
  partial_by_group |>
    dplyr::select(section, group, cytokine, behavior, pcor, p_partial, n_partial)
} else empty_partial

results_tbl <- pairs_by_group |>
  dplyr::left_join(partial_for_join,
                   by = c("section","group","cytokine","behavior")) |>
  dplyr::mutate(dplyr::across(
    dplyr::any_of(c("rho","p","fdr","pcor","p_partial")), ~ signif(.x, 3))) |>
  dplyr::arrange(section, group, fdr, p)

cat("results_tbl dim:", dim(results_tbl), "\n")
cat("cols:", paste(names(results_tbl), collapse=", "), "\n")

cat("\n*** ALL CHUNKS RAN SUCCESSFULLY ***\n")
