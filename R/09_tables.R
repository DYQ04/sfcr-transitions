source(file.path("src", "00_utils", "utils.R"))

make_tables <- function() {
  wave_long <- readRDS(file.path(project_root(), "data_intermediate", "wave_long", "harmonized_wave_long.rds"))
  intervals <- readRDS(file.path(project_root(), "data_derived", "analysis_sets", "person_interval_main.rds"))
  coef_dt <- fread(file.path(project_root(), "data_derived", "models", "one_stage_multinom_coefficients.csv"))
  meta_summary <- fread(file.path(project_root(), "outputs", "results_summary", "meta_summary.csv"))
  sensitivity_summary <- fread(file.path(project_root(), "outputs", "results_summary", "sensitivity_summary.csv"))

  baseline <- wave_long[order(cohort, respondent_id, wave)][, .SD[1], by = .(cohort, respondent_id)]
  table1 <- baseline[, .(
    n = .N,
    age_mean = mean(age, na.rm = TRUE),
    female_pct = mean(sex %in% c("女性", "女", "female", "Female"), na.rm = TRUE),
    partnered_pct = mean(partnered == 1, na.rm = TRUE),
    living_child_pct = mean(has_living_child == 1, na.rm = TRUE),
    adl_disabled_pct = mean(state == 1, na.rm = TRUE)
  ), by = .(cohort)]
  write_multi_table(table1, file.path(project_root(), "outputs", "tables", "main", "table_main_1_baseline_characteristics"))

  table2 <- intervals[, .(
    n = .N,
    next_independent = mean(next_state == 0),
    next_disabled = mean(next_state == 1),
    next_dead = mean(next_state == 2)
  ), by = .(state_label, sfcr_cat4)]
  write_multi_table(table2, file.path(project_root(), "outputs", "tables", "main", "table_main_2_transition_probabilities"))

  table3 <- coef_dt[grepl("^sfcr_cat4", term)]
  write_multi_table(table3, file.path(project_root(), "outputs", "tables", "main", "table_main_3_main_models"))

  table4 <- cbind(
    meta_summary,
    sensitivity_note = c(sensitivity_summary$note, rep(NA_character_, max(0, nrow(meta_summary) - nrow(sensitivity_summary))))[seq_len(nrow(meta_summary))],
    sensitivity_impact = c(sensitivity_summary$impact, rep(NA_character_, max(0, nrow(meta_summary) - nrow(sensitivity_summary))))[seq_len(nrow(meta_summary))]
  )
  write_multi_table(table4, file.path(project_root(), "outputs", "tables", "main", "table_main_4_heterogeneity_and_sensitivity_summary"))

  st1 <- wave_long[, .(rows = .N, ids = uniqueN(respondent_id), waves = paste(sort(unique(wave)), collapse = ",")), by = cohort]
  write_multi_table(st1, file.path(project_root(), "outputs", "tables", "supplement", "st1_cohort_overview"))

  vm <- fread(file.path(project_root(), "docs", "variable_mapping.csv"))
  write_multi_table(vm[harmonized_name %in% c("partnered", "has_living_child")], file.path(project_root(), "outputs", "tables", "supplement", "st2_exposure_variable_mapping"))
  write_multi_table(vm[harmonized_name == "adl5_count"], file.path(project_root(), "outputs", "tables", "supplement", "st3_outcome_variable_mapping"))

  dl <- readLines(file.path(project_root(), "docs", "decision_log.md"))
  st4 <- data.table(entry = dl)
  write_multi_table(st4, file.path(project_root(), "outputs", "tables", "supplement", "st4_harmonization_decision_ledger"))

  st5 <- data.table(
    item = c("SHARE", "LASI", "Wealth proxy", "ADL outcome"),
    note = c(
      "Retained as cohort-country units using SHARE country identifier.",
      "Single-wave in current local snapshot; audited but not interval-contributing.",
      "Within-wave tertiles built from first available wealth-like total or household income proxy.",
      "Reconstructed from five ADL items rather than imported summary totals."
    )
  )
  write_multi_table(st5, file.path(project_root(), "outputs", "tables", "supplement", "st5_comparability_notes"))

  st6 <- wave_long[, lapply(.SD, function(x) mean(is.na(x))), by = .(cohort, wave), .SDcols = c("partnered", "has_living_child", "adl5_count", "wealth_total", "self_rated_health")]
  write_multi_table(st6, file.path(project_root(), "outputs", "tables", "supplement", "st6_missingness_by_cohort_wave"))

  st7 <- fread(file.path(project_root(), "data_intermediate", "audit", "interval_exclusion_reasons.csv"))
  write_multi_table(st7, file.path(project_root(), "outputs", "tables", "supplement", "st7_included_vs_excluded"))

  st8 <- intervals[, .(n = .N, age_mean = mean(age, na.rm = TRUE), partnered_pct = mean(partnered == 1, na.rm = TRUE)), by = cohort_country]
  write_multi_table(st8, file.path(project_root(), "outputs", "tables", "supplement", "st8_cohort_country_descriptives"))

  st9 <- coef_dt
  write_multi_table(st9, file.path(project_root(), "outputs", "tables", "supplement", "st9_full_main_coefficients"))

  st10 <- fread(file.path(project_root(), "outputs", "results_summary", "meta_summary.csv"))
  write_multi_table(st10, file.path(project_root(), "outputs", "tables", "supplement", "st10_meta_analysis_results"))

  st11 <- fread(file.path(project_root(), "outputs", "results_summary", "subgroup_summary.csv"))
  write_multi_table(st11, file.path(project_root(), "outputs", "tables", "supplement", "st11_full_interactions"))

  st12 <- fread(file.path(project_root(), "outputs", "results_summary", "sensitivity_summary.csv"))
  write_multi_table(st12, file.path(project_root(), "outputs", "tables", "supplement", "st12_full_sensitivity_results"))

  st13 <- coef_dt[, .(
    transitions = paste(sort(unique(y.level)), collapse = ","),
    terms = .N
  ), by = origin_state]
  write_multi_table(st13, file.path(project_root(), "outputs", "tables", "supplement", "st13_model_diagnostics"))

  st14 <- data.table(
    cohort = c("CHARLS", "ELSA", "HRS", "KLoSA", "LASI", "MHAS", "SHARE"),
    weight_strategy = c(
      "working/raw supplement person-level weights when available",
      "harmonized ELSA cross-sectional or longitudinal person weights",
      "RAND / Gateway HRS person-level or longitudinal weights",
      "harmonized KLoSA person-level and longitudinal weights",
      "LASI person-level post-stratified weight",
      "MHAS person-level analysis weight",
      "SHARE person-level analysis and longitudinal weights"
    )
  )
  write_multi_table(st14, file.path(project_root(), "outputs", "tables", "supplement", "st14_weight_specifications"))
}
