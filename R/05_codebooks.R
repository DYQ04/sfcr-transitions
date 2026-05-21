source(file.path("src", "00_utils", "utils.R"))

make_codebooks <- function() {
  priority <- read_priority_map()
  catlog <- cohort_catalog()

  rows <- rbindlist(lapply(names(catlog), function(cohort) {
    wf <- catlog[[cohort]]$working[[1]]
    if (is.na(wf) || !file.exists(wf)) return(NULL)
    x <- read_dta(wf, n_max = 1)
    labs <- vapply(x, safe_label, character(1))
    data.table(
      cohort = cohort,
      raw_source_variable = names(x),
      raw_label = labs
    )
  }), fill = TRUE)

  map_rows <- rbindlist(list(
    data.table(variable_role = "exposure", harmonized_name = "partnered", cohort = names(priority$core_working_data$marital_status), raw_source_variable = vapply(priority$core_working_data$marital_status, function(x) paste(x, collapse = "; "), character(1)), coding_rule = "Marital/partner status recoded to binary structural availability.", wave_coverage = "working-data waves", missing_rule = "If ambiguous or absent, set NA then attempt supplement.", notes = "Partnered includes married, cohabiting, registered partner."),
    data.table(variable_role = "exposure", harmonized_name = "has_living_child", cohort = names(priority$core_working_data$child_count), raw_source_variable = vapply(priority$core_working_data$child_count, function(x) paste(x, collapse = "; "), character(1)), coding_rule = "Living child count > 0.", wave_coverage = "working-data waves with HRS supplement from RAND wide file", missing_rule = "If child count unavailable after supplement, set NA.", notes = "Main exposure component."),
    data.table(variable_role = "outcome", harmonized_name = "adl5_count", cohort = names(catlog), raw_source_variable = c("batha/dressa/eata/beda/toilta", "batha/dressa/eata/beda/toilta", "batha/dressa/eata/beda/toilta", "bathb/dressb/eatb/bedb_k/toiltb", "r1batha/r1dressa/r1eata/r1beda/r1toilta", "batha/dressa/eata/beda/toilta", "batha/dressa/eata/beda/toilta"), coding_rule = "Item-level recode to binary difficulty, then sum 5 items.", wave_coverage = "all available waves", missing_rule = "If item labels cannot be resolved, set item missing.", notes = "Primary outcome reconstructed; summary ADL totals not used as final outcome."),
    data.table(variable_role = "covariate", harmonized_name = "wealth_total", cohort = names(catlog), raw_source_variable = "working-data wealth/income or supplement-wide atotb/atotw/itot candidates", coding_rule = "Use first available wealth-like total, then within cohort-country-wave tertiles.", wave_coverage = "cohort-specific", missing_rule = "Missing when no acceptable total wealth/income proxy found.", notes = "Used as pragmatic wealth proxy in current local data snapshot.")
  ), fill = TRUE)

  codebook <- merge(map_rows, rows, by = c("cohort", "raw_source_variable"), all.x = TRUE)
  write_multi_table(map_rows, file.path(project_root(), "docs", "variable_mapping"))
  write_multi_table(codebook, file.path(project_root(), "docs", "harmonization_codebook"))
}
