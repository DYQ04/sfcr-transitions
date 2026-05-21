source(file.path("src", "00_utils", "utils.R"))

prepare_single_working <- function(cohort_name) {
  priority <- read_priority_map()$core_working_data
  catlog <- cohort_catalog()[[cohort_name]]
  working_file <- catlog$working[[1]]
  dt <- as.data.table(read_dta(working_file))
  nm <- names(dt)

  id_var <- first_existing(nm, priority$id[[cohort_name]])
  wave_var <- first_existing(nm, priority$wave[[cohort_name]])
  year_var <- first_existing(nm, priority$interview_year[[cohort_name]])
  month_var <- first_existing(nm, priority$interview_month[[cohort_name]])
  marital_var <- first_existing(nm, priority$marital_status[[cohort_name]])
  child_var <- first_existing(nm, priority$child_count[[cohort_name]])
  coresd_var <- first_existing(nm, priority$co_resident_child[[cohort_name]])
  kcnt_var <- first_existing(nm, priority$child_contact[[cohort_name]])
  srh_var <- first_existing(nm, priority$self_rated_health[[cohort_name]])
  dep_var <- first_existing(nm, priority$depression[[cohort_name]])
  cog_var <- first_existing(nm, priority$cognition[[cohort_name]])
  proxy_var <- first_existing(nm, priority$proxy[[cohort_name]])

  age_var <- first_existing(nm, c("age", "agey", "r1agey", "ragey_e", "ragey_m", "ragey_b"))
  sex_var <- first_existing(nm, c("ragender"))
  educ_var <- first_existing(nm, c("raeducl", "raeduc_c", "raeduc_e", "raeduc_k", "raeduc_l"))
  death_year_var <- first_existing(nm, c("radyear"))
  death_month_var <- first_existing(nm, c("radmonth"))
  death_wave_var <- first_existing(nm, c("iwstat"))
  wealth_var <- first_existing(nm, c("wealth", "income_total"))

  adl_map <- list(
    bathing = first_existing(nm, c("batha", "bathb", "r1batha")),
    dressing = first_existing(nm, c("dressa", "dressb", "r1dressa")),
    eating = first_existing(nm, c("eata", "eatb", "r1eata")),
    bed = first_existing(nm, c("beda", "bedb_k", "bedb", "r1beda")),
    toileting = first_existing(nm, c("toilta", "toiltb", "r1toilta"))
  )
  chronic_vars <- intersect(nm, c("hibpe", "diabe", "cancre", "lunge", "hearte", "stroke", "psyche", "arthre"))

  out <- data.table(
    cohort = cohort_name,
    respondent_id = as.character(dt[[id_var]]),
    wave = if (!is.na(wave_var)) as.integer(dt[[wave_var]]) else 1L,
    interview_year = suppressWarnings(as.integer(dt[[year_var]])),
    interview_month = suppressWarnings(as.integer(dt[[month_var]])),
    age = suppressWarnings(as.numeric(dt[[age_var]])),
    sex = as.character(as_label_factor(dt[[sex_var]])),
    education = as.character(as_label_factor(dt[[educ_var]])),
    partner_status_raw = if (!is.na(marital_var)) as.character(as_label_factor(dt[[marital_var]])) else NA_character_,
    child_count_raw = NA_real_,
    co_resident_child_raw = if (!is.na(coresd_var)) dt[[coresd_var]] else NA,
    child_contact_raw = if (!is.na(kcnt_var)) dt[[kcnt_var]] else NA,
    self_rated_health_raw = if (!is.na(srh_var)) dt[[srh_var]] else NA,
    depression_raw = if (!is.na(dep_var)) dt[[dep_var]] else NA,
    cognition_raw = if (!is.na(cog_var)) dt[[cog_var]] else NA,
    proxy_raw = if (!is.na(proxy_var)) dt[[proxy_var]] else NA,
    weight_working = NA_real_,
    wealth_working = if (!is.na(wealth_var)) suppressWarnings(as.numeric(dt[[wealth_var]])) else NA_real_,
    death_year = if (!is.na(death_year_var)) suppressWarnings(as.integer(dt[[death_year_var]])) else NA_integer_,
    death_month = if (!is.na(death_month_var)) suppressWarnings(as.integer(dt[[death_month_var]])) else NA_integer_,
    death_indicator_wave = if (!is.na(death_wave_var)) dt[[death_wave_var]] else NA,
    chronic_count_raw = if ("chronic_num" %in% nm) suppressWarnings(as.numeric(dt[["chronic_num"]])) else NA_real_
  )

  if (!is.na(child_var)) {
    out[, child_count_raw := suppressWarnings(as.numeric(dt[[child_var]]))]
  } else if (all(c("hson", "hdau") %in% nm)) {
    out[, child_count_raw := suppressWarnings(as.numeric(dt[["hson"]]) + as.numeric(dt[["hdau"]]))]
  }
  if (is.na(out$interview_year[1L]) && cohort_name == "LASI" && "r1iwy" %in% nm) {
    out[, interview_year := suppressWarnings(as.integer(dt[["r1iwy"]]))]
    out[, interview_month := suppressWarnings(as.integer(dt[["r1iwm"]]))]
    out[, wave := 1L]
  }

  out[, interview_date := as.Date(sprintf("%04d-%02d-15", interview_year, fifelse(is.na(interview_month), 6L, pmax(pmin(interview_month, 12L), 1L))))]
  out[, partnered_working := if (!is.na(marital_var)) marital_partnered(dt[[marital_var]]) else NA_integer_]
  out[, has_living_child_working := fifelse(!is.na(child_count_raw), as.integer(child_count_raw > 0), NA_integer_)]
  out[, co_resident_child_working := recode_yesno_indicator(co_resident_child_raw)]
  out[, child_contact_working := recode_yesno_indicator(child_contact_raw)]
  out[, proxy_working := recode_yesno_indicator(proxy_raw)]
  out[, death_wave_yes := recode_yesno_indicator(death_indicator_wave)]
  out[, depression_score := suppressWarnings(as.numeric(depression_raw))]
  out[, cognition_score := suppressWarnings(as.numeric(cognition_raw))]
  out[, self_rated_health_label := as.character(as_label_factor(self_rated_health_raw))]

  for (nm_out in names(adl_map)) {
    src <- adl_map[[nm_out]]
    out[[paste0("adl_", nm_out)]] <- if (!is.na(src)) recode_binary_yes(dt[[src]]) else NA_integer_
  }

  if (!"chronic_num" %in% nm && length(chronic_vars)) {
    tmp <- as.data.table(lapply(dt[, ..chronic_vars], recode_yesno_indicator))
    out[, chronic_count_raw := rowSums(tmp, na.rm = TRUE)]
  }

  for (j in names(out)) {
    if (inherits(out[[j]], "haven_labelled")) {
      out[[j]] <- haven::zap_labels(out[[j]])
    }
  }

  out[, cohort_country := fifelse(cohort == "SHARE", NA_character_, catlog$default_country)]
  out[]
}

prepare_working_data <- function() {
  cohorts <- names(cohort_catalog())
  out <- rbindlist(lapply(cohorts, prepare_single_working), fill = TRUE)
  fwrite(out, file.path(project_root(), "data_intermediate", "cleaned", "working_long.csv"))
  saveRDS(out, file.path(project_root(), "data_intermediate", "cleaned", "working_long.rds"))
  append_decision_log("Prepared primary working-data long file across all cohorts, preserving raw marital, child, ADL, mortality, and covariate fields.")
  out
}
