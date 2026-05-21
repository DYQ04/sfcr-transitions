source(file.path("src", "00_utils", "utils.R"))

build_harmonized_wave_long <- function(working_long, supplement_long) {
  dt <- merge(copy(working_long), copy(supplement_long), by = c("cohort", "respondent_id", "wave"), all.x = TRUE)

  dt[cohort == "SHARE" & !is.na(country_raw), cohort_country := as.character(country_raw)]
  defaults <- read_project_config()$defaults$country_by_cohort
  for (nm in names(defaults)) dt[cohort == nm & is.na(cohort_country), cohort_country := defaults[[nm]]]

  dt[, partner_status_final := fcoalesce(partner_status_raw, as.character(as_label_factor(partner_status_supp)))]
  dt[, partnered := fcoalesce(partnered_working, marital_partnered(partner_status_supp))]
  dt[, child_count_final := fcoalesce(suppressWarnings(as.numeric(child_count_raw)), suppressWarnings(as.numeric(child_count_supp)))]
  dt[, has_living_child := fifelse(!is.na(child_count_final), as.integer(child_count_final > 0), has_living_child_working)]
  dt[, co_resident_child := fcoalesce(co_resident_child_working, recode_yesno_indicator(co_resident_child_supp))]
  dt[, frequent_child_contact := fcoalesce(child_contact_working, recode_yesno_indicator(child_contact_supp))]
  dt[, proxy := fcoalesce(proxy_working, recode_yesno_indicator(proxy_supp))]
  dt[, wealth_total := fcoalesce(suppressWarnings(as.numeric(wealth_working)), suppressWarnings(as.numeric(wealth_supp)))]
  dt[, person_weight := fcoalesce(suppressWarnings(as.numeric(weight_working)), suppressWarnings(as.numeric(weight_supp)))]

  dt[, has_adl_items := as.integer(!is.na(adl_bathing) | !is.na(adl_dressing) | !is.na(adl_eating) | !is.na(adl_bed) | !is.na(adl_toileting))]
  dt[, adl5_count := rowSums(.SD, na.rm = TRUE), .SDcols = c("adl_bathing", "adl_dressing", "adl_eating", "adl_bed", "adl_toileting")]
  dt[has_adl_items == 0L, adl5_count := NA_real_]
  dt[, adl5_count_strict := fifelse(adl5_count >= 2, 1L, fifelse(adl5_count < 2, 0L, NA_integer_))]
  dt[, state := fifelse(adl5_count == 0, 0L, fifelse(adl5_count >= 1, 1L, NA_integer_))]

  dt[, death_indicator := fcoalesce(death_wave_yes, recode_yesno_indicator(death_indicator_wave))]
  dt[, death_date := fifelse(!is.na(death_year), sprintf("%04d-%02d-15", death_year, fifelse(is.na(death_month), 6L, pmax(pmin(death_month, 12L), 1L))), NA_character_)]
  dt[, death_date := as.Date(death_date)]

  dt[, sfcr_score := fifelse(!is.na(partnered) & !is.na(has_living_child), partnered + has_living_child, NA_integer_)]
  dt[, sfcr_cat4 := fifelse(
    partnered == 1 & has_living_child == 1, "partner_child",
    fifelse(
      partnered == 1 & has_living_child == 0, "partner_only",
      fifelse(partnered == 0 & has_living_child == 1, "child_only", fifelse(partnered == 0 & has_living_child == 0, "neither", NA_character_))
    )
  )]
  dt[, sfcr_cat4 := factor(sfcr_cat4, levels = c("neither", "child_only", "partner_only", "partner_child"))]

  dt[, self_rated_health := tolower(as.character(as_label_factor(fcoalesce(self_rated_health_raw, shlt_supp))))]
  dt[, depression_z := if (all(is.na(depression_score))) NA_real_ else scale(depression_score)[, 1], by = .(cohort_country, wave)]
  dt[, cognition_z := if (all(is.na(cognition_score))) NA_real_ else scale(cognition_score)[, 1], by = .(cohort_country, wave)]
  dt[, wealth_tertile := fifelse(
    !is.na(wealth_total),
    cut(wealth_total, quantile(wealth_total, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), include.lowest = TRUE, labels = c("T1", "T2", "T3")),
    NA
  ), by = .(cohort_country, wave)]

  keep_cols <- c(
    "cohort", "cohort_country", "respondent_id", "wave", "interview_year", "interview_month", "interview_date",
    "age", "sex", "education", "partner_status_final", "partnered", "child_count_final", "has_living_child",
    "sfcr_score", "sfcr_cat4", "co_resident_child", "frequent_child_contact",
    "adl_bathing", "adl_dressing", "adl_eating", "adl_bed", "adl_toileting", "adl5_count", "adl5_count_strict", "state",
    "death_indicator", "death_year", "death_month", "death_date",
    "self_rated_health", "chronic_count_raw", "depression_score", "depression_z", "cognition_score", "cognition_z",
    "proxy", "wealth_total", "wealth_tertile", "person_weight"
  )

  out <- dt[, ..keep_cols]
  setorder(out, cohort, cohort_country, respondent_id, wave)
  fwrite(out, file.path(project_root(), "data_intermediate", "wave_long", "harmonized_wave_long.csv"))
  saveRDS(out, file.path(project_root(), "data_intermediate", "wave_long", "harmonized_wave_long.rds"))
  append_decision_log("Merged working-data long files with supplemental raw harmonized variables and derived SFCR, ADL5, death, chronic burden, and standardized depression/cognition measures.")
  out
}
