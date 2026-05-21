source(file.path("src", "00_utils", "utils.R"))

build_interval_data <- function(wave_long) {
  dt <- copy(wave_long)
  setorder(dt, cohort, cohort_country, respondent_id, wave, interview_year)

  dt[, next_wave := shift(wave, type = "lead"), by = .(cohort, cohort_country, respondent_id)]
  dt[, next_year := shift(interview_year, type = "lead"), by = .(cohort, cohort_country, respondent_id)]
  dt[, next_state_obs := shift(state, type = "lead"), by = .(cohort, cohort_country, respondent_id)]
  dt[, next_death_wave := shift(death_indicator, type = "lead"), by = .(cohort, cohort_country, respondent_id)]

  dt[, next_state := fifelse(next_death_wave == 1, 2L, next_state_obs)]
  dt[is.na(next_state) & !is.na(death_year) & !is.na(interview_year) & death_year >= interview_year, next_state := 2L]
  dt[, interval_length := fifelse(!is.na(next_year), pmax(next_year - interview_year, 1), pmax(death_year - interview_year, 1))]
  dt[, valid_interval := as.integer(!is.na(state) & !is.na(next_state))]
  dt[, include_reason := fifelse(
    age < read_project_config()$age_min, "age_lt_50",
    fifelse(is.na(sex) | sex == "", "missing_sex", fifelse(is.na(state), "missing_state_t", fifelse(is.na(next_state), "missing_next_state", "included")))
  )]

  intervals <- dt[include_reason == "included" & valid_interval == 1L]
  intervals[, state_label := factor(state, levels = c(0, 1), labels = c("independent", "disabled"))]
  intervals[, next_state_label := factor(next_state, levels = c(0, 1, 2), labels = c("independent", "disabled", "dead"))]
  intervals[, age_group := cut(age, breaks = c(50, 65, 75, Inf), right = FALSE, labels = c("50-64", "65-74", "75+"))]
  intervals[, self_respondent_only := as.integer(is.na(proxy) | proxy == 0)]

  onset <- intervals[state == 0 & next_state == 1][order(cohort, cohort_country, respondent_id, wave)]
  onset_first <- onset[, .SD[1], by = .(cohort, cohort_country, respondent_id)]
  onset_follow <- merge(
    onset_first[, .(cohort, cohort_country, respondent_id, landmark_wave = wave)],
    intervals,
    by = c("cohort", "cohort_country", "respondent_id"),
    allow.cartesian = TRUE
  )[wave == landmark_wave + 1]

  fwrite(intervals, file.path(project_root(), "data_derived", "analysis_sets", "person_interval_main.csv"))
  saveRDS(intervals, file.path(project_root(), "data_derived", "analysis_sets", "person_interval_main.rds"))
  fwrite(onset_follow, file.path(project_root(), "data_derived", "analysis_sets", "first_onset_landmark.csv"))
  fwrite(dt[, .N, by = .(cohort, include_reason)], file.path(project_root(), "data_intermediate", "audit", "interval_exclusion_reasons.csv"))
  append_decision_log("Constructed person-wave intervals, including synthetic terminal death intervals when only death year remained after the last observed alive wave.")

  list(intervals = intervals, onset_landmark = onset_follow)
}
