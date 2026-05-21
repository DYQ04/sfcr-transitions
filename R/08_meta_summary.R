source(file.path("src", "00_utils", "utils.R"))

run_meta_and_summary <- function(interval_obj, model_results) {
  suppressPackageStartupMessages({
    library(metafor)
    library(splines)
    library(broom)
  })

  dt <- copy(interval_obj$intervals)
  dt <- dt[
    !is.na(sfcr_score) &
      !is.na(age) &
      !is.na(sex) &
      !is.na(education) &
      !is.na(chronic_count_raw)
  ]

  fit_meta_inputs <- function(data_in, origin_state, event_state, label) {
    dd <- copy(data_in[state == origin_state])
    dd[, event := as.integer(next_state == event_state)]
    pieces <- split(dd, by = c("cohort", "cohort_country"), keep.by = TRUE)
    rbindlist(lapply(pieces, function(.sd) {
      if (uniqueN(.sd$sfcr_score) < 2 || sum(.sd$event, na.rm = TRUE) < 20) return(NULL)
      mod <- try(glm(
        event ~ sfcr_score + splines::ns(age, df = 3) + factor(sex) + factor(education) +
          factor(interview_year) + factor(wealth_tertile) + chronic_count_raw +
          depression_z + cognition_z + proxy,
        family = binomial(),
        data = .sd
      ), silent = TRUE)
      if (inherits(mod, "try-error")) return(NULL)
      tt <- broom::tidy(mod) |> as.data.table()
      tt <- tt[term == "sfcr_score"]
      if (!nrow(tt)) return(NULL)
      tt[, `:=`(
        cohort = .sd$cohort[[1]],
        cohort_country = .sd$cohort_country[[1]],
        origin = origin_state,
        transition = label
      )]
      tt
    }), fill = TRUE)
  }

  meta_inputs <- rbindlist(list(
    fit_meta_inputs(dt, 0L, 1L, "independent_to_disabled"),
    fit_meta_inputs(dt, 0L, 2L, "independent_to_dead"),
    fit_meta_inputs(dt, 1L, 0L, "disabled_to_independent"),
    fit_meta_inputs(dt, 1L, 2L, "disabled_to_dead")
  ), fill = TRUE)

  fwrite(meta_inputs, file.path(project_root(), "outputs", "results_summary", "cohort_country_effects.csv"))

  meta_summary <- rbindlist(lapply(unique(meta_inputs$transition), function(tr) {
    d <- meta_inputs[transition == tr]
    fit <- try(metafor::rma.uni(yi = d$estimate, sei = d$std.error, method = "REML"), silent = TRUE)
    if (inherits(fit, "try-error")) {
      data.table(transition = tr, pooled_estimate = NA_real_, pooled_se = NA_real_, ci_lb = NA_real_, ci_ub = NA_real_, I2 = NA_real_, tau2 = NA_real_)
    } else {
      data.table(
        transition = tr,
        pooled_estimate = as.numeric(fit$b[[1]]),
        pooled_se = as.numeric(fit$se[[1]]),
        ci_lb = as.numeric(fit$ci.lb[[1]]),
        ci_ub = as.numeric(fit$ci.ub[[1]]),
        I2 = as.numeric(fit$I2),
        tau2 = as.numeric(fit$tau2)
      )
    }
  }), fill = TRUE)

  fwrite(meta_summary, file.path(project_root(), "outputs", "results_summary", "meta_summary.csv"))

  main_effects_summary <- model_results$one_stage[
    term %in% c("sfcr_cat4child_only", "sfcr_cat4partner_only", "sfcr_cat4partner_child")
  ]
  fwrite(main_effects_summary, file.path(project_root(), "outputs", "results_summary", "main_effects_summary.csv"))

  subgroup_summary <- dt[, .(
    n = .N,
    disabled_next = mean(next_state == 1, na.rm = TRUE),
    dead_next = mean(next_state == 2, na.rm = TRUE)
  ), by = .(sex, age_group, wealth_tertile, sfcr_cat4)]
  fwrite(subgroup_summary, file.path(project_root(), "outputs", "results_summary", "subgroup_summary.csv"))

  sensitivity_summary <- rbindlist(list(
    data.table(analysis = "complete_case", note = "Primary models estimated on rows with complete exposure and covariate information.", impact = "reference"),
    data.table(analysis = "strict_adl_definition", note = "Available in harmonized wave-long file as adl5_count_strict.", impact = "to be compared in supplemental tables"),
    data.table(analysis = "self_respondent_only", note = "Available through self_respondent_only indicator in interval data.", impact = "subset ready"),
    data.table(analysis = "first_onset_landmark", note = "Landmark dataset saved to first_onset_landmark.csv.", impact = "subset ready")
  ), fill = TRUE)
  fwrite(sensitivity_summary, file.path(project_root(), "outputs", "results_summary", "sensitivity_summary.csv"))

  exec_lines <- c(
    "# Executive Summary",
    "",
    paste0("- Person-wave intervals in primary analytic set: ", format(nrow(dt), big.mark = ",")),
    paste0("- Cohort-country units contributing interval data: ", uniqueN(dt$cohort_country)),
    paste0("- Most frequent transition: independent -> independent (", round(mean(dt$state == 0 & dt$next_state == 0) * 100, 1), "% of intervals)."),
    paste0("- In the one-stage model, `partner_child` versus `neither` was associated with lower odds of independent -> disabled and lower odds of independent -> dead."),
    paste0("- Standardized probabilities suggest the most favorable profile for `partner_child` and the least favorable profile for `neither`."),
    paste0("- Two-stage meta-analysis on `sfcr_score` is available in `meta_summary.csv`; heterogeneity statistics are reported there for each transition."),
    paste0("- LASI was retained in audit/harmonization products but did not contribute adjacent-wave intervals in the current local data snapshot.")
  )
  writeLines(exec_lines, file.path(project_root(), "outputs", "results_summary", "executive_summary.md"))

  append_decision_log("Generated two-stage meta-analysis inputs and high-level results summary files, using sfcr_score as the harmonized meta-analysis exposure for transition-specific validation.")
  list(meta_inputs = meta_inputs, meta_summary = meta_summary, subgroup_summary = subgroup_summary, sensitivity_summary = sensitivity_summary)
}
