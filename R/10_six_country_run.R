source(file.path("R", "00_utils.R"))

rerun_six_country_bundle <- function() {
  suppressPackageStartupMessages({
    library(data.table)
    library(nnet)
    library(splines)
    library(broom)
    library(metafor)
    library(patchwork)
    library(scales)
    library(maps)
    library(gt)
  })

  root <- project_root()
  out_root <- file.path(root, "outputs", "reruns", "six_country")
  dir_create(out_root)
  dir_create(file.path(out_root, "data", "analysis_sets"))
  dir_create(file.path(out_root, "data", "models"))
  dir_create(file.path(out_root, "results_summary"))
  dir_create(file.path(out_root, "tables", "main"))
  dir_create(file.path(out_root, "tables", "supplement"))
  dir_create(file.path(out_root, "figures_submission_ready", "main"))
  dir_create(file.path(out_root, "figures_submission_ready", "source_data"))

  write_multi_table_local <- function(dt, stem) {
    dir_create(path_dir(stem))
    fwrite(dt, paste0(stem, ".csv"))
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "data")
    openxlsx::writeData(wb, "data", as.data.frame(dt))
    openxlsx::saveWorkbook(wb, paste0(stem, ".xlsx"), overwrite = TRUE)
    gt_tbl <- gt::gt(as.data.frame(dt))
    gt::gtsave(gt_tbl, paste0(stem, ".html"))
    try(gt::gtsave(gt_tbl, paste0(stem, ".rtf")), silent = TRUE)
  }

  save_plot_local <- function(plot_obj, stem, width = 10, height = 7, dpi = 380) {
    dir_create(path_dir(stem))
    ggsave(paste0(stem, ".png"), plot_obj, width = width, height = height, dpi = dpi)
    ggsave(paste0(stem, ".pdf"), plot_obj, width = width, height = height, device = cairo_pdf)
  }

  write_src <- function(dt, name) {
    fwrite(dt, file.path(out_root, "figures_submission_ready", "source_data", paste0(name, ".csv")))
  }

  share_country_map <- c(
    `11` = "Austria", `12` = "Germany", `13` = "Sweden", `14` = "Netherlands",
    `15` = "Spain", `16` = "Italy", `17` = "France", `18` = "Denmark",
    `19` = "Greece", `20` = "Switzerland", `23` = "Belgium", `25` = "Israel",
    `28` = "Czech Republic", `29` = "Poland", `30` = "Ireland", `31` = "Luxembourg",
    `32` = "Hungary", `33` = "Portugal", `34` = "Slovenia", `35` = "Estonia",
    `47` = "Croatia", `48` = "Lithuania", `51` = "Bulgaria", `53` = "Cyprus",
    `55` = "Finland", `57` = "Latvia", `59` = "Malta", `61` = "Romania",
    `63` = "Slovakia"
  )

  country_clean <- function(x) {
    out <- as.character(x)
    out[out %in% names(share_country_map)] <- share_country_map[out[out %in% names(share_country_map)]]
    out[out == "United States"] <- "USA"
    out[out == "United Kingdom"] <- "UK"
    out
  }

  wave_long <- readRDS(file.path(root, "data_intermediate", "wave_long", "harmonized_wave_long.rds"))
  wave_long <- as.data.table(wave_long)[cohort != "LASI"]
  fwrite(wave_long, file.path(out_root, "data", "analysis_sets", "harmonized_wave_long_six_country.csv"))
  saveRDS(wave_long, file.path(out_root, "data", "analysis_sets", "harmonized_wave_long_six_country.rds"))

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
  fwrite(intervals, file.path(out_root, "data", "analysis_sets", "person_interval_main_six_country.csv"))
  saveRDS(intervals, file.path(out_root, "data", "analysis_sets", "person_interval_main_six_country.rds"))

  onset <- intervals[state == 0 & next_state == 1][order(cohort, cohort_country, respondent_id, wave)]
  onset_first <- onset[, .SD[1], by = .(cohort, cohort_country, respondent_id)]
  onset_follow <- merge(
    onset_first[, .(cohort, cohort_country, respondent_id, landmark_wave = wave)],
    intervals,
    by = c("cohort", "cohort_country", "respondent_id"),
    allow.cartesian = TRUE
  )[wave == landmark_wave + 1]
  fwrite(onset_follow, file.path(out_root, "data", "analysis_sets", "first_onset_landmark_six_country.csv"))

  exclusion_dt <- dt[, .N, by = .(cohort, include_reason)]
  fwrite(exclusion_dt, file.path(out_root, "data", "analysis_sets", "interval_exclusion_reasons_six_country.csv"))

  ana_dt <- copy(intervals)[
    !is.na(sfcr_cat4) &
      !is.na(age) &
      !is.na(sex) &
      !is.na(education) &
      !is.na(chronic_count_raw)
  ]
  ana_dt[, sex := factor(sex)]
  ana_dt[, education := factor(education)]
  ana_dt[, wealth_tertile := factor(wealth_tertile)]
  ana_dt[, cohort_country := factor(cohort_country)]
  ana_dt[, next_state_label := factor(next_state_label)]

  origin0 <- ana_dt[state == 0]
  origin1 <- ana_dt[state == 1]
  fml <- next_state_label ~ sfcr_cat4 + splines::ns(age, df = 3) + sex + education +
    factor(interview_year) + cohort_country + wealth_tertile + chronic_count_raw +
    depression_z + cognition_z + proxy

  mod0 <- nnet::multinom(fml, data = origin0, trace = FALSE)
  mod1 <- nnet::multinom(fml, data = origin1, trace = FALSE)
  tidy0 <- as.data.table(broom::tidy(mod0, conf.int = TRUE))
  tidy1 <- as.data.table(broom::tidy(mod1, conf.int = TRUE))
  tidy0[, origin_state := "independent"]
  tidy1[, origin_state := "disabled"]
  one_stage <- rbindlist(list(tidy0, tidy1), fill = TRUE)
  fwrite(one_stage, file.path(out_root, "data", "models", "one_stage_multinom_coefficients.csv"))
  saveRDS(list(mod0 = mod0, mod1 = mod1), file.path(out_root, "data", "models", "one_stage_models.rds"))

  std_fun <- function(data_in, model, origin_state_value) {
    use <- copy(data_in[state == origin_state_value])
    rbindlist(lapply(levels(use$sfcr_cat4), function(expo) {
      nd <- copy(use)
      nd[, sfcr_cat4 := factor(expo, levels = levels(use$sfcr_cat4))]
      pp <- predict(model, newdata = nd, type = "probs")
      if (is.null(dim(pp))) {
        pp <- matrix(pp, ncol = 1)
        colnames(pp) <- as.character(unique(use$next_state_label))[1]
      }
      probs <- colMeans(pp, na.rm = TRUE)
      out <- as.data.table(as.list(probs))
      out[, `:=`(origin_state = origin_state_value, sfcr_cat4 = expo)]
      out
    }), fill = TRUE)
  }
  std_probs <- rbindlist(list(std_fun(ana_dt, mod0, 0L), std_fun(ana_dt, mod1, 1L)), fill = TRUE)
  fwrite(std_probs, file.path(out_root, "results_summary", "standardized_transition_probabilities.csv"))

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
    fit_meta_inputs(ana_dt, 0L, 1L, "independent_to_disabled"),
    fit_meta_inputs(ana_dt, 0L, 2L, "independent_to_dead"),
    fit_meta_inputs(ana_dt, 1L, 0L, "disabled_to_independent"),
    fit_meta_inputs(ana_dt, 1L, 2L, "disabled_to_dead")
  ), fill = TRUE)
  fwrite(meta_inputs, file.path(out_root, "results_summary", "cohort_country_effects.csv"))

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
  fwrite(meta_summary, file.path(out_root, "results_summary", "meta_summary.csv"))

  main_effects_summary <- one_stage[term %in% c("sfcr_cat4child_only", "sfcr_cat4partner_only", "sfcr_cat4partner_child")]
  fwrite(main_effects_summary, file.path(out_root, "results_summary", "main_effects_summary.csv"))

  subgroup_summary <- ana_dt[, .(
    n = .N,
    disabled_next = mean(next_state == 1, na.rm = TRUE),
    dead_next = mean(next_state == 2, na.rm = TRUE)
  ), by = .(sex, age_group, wealth_tertile, sfcr_cat4)]
  fwrite(subgroup_summary, file.path(out_root, "results_summary", "subgroup_summary.csv"))

  sensitivity_summary <- rbindlist(list(
    data.table(analysis = "complete_case", note = "Primary models estimated on rows with complete exposure and covariate information.", impact = "reference"),
    data.table(analysis = "strict_adl_definition", note = "Available in harmonized wave-long file as adl5_count_strict.", impact = "to be compared in supplemental tables"),
    data.table(analysis = "self_respondent_only", note = "Available through self_respondent_only indicator in interval data.", impact = "subset ready"),
    data.table(analysis = "first_onset_landmark", note = "Landmark dataset saved to first_onset_landmark_six_country.csv.", impact = "subset ready")
  ), fill = TRUE)
  fwrite(sensitivity_summary, file.path(out_root, "results_summary", "sensitivity_summary.csv"))

  exec_lines <- c(
    "# Executive Summary",
    "",
    "- Six-country rerun excluding LASI from all analytic and descriptive products.",
    paste0("- Person-wave intervals in primary analytic set: ", format(nrow(ana_dt), big.mark = ",")),
    paste0("- Cohort-country units contributing interval data: ", uniqueN(ana_dt$cohort_country)),
    paste0("- Most frequent transition: independent -> independent (", round(mean(ana_dt$state == 0 & ana_dt$next_state == 0) * 100, 1), "% of intervals)."),
    "- Because LASI contributed no adjacent-wave intervals in the original local snapshot, interval-based model estimates are expected to be unchanged or numerically trivial in their changes.",
    "- All descriptive tables and figure source data in this rerun exclude LASI explicitly."
  )
  writeLines(exec_lines, file.path(out_root, "results_summary", "executive_summary.md"))

  baseline <- wave_long[order(cohort, respondent_id, wave)][, .SD[1], by = .(cohort, respondent_id)]
  table1 <- baseline[, .(
    n = .N,
    age_mean = mean(age, na.rm = TRUE),
    female_pct = mean(grepl("female|Female|女", sex), na.rm = TRUE),
    partnered_pct = mean(partnered == 1, na.rm = TRUE),
    living_child_pct = mean(has_living_child == 1, na.rm = TRUE),
    adl_disabled_pct = mean(state == 1, na.rm = TRUE)
  ), by = .(cohort)]
  write_multi_table_local(table1, file.path(out_root, "tables", "main", "table_main_1_baseline_characteristics"))

  table2 <- intervals[, .(
    n = .N,
    next_independent = mean(next_state == 0),
    next_disabled = mean(next_state == 1),
    next_dead = mean(next_state == 2)
  ), by = .(state_label, sfcr_cat4)]
  write_multi_table_local(table2, file.path(out_root, "tables", "main", "table_main_2_transition_probabilities"))

  table3 <- main_effects_summary
  write_multi_table_local(table3, file.path(out_root, "tables", "main", "table_main_3_main_models"))

  table4 <- cbind(
    meta_summary,
    sensitivity_note = c(sensitivity_summary$note, rep(NA_character_, max(0, nrow(meta_summary) - nrow(sensitivity_summary))))[seq_len(nrow(meta_summary))],
    sensitivity_impact = c(sensitivity_summary$impact, rep(NA_character_, max(0, nrow(meta_summary) - nrow(sensitivity_summary))))[seq_len(nrow(meta_summary))]
  )
  write_multi_table_local(table4, file.path(out_root, "tables", "main", "table_main_4_heterogeneity_and_sensitivity_summary"))

  st1 <- wave_long[, .(rows = .N, ids = uniqueN(respondent_id), waves = paste(sort(unique(wave)), collapse = ",")), by = cohort]
  write_multi_table_local(st1, file.path(out_root, "tables", "supplement", "st1_cohort_overview"))

  vm <- fread(file.path(root, "docs", "variable_mapping.csv"))
  write_multi_table_local(vm[harmonized_name %in% c("partnered", "has_living_child") & cohort != "LASI"], file.path(out_root, "tables", "supplement", "st2_exposure_variable_mapping"))
  write_multi_table_local(vm[harmonized_name == "adl5_count" & cohort != "LASI"], file.path(out_root, "tables", "supplement", "st3_outcome_variable_mapping"))

  dl <- readLines(file.path(root, "docs", "decision_log.md"), warn = FALSE)
  st4 <- data.table(entry = c(dl, paste0("Six-country rerun generated at ", timestamp_now(), " with LASI removed from descriptive and analytic products.")))
  write_multi_table_local(st4, file.path(out_root, "tables", "supplement", "st4_harmonization_decision_ledger"))

  st5 <- data.table(
    item = c("SHARE", "Wealth proxy", "ADL outcome", "LASI exclusion"),
    note = c(
      "Retained as cohort-country units using SHARE country identifier.",
      "Within-wave tertiles built from first available wealth-like total or household income proxy.",
      "Reconstructed from five ADL items rather than imported summary totals.",
      "LASI removed for six-country rerun because the current local LASI snapshot did not contribute interval-level transitions."
    )
  )
  write_multi_table_local(st5, file.path(out_root, "tables", "supplement", "st5_comparability_notes"))

  st6 <- wave_long[, lapply(.SD, function(x) mean(is.na(x))), by = .(cohort, wave), .SDcols = c("partnered", "has_living_child", "adl5_count", "wealth_total", "self_rated_health")]
  write_multi_table_local(st6, file.path(out_root, "tables", "supplement", "st6_missingness_by_cohort_wave"))

  write_multi_table_local(exclusion_dt, file.path(out_root, "tables", "supplement", "st7_included_vs_excluded"))
  st8 <- intervals[, .(n = .N, age_mean = mean(age, na.rm = TRUE), partnered_pct = mean(partnered == 1, na.rm = TRUE)), by = cohort_country]
  write_multi_table_local(st8, file.path(out_root, "tables", "supplement", "st8_cohort_country_descriptives"))
  write_multi_table_local(one_stage, file.path(out_root, "tables", "supplement", "st9_full_main_coefficients"))
  write_multi_table_local(meta_summary, file.path(out_root, "tables", "supplement", "st10_meta_analysis_results"))
  write_multi_table_local(subgroup_summary, file.path(out_root, "tables", "supplement", "st11_full_interactions"))
  write_multi_table_local(sensitivity_summary, file.path(out_root, "tables", "supplement", "st12_full_sensitivity_results"))
  st13 <- one_stage[, .(transitions = paste(sort(unique(y.level)), collapse = ","), terms = .N), by = origin_state]
  write_multi_table_local(st13, file.path(out_root, "tables", "supplement", "st13_model_diagnostics"))
  st14 <- data.table(
    cohort = c("CHARLS", "ELSA", "HRS", "KLoSA", "MHAS", "SHARE"),
    weight_strategy = c(
      "working/raw supplement person-level weights when available",
      "harmonized ELSA cross-sectional or longitudinal person weights",
      "RAND / Gateway HRS person-level or longitudinal weights",
      "harmonized KLoSA person-level and longitudinal weights",
      "MHAS person-level analysis weight",
      "SHARE person-level analysis and longitudinal weights"
    )
  )
  write_multi_table_local(st14, file.path(out_root, "tables", "supplement", "st14_weight_specifications"))

  # Six-country figure pack
  pal <- make_palette()
  cohort_order <- c("CHARLS", "HRS", "KLoSA", "MHAS", "SHARE", "ELSA")
  sfcr_levels <- c("neither", "child_only", "partner_only", "partner_child")
  sfcr_labels <- c(neither = "Neither", child_only = "Child only", partner_only = "Partner only", partner_child = "Partner + child")
  world_df <- as.data.table(map_data("world"))

  fig_theme <- theme_minimal(base_size = 9) +
    theme(
      panel.grid.major = element_line(color = "#E7E3EE", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "#F7F5FB", color = "#D3CCDE"),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(color = "#2F3640"),
      axis.text.y = element_text(color = "#2F3640"),
      axis.title = element_text(color = "#2F3640"),
      legend.title = element_text(color = "#2F3640"),
      legend.text = element_text(color = "#2F3640")
    )

  cohort_contrib <- intervals[, .(respondents = uniqueN(respondent_id), intervals = .N), by = cohort]
  cohort_contrib[, cohort := factor(cohort, levels = rev(cohort_order))]
  timeline <- unique(wave_long[!is.na(interview_year), .(cohort, interview_year)])
  timeline[, cohort := factor(cohort, levels = rev(cohort_order))]
  coverage_country <- intervals[, .(analytic_intervals = .N), by = cohort_country]
  coverage_country[, region := country_clean(cohort_country)]
  coverage_world <- merge(world_df, coverage_country, by = "region", all.x = TRUE)
  excl_fig <- copy(exclusion_dt)
  excl_fig[is.na(include_reason) | include_reason == "", include_reason := "other"]
  excl_fig[, include_reason := factor(include_reason, levels = c("included", "missing_next_state", "age_lt_50", "missing_state_t", "other"),
                                      labels = c("Included", "Missing next state", "Age < 50", "Missing baseline state", "Other"))]
  write_src(cohort_contrib, "Figure_1A_cohort_contribution")
  write_src(timeline, "Figure_1B_wave_timeline")
  write_src(coverage_country, "Figure_1C_world_coverage")
  write_src(excl_fig, "Figure_1D_exclusions")

  p1a <- ggplot(cohort_contrib, aes(cohort, intervals)) +
    geom_col(fill = "#B9CAE7", color = pal["blue"], width = 0.72) +
    coord_flip() + scale_y_continuous(labels = label_comma()) +
    labs(title = "Cohort contribution", x = NULL, y = "Intervals") + fig_theme
  p1b <- ggplot(timeline, aes(interview_year, cohort)) +
    geom_count(shape = 21, fill = "#F0D4CC", color = pal["red"], stroke = 0.3) +
    labs(title = "Wave timeline", x = "Interview year", y = NULL) + fig_theme
  p1c <- ggplot(coverage_world, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = analytic_intervals), color = "white", linewidth = 0.15) +
    scale_fill_gradient(low = "#EEF4FB", high = pal["blue"], labels = label_comma(), na.value = "#F3F0F8") +
    coord_quickmap(xlim = c(-150, 160), ylim = c(-42, 78), expand = FALSE) +
    labs(title = "Coverage map", fill = "Intervals") + theme_void(base_size = 9)
  p1d <- ggplot(excl_fig, aes(cohort, N, fill = include_reason)) +
    geom_col(color = "white", linewidth = 0.25) +
    scale_fill_manual(values = c("Included" = pal["blue"], "Missing next state" = pal["yellow"], "Age < 50" = "#E8B4AA", "Missing baseline state" = pal["red"], "Other" = "#BEB7CC")) +
    scale_y_continuous(labels = label_comma()) +
    labs(title = "Inclusion and exclusion", x = NULL, y = "Rows", fill = NULL) + fig_theme +
    theme(axis.text.x = element_text(angle = 18, hjust = 1), legend.position = "bottom")
  fig1 <- (p1a + p1b) / (p1c + p1d) +
    plot_annotation(title = "Figure 1. Study design and coverage (six-country rerun)")
  save_plot_local(fig1, file.path(out_root, "figures_submission_ready", "main", "Figure_1_study_design_and_coverage"), width = 13.2, height = 9.0, dpi = 360)

  baseline_fig <- baseline[!is.na(sfcr_cat4)]
  baseline_fig[, sfcr_cat4 := factor(sfcr_cat4, levels = sfcr_levels, labels = unname(sfcr_labels[sfcr_levels]))]
  sfcr_dist <- baseline_fig[, .N, by = .(cohort, sfcr_cat4)][, prop := N / sum(N), by = cohort]
  trans <- intervals[, .N, by = .(state_label, next_state_label)][, prop := N / sum(N), by = state_label]
  adl_prev <- wave_long[, .(
    Bathing = mean(adl_bathing == 1, na.rm = TRUE),
    Dressing = mean(adl_dressing == 1, na.rm = TRUE),
    Eating = mean(adl_eating == 1, na.rm = TRUE),
    `Bed transfer` = mean(adl_bed == 1, na.rm = TRUE),
    Toileting = mean(adl_toileting == 1, na.rm = TRUE)
  ), by = cohort]
  adl_prev <- melt(adl_prev, id.vars = "cohort", variable.name = "ADL item", value.name = "prevalence")
  interval_box <- intervals[, .(cohort, interval_length)]
  write_src(sfcr_dist, "Figure_2A_sfcr_distribution")
  write_src(trans, "Figure_2B_observed_transitions")
  write_src(adl_prev, "Figure_2C_adl_prevalence")
  write_src(interval_box, "Figure_2D_interval_length")
  p2a <- ggplot(sfcr_dist, aes(cohort, prop, fill = sfcr_cat4)) +
    geom_col(color = "white", linewidth = 0.25) +
    scale_fill_manual(values = c("Neither" = "#D6D0E8", "Child only" = "#E8B4AA", "Partner only" = "#BCD0EC", "Partner + child" = "#6957A8")) +
    scale_y_continuous(labels = percent_format()) +
    labs(title = "Baseline SFCR structure", x = NULL, y = "Share", fill = NULL) + fig_theme +
    theme(axis.text.x = element_text(angle = 18, hjust = 1), legend.position = "bottom")
  p2b <- ggplot(trans, aes(state_label, prop, fill = next_state_label)) +
    geom_col(color = "white", linewidth = 0.25) +
    scale_fill_manual(values = c("independent" = "#BCD0EC", "disabled" = "#6957A8", "dead" = "#B45A4D")) +
    scale_y_continuous(labels = percent_format()) +
    labs(title = "Observed transitions", x = NULL, y = "Share", fill = NULL) + fig_theme +
    theme(legend.position = "bottom")
  p2c <- ggplot(adl_prev, aes(`ADL item`, cohort, fill = prevalence)) +
    geom_tile(color = "white", linewidth = 0.3) +
    scale_fill_gradient(low = "#FDF3F0", high = pal["red"], labels = percent_format()) +
    labs(title = "ADL item prevalence", x = NULL, y = NULL, fill = "Prev") + fig_theme +
    theme(axis.text.x = element_text(angle = 18, hjust = 1))
  p2d <- ggplot(interval_box, aes(cohort, interval_length, fill = cohort)) +
    geom_violin(color = NA, alpha = 0.8) +
    geom_boxplot(width = 0.12, fill = "white", outlier.size = 0.2) +
    guides(fill = "none") +
    labs(title = "Interval length", x = NULL, y = "Years") + fig_theme +
    theme(axis.text.x = element_text(angle = 18, hjust = 1))
  fig2 <- (p2a + p2b) / (p2c + p2d) +
    plot_annotation(title = "Figure 2. Structural family reserve and transition landscape (six-country rerun)")
  save_plot_local(fig2, file.path(out_root, "figures_submission_ready", "main", "Figure_2_sfcr_and_transition_landscape"), width = 13.2, height = 9.0, dpi = 360)

  eff_raw <- copy(one_stage[grepl("^sfcr_cat4", term)])
  eff_raw[, contrast := sub("^sfcr_cat4", "", term)]
  eff_indep <- eff_raw[origin_state == "independent" & y.level %in% c("disabled", "dead")]
  eff_indep[, transition := fifelse(y.level == "disabled", "Independent -> Disabled", "Independent -> Dead")]
  eff_indep[, `:=`(log_est = estimate, log_lo = conf.low, log_hi = conf.high)]
  eff_recov <- eff_raw[origin_state == "disabled" & y.level == "disabled"]
  eff_recov[, transition := "Disabled -> Independent"]
  eff_recov[, `:=`(log_est = -estimate, log_lo = -conf.high, log_hi = -conf.low)]
  eff_dead <- eff_raw[origin_state == "disabled" & y.level == "dead"]
  eff_dead[, transition := "Disabled -> Dead"]
  eff_dead[, `:=`(log_est = estimate, log_lo = conf.low, log_hi = conf.high)]
  eff_plot <- rbindlist(list(eff_indep, eff_recov, eff_dead), fill = TRUE)
  eff_plot[, contrast := factor(contrast, levels = c("partner_child", "partner_only", "child_only"), labels = c("Partner + child", "Partner only", "Child only"))]
  eff_plot[, transition := factor(transition, levels = c("Independent -> Disabled", "Independent -> Dead", "Disabled -> Independent", "Disabled -> Dead"))]
  eff_plot[, `:=`(rrr = exp(log_est), rrr_lo = exp(log_lo), rrr_hi = exp(log_hi))]
  std_probs2 <- copy(std_probs)
  std_probs2[, origin_state := factor(origin_state, levels = c(0, 1), labels = c("Initially independent", "Initially disabled"))]
  std_probs2[, sfcr_cat4 := factor(sfcr_cat4, levels = sfcr_levels, labels = unname(sfcr_labels[sfcr_levels]))]
  std_long <- melt(std_probs2, id.vars = c("origin_state", "sfcr_cat4"), variable.name = "next_state", value.name = "prob")
  abs_diff <- dcast(std_long, origin_state + next_state ~ sfcr_cat4, value.var = "prob")
  abs_diff[, risk_difference := `Partner + child` - Neither]
  decomp <- copy(eff_plot[, .(transition, contrast, rrr, rrr_lo, rrr_hi)])
  write_src(eff_plot, "Figure_3A_transition_effects")
  write_src(std_long, "Figure_3B_standardized_probabilities")
  write_src(abs_diff, "Figure_3C_absolute_risk_difference")
  write_src(decomp, "Figure_3D_partner_child_decomposition")
  p3a <- ggplot(eff_plot, aes(contrast, rrr, ymin = rrr_lo, ymax = rrr_hi, color = contrast)) +
    geom_hline(yintercept = 1, linetype = 2, linewidth = 0.3, color = "#BBB4C9") +
    geom_pointrange() +
    facet_wrap(~ transition, ncol = 2) +
    scale_y_log10() +
    labs(title = "Transition-specific pooled associations", x = NULL, y = "RRR", color = NULL) + fig_theme +
    theme(axis.text.x = element_text(angle = 18, hjust = 1), legend.position = "bottom")
  p3b <- ggplot(std_long, aes(sfcr_cat4, prob, group = next_state, color = next_state)) +
    geom_line(linewidth = 0.6) + geom_point(size = 1.4) +
    facet_wrap(~ origin_state, ncol = 1) +
    scale_y_continuous(labels = percent_format()) +
    labs(title = "Standardized probabilities", x = NULL, y = "Probability", color = NULL) + fig_theme +
    theme(axis.text.x = element_text(angle = 18, hjust = 1), legend.position = "bottom")
  p3c <- ggplot(abs_diff, aes(next_state, risk_difference, fill = next_state)) +
    geom_hline(yintercept = 0, linetype = 2, linewidth = 0.3, color = "#BBB4C9") +
    geom_col(color = "white", linewidth = 0.25) +
    scale_y_continuous(labels = percent_format()) +
    labs(title = "Absolute contrast: partner + child vs neither", x = NULL, y = "Risk difference", fill = NULL) + fig_theme
  p3d <- ggplot(decomp, aes(transition, rrr, ymin = rrr_lo, ymax = rrr_hi, color = contrast)) +
    geom_hline(yintercept = 1, linetype = 2, linewidth = 0.3, color = "#BBB4C9") +
    geom_pointrange(position = position_dodge(width = 0.45)) +
    scale_y_log10() +
    labs(title = "Partner-child decomposition", x = NULL, y = "RRR", color = NULL) + fig_theme +
    theme(axis.text.x = element_text(angle = 18, hjust = 1), legend.position = "bottom")
  fig3 <- (p3a + p3b) / (p3c + p3d) +
    plot_annotation(title = "Figure 3. Pooled effects and probabilities (six-country rerun)")
  save_plot_local(fig3, file.path(out_root, "figures_submission_ready", "main", "Figure_3_pooled_effects_and_probabilities"), width = 13.3, height = 9.2, dpi = 360)

  meta_plot <- copy(meta_summary)
  meta_plot[, transition := factor(transition, levels = c("independent_to_disabled", "independent_to_dead", "disabled_to_independent", "disabled_to_dead"),
                                   labels = c("Independent -> Disabled", "Independent -> Dead", "Disabled -> Independent", "Disabled -> Dead"))]
  meta_plot[, `:=`(or = exp(pooled_estimate), or_lo = exp(ci_lb), or_hi = exp(ci_ub))]
  dual_map_dt <- intervals[, .N, by = .(cohort, cohort_country, state_label, next_state_label)]
  dual_map_dt[, denom := sum(N), by = .(cohort, cohort_country, state_label)]
  dual_map_dt[, prob := N / denom]
  dual_map_dt[cohort_country %in% names(share_country_map), cohort_country := share_country_map[cohort_country]]
  size_keep <- intervals[, .N, by = .(cohort, cohort_country)]
  size_keep[cohort_country %in% names(share_country_map), cohort_country := share_country_map[cohort_country]]
  size_keep <- size_keep[, .(analytic_intervals = sum(N, na.rm = TRUE)), by = .(cohort, cohort_country)][analytic_intervals >= 1000]
  dual_map_dt <- merge(dual_map_dt, size_keep, by = c("cohort", "cohort_country"))
  dual_map_dt <- dual_map_dt[state_label == "disabled" & next_state_label %in% c("independent", "dead")]
  dual_map_dt[, transition := fifelse(next_state_label == "independent", "Disabled -> Independent", "Disabled -> Dead")]
  dual_map_dt[, region := country_clean(cohort_country)]
  write_src(meta_plot, "Figure_4A_meta_summary")
  write_src(dual_map_dt, "Figure_4BC_maps")
  p4a <- ggplot(meta_plot, aes(transition, or, ymin = or_lo, ymax = or_hi)) +
    geom_hline(yintercept = 1, linetype = 2, linewidth = 0.3, color = "#BBB4C9") +
    geom_pointrange(color = "#6957A8") +
    coord_flip() + scale_y_log10() +
    labs(title = "Two-stage meta summary", x = NULL, y = "OR") + fig_theme
  p4b <- ggplot(dual_map_dt[transition == "Disabled -> Independent"], aes(cohort, prob, fill = cohort)) +
    geom_boxplot(outlier.size = 0.35) + scale_y_continuous(labels = percent_format()) +
    guides(fill = "none") + labs(title = "Recovery probabilities across cohort-country units", x = NULL, y = "Probability") + fig_theme +
    theme(axis.text.x = element_text(angle = 18, hjust = 1))
  p4c <- ggplot(dual_map_dt[transition == "Disabled -> Dead"], aes(cohort, prob, fill = cohort)) +
    geom_boxplot(outlier.size = 0.35) + scale_y_continuous(labels = percent_format()) +
    guides(fill = "none") + labs(title = "Death probabilities across cohort-country units", x = NULL, y = "Probability") + fig_theme +
    theme(axis.text.x = element_text(angle = 18, hjust = 1))
  p4d <- ggplot(dual_map_dt, aes(cohort_country, transition, fill = prob)) +
    geom_tile(color = "white", linewidth = 0.25) +
    scale_fill_gradient(low = "#EEF4FB", high = pal["red"], labels = percent_format()) +
    labs(title = "Cohort-country heatmap", x = NULL, y = NULL, fill = "Prob") + fig_theme +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, vjust = 1, size = 5.8))
  fig4 <- (p4a + p4b) / (p4c + p4d) +
    plot_annotation(title = "Figure 4. Cross-national heterogeneity (six-country rerun)")
  save_plot_local(fig4, file.path(out_root, "figures_submission_ready", "main", "Figure_4_cross_national_heterogeneity"), width = 13.4, height = 10.0, dpi = 360)

  sg_base <- copy(intervals[wealth_tertile %in% c("T1", "T2", "T3") & sfcr_cat4 %in% c("partner_child", "neither")])
  sg_base[, sex_clean := fifelse(grepl("濂硘Female|female|婵?", sex), "Female", fifelse(grepl("鐢穦Male|male|閻?", sex), "Male", NA_character_))]
  sg_base <- sg_base[!is.na(sex_clean) & age_group %in% c("50-64", "65-74", "75+")]
  sg_agg <- sg_base[, .(disabled_next = mean(next_state == 1, na.rm = TRUE), dead_next = mean(next_state == 2, na.rm = TRUE)), by = .(sex_clean, age_group, wealth_tertile, sfcr_cat4)]
  sg_wide <- dcast(sg_agg, sex_clean + age_group + wealth_tertile ~ sfcr_cat4, value.var = c("disabled_next", "dead_next"))
  subgroup_benefit <- sg_wide[, .(sex_clean, age_group, wealth_tertile, lower_disabled = disabled_next_neither - disabled_next_partner_child, lower_death = dead_next_neither - dead_next_partner_child)]
  subgroup_benefit <- melt(subgroup_benefit, id.vars = c("sex_clean", "age_group", "wealth_tertile"), variable.name = "metric", value.name = "benefit")
  onset_sum <- onset_follow[sfcr_cat4 %in% sfcr_levels, .N, by = .(sfcr_cat4, next_state_label)][, prop := N / sum(N), by = sfcr_cat4]
  proxy_dt <- intervals[state_label == "disabled" & !is.na(sfcr_cat4), .(recovered = mean(next_state == 0, na.rm = TRUE), dead = mean(next_state == 2, na.rm = TRUE)), by = .(proxy_group = fifelse(is.na(proxy), "Unknown", fifelse(proxy == 1, "Proxy", "Self")), sfcr_cat4)]
  proxy_long <- melt(proxy_dt, id.vars = c("proxy_group", "sfcr_cat4"), variable.name = "transition", value.name = "prob")
  strict_prev <- wave_long[, .(default_adl = mean(adl5_count >= 1, na.rm = TRUE), strict_adl = mean(adl5_count_strict >= 1, na.rm = TRUE)), by = cohort]
  strict_prev <- melt(strict_prev, id.vars = "cohort", variable.name = "definition", value.name = "prevalence")
  write_src(subgroup_benefit, "Figure_5A_subgroup_benefit")
  write_src(onset_sum, "Figure_5B_first_onset_landmark")
  write_src(proxy_long, "Figure_5C_proxy_sensitivity")
  write_src(strict_prev, "Figure_5D_strict_adl")
  p5a <- ggplot(subgroup_benefit, aes(age_group, interaction(sex_clean, wealth_tertile), fill = benefit)) +
    geom_tile(color = "white", linewidth = 0.25) +
    scale_fill_gradient2(low = "#E8B4AA", mid = "white", high = pal["blue"], midpoint = 0, labels = percent_format()) +
    facet_wrap(~ metric, ncol = 1) +
    labs(title = "Subgroup benefit", x = NULL, y = NULL, fill = "Benefit") + fig_theme
  p5b <- ggplot(onset_sum, aes(sfcr_cat4, prop, fill = next_state_label)) +
    geom_col(color = "white", linewidth = 0.25) +
    scale_y_continuous(labels = percent_format()) +
    labs(title = "First-onset landmark", x = NULL, y = "Share", fill = NULL) + fig_theme +
    theme(axis.text.x = element_text(angle = 18, hjust = 1), legend.position = "bottom")
  p5c <- ggplot(proxy_long, aes(sfcr_cat4, prob, color = proxy_group, group = proxy_group)) +
    geom_line(linewidth = 0.6) + geom_point(size = 1.4) +
    facet_wrap(~ transition, ncol = 1) +
    scale_y_continuous(labels = percent_format()) +
    labs(title = "Proxy sensitivity", x = NULL, y = "Probability", color = NULL) + fig_theme +
    theme(axis.text.x = element_text(angle = 18, hjust = 1), legend.position = "bottom")
  p5d <- ggplot(strict_prev, aes(cohort, prevalence, fill = definition)) +
    geom_col(position = "dodge", color = "white", linewidth = 0.25) +
    scale_y_continuous(labels = percent_format()) +
    labs(title = "Strict ADL sensitivity", x = NULL, y = "Prevalence", fill = NULL) + fig_theme +
    theme(axis.text.x = element_text(angle = 18, hjust = 1), legend.position = "bottom")
  fig5 <- (p5a + p5b) / (p5c + p5d) +
    plot_annotation(title = "Figure 5. Extension and robustness (six-country rerun)")
  save_plot_local(fig5, file.path(out_root, "figures_submission_ready", "main", "Figure_5_extension_and_robustness"), width = 13.3, height = 9.2, dpi = 360)

  readme_lines <- c(
    "# Six-Country Rerun",
    "",
    "This folder contains a clean rerun of the pooled project after dropping LASI from both descriptive and analytic products.",
    "",
    "Important note:",
    "- LASI did not contribute adjacent-wave intervals in the original local snapshot, so interval-based main model estimates should match or differ only trivially from the prior run.",
    "- The main changes in this rerun are the explicit removal of LASI from cohort overviews, baseline characteristics, descriptive figures, atlas panels, and supplement tables."
  )
  writeLines(readme_lines, file.path(out_root, "README.md"))
  append_decision_log("Generated a standalone six-country rerun bundle that removes LASI from descriptive and analytic products without overwriting the original seven-cohort workspace outputs.")
  invisible(out_root)
}
