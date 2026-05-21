source(file.path("src", "00_utils", "utils.R"))

run_main_models <- function(interval_obj) {
  suppressPackageStartupMessages({
    library(nnet)
    library(splines)
    library(broom)
  })

  dt <- copy(interval_obj$intervals)
  dt <- dt[
    !is.na(sfcr_cat4) &
      !is.na(age) &
      !is.na(sex) &
      !is.na(education) &
      !is.na(chronic_count_raw)
  ]

  dt[, sex := factor(sex)]
  dt[, education := factor(education)]
  dt[, wealth_tertile := factor(wealth_tertile)]
  dt[, cohort_country := factor(cohort_country)]

  origin0 <- dt[state == 0]
  origin1 <- dt[state == 1]

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
  fwrite(one_stage, file.path(project_root(), "data_derived", "models", "one_stage_multinom_coefficients.csv"))

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

  std_probs <- rbindlist(list(std_fun(dt, mod0, 0L), std_fun(dt, mod1, 1L)), fill = TRUE)
  fwrite(std_probs, file.path(project_root(), "outputs", "results_summary", "standardized_transition_probabilities.csv"))
  saveRDS(list(mod0 = mod0, mod1 = mod1), file.path(project_root(), "data_derived", "models", "one_stage_models.rds"))
  append_decision_log("Fit primary one-stage multinomial models stratified by origin state and generated standardized transition probabilities by SFCR category.")
  list(one_stage = one_stage, std_probs = std_probs, models = list(mod0 = mod0, mod1 = mod1), analysis_data = dt)
}
