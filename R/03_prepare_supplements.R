source(file.path("src", "00_utils", "utils.R"))

prepare_supplement_sources <- function() {
  catlog <- cohort_catalog()
  pm <- read_priority_map()$supplement_wide

  specs <- list(
    CHARLS = list(id_var = "ID", waves = 1:5, file = catlog$CHARLS$raw_main[[1]]),
    ELSA = list(id_var = "idauniq", waves = 1:9, file = catlog$ELSA$raw_main[[1]]),
    HRS = list(id_var = "hhidpn", waves = 1:15, file = catlog$HRS$raw_wealth[[1]]),
    KLoSA = list(id_var = "pid", waves = 1:8, file = catlog$KLoSA$raw_main[[1]]),
    LASI = list(id_var = "hhidpn", waves = 1, file = catlog$LASI$raw_main[[1]]),
    MHAS = list(id_var = "rahhidnp", waves = 1:5, file = catlog$MHAS$raw_main[[1]]),
    SHARE = list(id_var = "mergeid", waves = c(1, 2, 4, 5, 6, 7, 8), file = catlog$SHARE$raw_main[[1]])
  )

  out <- rbindlist(lapply(names(specs), function(cohort) {
    spec <- specs[[cohort]]
    if (is.na(spec$file) || !file.exists(spec$file)) return(NULL)
    dt <- as.data.table(read_dta(spec$file))
    if (!spec$id_var %in% names(dt)) {
      spec$id_var <- first_existing(names(dt), c("ID", "idauniq", "hhidpn", "pid", "rahhidnp", "mergeid", "hhid"))
    }
    if (is.na(spec$id_var)) return(NULL)
    var_specs <- list(
      wealth_supp = pm$wealth_candidates,
      child_count_supp = pm$child_count_candidates,
      co_resident_child_supp = pm$co_resident_child_candidates,
      child_contact_supp = pm$child_contact_candidates,
      partner_status_supp = pm$partner_candidates,
      proxy_supp = pm$proxy_candidates,
      weight_supp = pm$weight_candidates,
      shlt_supp = pm$shlt_candidates
    )
    long <- create_wave_supplement(dt, cohort, spec$id_var, spec$waves, var_specs)
    if ("country" %in% names(dt)) {
      long[, country_raw := dt[match(respondent_id, as.character(dt[[spec$id_var]])), as.character(country)]]
    }
    long
  }), fill = TRUE)

  fwrite(out, file.path(project_root(), "data_intermediate", "harmonized", "supplement_long.csv"))
  saveRDS(out, file.path(project_root(), "data_intermediate", "harmonized", "supplement_long.rds"))
  append_decision_log("Extracted supplemental wide-to-long variables from harmonized, Gateway, RAND, and SHARE files for wealth, child count, child co-residence/contact, proxy, and SHARE country.")
  out
}
