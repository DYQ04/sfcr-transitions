suppressPackageStartupMessages({
  library(data.table)
  library(haven)
  library(yaml)
  library(openxlsx)
  library(stringr)
  library(fs)
  library(ggplot2)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x

project_root <- function() normalizePath(".", winslash = "/", mustWork = TRUE)

read_project_config <- function() {
  yaml::read_yaml(file.path(project_root(), "config", "project_config.yml"))
}

read_priority_map <- function() {
  yaml::read_yaml(file.path(project_root(), "config", "variable_priority_map.yml"))
}

source_root <- function() {
  cfg <- read_project_config()
  normalizePath(file.path(project_root(), cfg$source_root_relative), winslash = "/", mustWork = TRUE)
}

check_required_packages <- function() {
  pkgs <- c(
    "data.table", "haven", "yaml", "openxlsx", "readxl", "writexl",
    "stringr", "janitor", "ggplot2", "patchwork", "targets",
    "tarchetypes", "mice", "naniar", "nnet", "survival", "metafor",
    "broom", "dplyr", "tidyr", "purrr", "gt", "gtsummary",
    "sandwich", "lmtest", "clubSandwich", "splines"
  )
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Missing required packages: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

timestamp_now <- function() format(Sys.time(), "%Y-%m-%d %H:%M %Z")

append_decision_log <- function(text) {
  path <- file.path(project_root(), "docs", "decision_log.md")
  cat(paste0("\n## ", timestamp_now(), "\n\n- ", text, "\n"), file = path, append = TRUE)
  invisible(path)
}

append_discard_log <- function(cohort, variable_name, candidate_use, discard_reason, notes = "") {
  path <- file.path(project_root(), "docs", "discarded_variables.csv")
  dt <- data.table::fread(path)
  dt <- rbind(
    dt,
    data.table(
      cohort = cohort,
      variable_name = variable_name,
      candidate_use = candidate_use,
      discard_reason = discard_reason,
      notes = notes
    ),
    fill = TRUE
  )
  data.table::fwrite(dt, path)
}

save_plot_dual <- function(plot_obj, stem, width = 10, height = 7, dpi = 320) {
  dir_create(path_dir(stem))
  ggsave(paste0(stem, ".png"), plot_obj, width = width, height = height, dpi = dpi)
  ggsave(paste0(stem, ".pdf"), plot_obj, width = width, height = height, device = cairo_pdf)
}

write_multi_table <- function(dt, stem) {
  dir_create(path_dir(stem))
  data.table::fwrite(dt, paste0(stem, ".csv"))
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "data")
  openxlsx::writeData(wb, "data", dt)
  openxlsx::saveWorkbook(wb, paste0(stem, ".xlsx"), overwrite = TRUE)
  gt_tbl <- gt::gt(as.data.frame(dt))
  gt::gtsave(gt_tbl, paste0(stem, ".html"))
  try(gt::gtsave(gt_tbl, paste0(stem, ".rtf")), silent = TRUE)
}

safe_label <- function(x) {
  lab <- attr(x, "label")
  if (is.null(lab)) return("")
  paste(as.character(lab), collapse = " ")
}

as_label_factor <- function(x) haven::as_factor(x)

first_existing <- function(nms, choices) {
  hit <- choices[choices %in% nms]
  if (length(hit)) hit[[1]] else NA_character_
}

cohort_catalog <- function() {
  root <- source_root()
  all_dta <- list.files(root, pattern = "\\.dta$", recursive = TRUE, full.names = TRUE)
  list(
    CHARLS = list(
      cohort = "CHARLS",
      working = all_dta[basename(all_dta) == "charls.dta" & str_detect(all_dta, "Working_data")],
      raw_main = all_dta[str_detect(basename(all_dta), "H_CHARLS_[CD]_Data\\.dta")],
      default_country = "China"
    ),
    ELSA = list(
      cohort = "ELSA",
      working = all_dta[basename(all_dta) == "elsa.dta" & str_detect(all_dta, "Working_data")],
      raw_main = all_dta[basename(all_dta) == "h_elsa_g3.dta" & str_detect(all_dta, "Harmonized ELSA")],
      default_country = "United Kingdom"
    ),
    HRS = list(
      cohort = "HRS",
      working = all_dta[basename(all_dta) == "hrs.dta" & str_detect(all_dta, "Working_data")],
      raw_main = all_dta[basename(all_dta) == "H_HRS_d.dta" & str_detect(all_dta, "Gateway Harmonized HRS")],
      raw_wealth = all_dta[basename(all_dta) == "randhrs1992_2020v2.dta"],
      default_country = "United States"
    ),
    KLoSA = list(
      cohort = "KLoSA",
      working = all_dta[basename(all_dta) == "klosa.dta" & str_detect(all_dta, "Working_data")],
      raw_main = all_dta[basename(all_dta) == "H_KLoSA_e2.dta"],
      default_country = "South Korea"
    ),
    LASI = list(
      cohort = "LASI",
      working = all_dta[basename(all_dta) == "lasi.dta" & str_detect(all_dta, "Working_data")],
      raw_main = all_dta[basename(all_dta) == "H_LASI_a3.dta"],
      default_country = "India"
    ),
    MHAS = list(
      cohort = "MHAS",
      working = all_dta[basename(all_dta) == "mhas.dta" & str_detect(all_dta, "Working_data")],
      raw_main = all_dta[basename(all_dta) == "H_MHAS_c2.dta"],
      default_country = "Mexico"
    ),
    SHARE = list(
      cohort = "SHARE",
      working = all_dta[basename(all_dta) == "share.dta" & str_detect(all_dta, "Working_data")],
      raw_main = all_dta[basename(all_dta) == "H_SHARE_f2.dta"],
      default_country = NA_character_
    )
  )
}

marital_partnered <- function(x) {
  z <- tolower(as.character(as_label_factor(x)))
  fifelse(
    str_detect(z, "已婚|married|同居|cohabit|partner|伴侣|注册伴侣"),
    1L,
    fifelse(str_detect(z, "widow|寡|丧偶|divorc|离婚|separ|分居|never|从未"), 0L, NA_integer_)
  )
}

recode_binary_yes <- function(x) {
  z <- tolower(as.character(as_label_factor(x)))
  out <- fifelse(
    str_detect(z, "1\\.yes|^yes$|^是$|需要帮助|有困难|cannot|can't|unable"),
    1L,
    fifelse(str_detect(z, "0\\.no|^no$|^否$|无困难|不需要帮助"), 0L, NA_integer_)
  )
  if (all(is.na(out))) {
    num <- suppressWarnings(as.numeric(x))
    out <- fifelse(num %in% 1, 1L, fifelse(num %in% 0, 0L, NA_integer_))
  }
  out
}

recode_yesno_indicator <- function(x) {
  z <- tolower(as.character(as_label_factor(x)))
  out <- fifelse(
    str_detect(z, "^是$|^yes$|1\\.yes|married|已婚|同居|partner"),
    1L,
    fifelse(str_detect(z, "^否$|^no$|0\\.no"), 0L, NA_integer_)
  )
  if (all(is.na(out))) {
    num <- suppressWarnings(as.numeric(x))
    out <- fifelse(num %in% 1, 1L, fifelse(num %in% 0, 0L, NA_integer_))
  }
  out
}

extract_wide_wave_value <- function(dt, wave, candidates) {
  cols <- gsub("\\{wave\\}", as.character(wave), candidates)
  cols <- cols[cols %in% names(dt)]
  if (!length(cols)) return(rep(NA, nrow(dt)))
  out <- haven::zap_labels(dt[[cols[[1]]]])
  if (length(cols) > 1) {
    for (nm in cols[-1]) {
      idx <- is.na(out) | out %in% c("", ".", ".m", ".d")
      out[idx] <- haven::zap_labels(dt[[nm]])[idx]
    }
  }
  out
}

create_wave_supplement <- function(dt, cohort, id_var, wave_values, spec_list) {
  rbindlist(lapply(wave_values, function(wv) {
    base <- data.table(
      cohort = cohort,
      respondent_id = as.character(dt[[id_var]]),
      wave = as.integer(wv)
    )
    for (nm in names(spec_list)) {
      base[[nm]] <- extract_wide_wave_value(dt, wv, spec_list[[nm]])
    }
    base
  }), fill = TRUE)
}

make_palette <- function() {
  c(
    red = "#9F2D20",
    blue = "#1F4E79",
    yellow = "#C58A17",
    charcoal = "#2F3640",
    steel = "#5D6D7E",
    sand = "#D8C3A5"
  )
}
