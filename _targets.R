library(targets)
library(tarchetypes)

source(file.path("R", "00_utils.R"))
source(file.path("R", "01_manifest.R"))
source(file.path("R", "02_prepare_long.R"))
source(file.path("R", "03_prepare_supplements.R"))
source(file.path("R", "04_harmonize.R"))
source(file.path("R", "05_codebooks.R"))
source(file.path("R", "06_intervals.R"))
source(file.path("R", "07_models.R"))
source(file.path("R", "08_meta_summary.R"))
source(file.path("R", "09_tables.R"))
source(file.path("R", "10_six_country_run.R"))

tar_option_set(
  packages = c(
    "data.table", "haven", "yaml", "openxlsx", "stringr", "fs",
    "ggplot2", "patchwork", "nnet", "splines", "broom", "metafor",
    "scales", "maps", "gt", "dplyr", "tidyr", "ggrepel", "svglite",
    "ragg", "readr", "gridExtra"
  ),
  format = "rds"
)

list(
  tar_target(cohort_manifest, build_cohort_manifest()),
  tar_target(working_long, prepare_working_data()),
  tar_target(supplement_long, prepare_supplement_sources()),
  tar_target(harmonized_wave_long, build_harmonized_wave_long(working_long, supplement_long)),
  tar_target(codebooks_done, make_codebooks()),
  tar_target(interval_obj, build_interval_data(harmonized_wave_long)),
  tar_target(model_results, run_main_models(interval_obj)),
  tar_target(meta_results, run_meta_and_summary(interval_obj, model_results)),
  tar_target(tables_done, make_tables()),
  tar_target(six_country_bundle, rerun_six_country_bundle()),
  tar_target(nature_figures, {
    source(file.path("R", "11_nature_figures.R"))
    TRUE
  }),
  tar_target(nature_tables, {
    source(file.path("R", "12_nature_tables.R"))
    TRUE
  })
)
