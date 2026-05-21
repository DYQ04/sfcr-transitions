source(file.path("src", "00_utils", "utils.R"))

guess_file_role <- function(path) {
  low <- tolower(path)
  fifelse(
    str_detect(low, "working_data"),
    "working_data",
    fifelse(
      str_detect(low, "harmonized|gateway|easyshare|randhrs|tracker"),
      "harmonized_or_gateway",
      fifelse(
        str_detect(low, "roster|child|household|family|exit|eol"),
        "roster_or_family",
        fifelse(
          str_detect(low, "\\.do$"),
          "dofile",
          fifelse(
            str_detect(low, "codebook|guide|documentation|questionnaire|问卷|pdf|xlsx|rtf|htm"),
            "documentation",
            "other"
          )
        )
      )
    )
  )
}

extract_header_candidates <- function(path) {
  ext <- tolower(tools::file_ext(path))
  out <- list(id_candidates = NA_character_, date_candidates = NA_character_, weight_candidates = NA_character_)
  if (!ext %in% c("dta", "sav", "csv")) return(out)
  nms <- tryCatch({
    if (ext == "csv") names(fread(path, nrows = 0, showProgress = FALSE))
    else names(read_dta(path, n_max = 0))
  }, error = function(e) character())
  if (!length(nms)) return(out)
  out$id_candidates <- paste(nms[str_detect(nms, regex("id$|^id$|hhid|mergeid|idauniq|hhidpn|pid", ignore_case = TRUE))], collapse = "; ")
  out$date_candidates <- paste(nms[str_detect(nms, regex("wave|year|month|date|iw", ignore_case = TRUE))], collapse = "; ")
  out$weight_candidates <- paste(nms[str_detect(nms, regex("weight|wt", ignore_case = TRUE))], collapse = "; ")
  out
}

build_cohort_manifest <- function() {
  catlog <- cohort_catalog()
  root <- source_root()
  all_files <- list.files(root, recursive = TRUE, full.names = TRUE)

  rows <- rbindlist(lapply(names(catlog), function(cohort) {
    files <- all_files[str_detect(all_files, regex(cohort, ignore_case = TRUE))]
    if (!length(files)) return(NULL)
    rbindlist(lapply(files, function(path) {
      info <- file.info(path)
      hdr <- extract_header_candidates(path)
      data.table(
        cohort = cohort,
        file_path = normalizePath(path, winslash = "/", mustWork = FALSE),
        file_name = basename(path),
        file_ext = tools::file_ext(path),
        file_role = guess_file_role(path),
        file_size = info$size,
        readable = as.integer(file.access(path, 4) == 0),
        suspected_main = as.integer(str_detect(tolower(path), "working_data/.+\\.dta$|randhrs1992_2020v2\\.dta$|h_.*\\.dta$|easyshare")),
        suspected_harmonized = as.integer(str_detect(tolower(path), "harmonized|gateway|easyshare|randhrs")),
        suspected_roster = as.integer(str_detect(tolower(path), "child|roster|family|household|exit|eol")),
        wave_hint = str_extract(basename(path), "(wave[_ ]?[0-9]+|[0-9]{4})"),
        id_candidates = hdr$id_candidates,
        date_candidates = hdr$date_candidates,
        weight_candidates = hdr$weight_candidates
      )
    }), fill = TRUE)
  }), fill = TRUE)

  rows <- unique(rows)
  fwrite(rows, file.path(project_root(), "config", "cohort_manifest.csv"))
  fwrite(rows, file.path(project_root(), "data_intermediate", "audit", "cohort_file_inventory.csv"))
  append_decision_log("Generated initial cohort manifest and file inventory through automated recursive scanning.")
  rows
}
