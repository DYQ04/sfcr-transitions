if (!requireNamespace("targets", quietly = TRUE)) {
  stop("Package 'targets' is required. Run renv::restore() first.")
}

targets::tar_make()
