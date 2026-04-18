#!/usr/bin/env Rscript

.abort <- function(message) {
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_abort(message)
  }
  stop(message, call. = FALSE)
}

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), winslash = "/", mustWork = FALSE)
} else {
  NULL
}

if (!requireNamespace("rollNonlinear", quietly = TRUE)) {
  repo_root <- if (!is.null(script_path)) {
    dirname(dirname(dirname(script_path)))
  } else {
    getwd()
  }

  if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists(file.path(repo_root, "DESCRIPTION"))) {
    pkgload::load_all(repo_root, export_all = FALSE, helpers = FALSE, quiet = TRUE)
  } else {
    .abort("Package {.pkg rollNonlinear} must be installed, or {.pkg pkgload} must be available when running from the source tree.")
  }
}

rollNonlinear:::.cli_roll_yahoo_main()
