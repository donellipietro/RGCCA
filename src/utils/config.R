# = ========================================================================== =
# - Script: src/utils/config.R
# - Desc: Helpers for reading repository profiles and exporting shell env files.
# = ========================================================================== =


# Helpers ----

rgcca_first_non_empty <- function(...) {
  values <- list(...)
  for (value in values) {
    if (!is.null(value) && length(value) > 0 && !is.na(value[1]) &&
        nzchar(as.character(value[1]))) {
      return(as.character(value[1]))
    }
  }
  ""
}

rgcca_env <- function(name, default = "") {
  rgcca_first_non_empty(Sys.getenv(paste0("RGCCA_", name), unset = ""), default)
}

rgcca_cfg_value <- function(cfg, name, default = "") {
  value <- if (!is.null(cfg[[name]])) cfg[[name]] else default
  rgcca_env(name, value)
}

rgcca_as_scalar_character <- function(value) {
  if (is.null(value) || length(value) == 0) return("")
  paste(as.character(value), collapse = ",")
}

rgcca_add_derived_paths <- function(cfg) {
  cfg$PATH_REPO <- rgcca_cfg_value(cfg, "PATH_REPO")
  cfg$PATH_TMP <- rgcca_cfg_value(cfg, "PATH_TMP", file.path(cfg$PATH_REPO, "tmp"))

  cfg$PATH_SRC <- rgcca_cfg_value(cfg, "PATH_SRC", file.path(cfg$PATH_REPO, "src"))
  cfg$PATH_TESTS <- rgcca_cfg_value(cfg, "PATH_TESTS", file.path(cfg$PATH_REPO, "tests"))
  cfg$PATH_CPP <- rgcca_cfg_value(cfg, "PATH_CPP", file.path(cfg$PATH_REPO, "cpp"))
  cfg$PATH_DATA <- rgcca_cfg_value(cfg, "PATH_DATA", file.path(cfg$PATH_REPO, "data"))

  cfg$PATH_RESULTS <- rgcca_cfg_value(cfg, "PATH_RESULTS", file.path(cfg$PATH_REPO, "results"))
  cfg$PATH_IMAGES <- rgcca_cfg_value(cfg, "PATH_IMAGES", file.path(cfg$PATH_REPO, "images"))
  cfg$PATH_TEST_DATA <- rgcca_cfg_value(cfg, "PATH_TEST_DATA", file.path(cfg$PATH_REPO, "data/tests"))
  cfg$PATH_QUEUE <- rgcca_cfg_value(cfg, "PATH_QUEUE", file.path(cfg$PATH_TMP, "queue"))
  cfg$PATH_LOGS <- rgcca_cfg_value(cfg, "PATH_LOGS", file.path(cfg$PATH_TMP, "logs"))
  cfg$PATH_TMP_DATA <- rgcca_cfg_value(cfg, "PATH_TMP_DATA", file.path(cfg$PATH_TMP, "data"))
  cfg$PATH_TMP_RESULTS <- rgcca_cfg_value(cfg, "PATH_TMP_RESULTS", file.path(cfg$PATH_TMP, "results"))
  cfg$PATH_BUILD <- rgcca_cfg_value(cfg, "PATH_BUILD", file.path(cfg$PATH_REPO, "build"))

  cfg
}

rgcca_apply_overrides <- function(cfg) {
  for (name in names(cfg)) {
    cfg[[name]] <- rgcca_env(name, cfg[[name]])
  }
  cfg
}


# Public API ----

available_profiles <- function() {
  if (!exists("RGCCA_CONFIG_PROFILES", envir = .GlobalEnv)) {
    return(character())
  }

  names(get("RGCCA_CONFIG_PROFILES", envir = .GlobalEnv))
}

get_config <- function(profile = Sys.getenv("RGCCA_PROFILE", "macbook")) {
  profile <- rgcca_first_non_empty(profile, "macbook")

  if (!exists("RGCCA_CONFIG_PROFILES", envir = .GlobalEnv)) {
    stop("RGCCA_CONFIG_PROFILES is not defined. Source config.R first.", call. = FALSE)
  }

  profiles <- get("RGCCA_CONFIG_PROFILES", envir = .GlobalEnv)
  if (!profile %in% names(profiles)) {
    stop(
      paste0(
        "Unknown RGCCA profile: ", profile, "\n",
        "Available profiles: ", paste(names(profiles), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  cfg <- profiles[[profile]]
  cfg$RGCCA_PROFILE <- profile
  cfg <- rgcca_add_derived_paths(cfg)
  cfg <- rgcca_apply_overrides(cfg)

  cfg
}

print_config <- function(cfg = get_config()) {
  width <- max(nchar(names(cfg)))
  for (name in names(cfg)) {
    cat(sprintf("%-*s = %s\n", width, name, rgcca_as_scalar_character(cfg[[name]])))
  }
  invisible(cfg)
}

write_env <- function(cfg = get_config(),
                      env_file = file.path(cfg$PATH_REPO, ".env")) {
  env_values <- vapply(cfg, rgcca_as_scalar_character, character(1))
  env_lines <- paste(names(env_values), shQuote(env_values, type = "sh"), sep = "=")

  dir.create(dirname(env_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(env_lines, con = env_file)

  invisible(env_file)
}


# CLI ----

rgcca_config_usage <- function() {
  cat(
    "Usage:\n",
    "  Rscript config.R --profile <profile> --print\n",
    "  Rscript config.R --profile <profile> --write-env [--env-file <path>]\n",
    "\n",
    "Profiles:\n",
    paste0("  - ", available_profiles(), collapse = "\n"),
    "\n",
    sep = ""
  )
}

rgcca_parse_config_cli <- function(args) {
  out <- list(
    profile = Sys.getenv("RGCCA_PROFILE", "macbook"),
    print = FALSE,
    write_env = FALSE,
    env_file = NULL,
    help = FALSE
  )

  i <- 1
  while (i <= length(args)) {
    arg <- args[i]

    if (arg %in% c("-h", "--help")) {
      out$help <- TRUE
    } else if (arg == "--print") {
      out$print <- TRUE
    } else if (arg == "--write-env") {
      out$write_env <- TRUE
    } else if (arg == "--profile") {
      i <- i + 1
      if (i > length(args)) stop("--profile requires a value", call. = FALSE)
      out$profile <- args[i]
    } else if (grepl("^--profile=", arg)) {
      out$profile <- sub("^--profile=", "", arg)
    } else if (arg == "--env-file") {
      i <- i + 1
      if (i > length(args)) stop("--env-file requires a value", call. = FALSE)
      out$env_file <- args[i]
    } else if (grepl("^--env-file=", arg)) {
      out$env_file <- sub("^--env-file=", "", arg)
    } else {
      stop(paste("Unknown argument:", arg), call. = FALSE)
    }

    i <- i + 1
  }

  out
}

rgcca_config_is_cli <- function(config_file = "config.R") {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) == 0) return(FALSE)

  cli_file <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)
  expected_file <- normalizePath(config_file, mustWork = FALSE)

  identical(cli_file, expected_file)
}

rgcca_config_main <- function() {
  args <- rgcca_parse_config_cli(commandArgs(trailingOnly = TRUE))

  if (args$help) {
    rgcca_config_usage()
    return(invisible(NULL))
  }

  cfg <- get_config(args$profile)

  if (!args$print && !args$write_env) {
    rgcca_config_usage()
    return(invisible(NULL))
  }

  if (args$print) {
    print_config(cfg)
  }

  if (args$write_env) {
    env_file <- rgcca_first_non_empty(args$env_file, file.path(cfg$PATH_REPO, ".env"))
    write_env(cfg, env_file)
    cat("Wrote environment file:", env_file, "\n")
  }

  invisible(cfg)
}
