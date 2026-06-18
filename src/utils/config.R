# = ========================================================================== =
# - Script: src/utils/config.R
# - Desc: Helpers for reading repository profiles and exporting shell env files.
# = ========================================================================== =


# Helpers ----

first_non_empty <- function(...) {
  values <- list(...)
  for (value in values) {
    if (!is.null(value) && length(value) > 0 && !is.na(value[1]) &&
        nzchar(as.character(value[1]))) {
      return(as.character(value[1]))
    }
  }
  ""
}

config_env <- function(name, default = "") {
  first_non_empty(Sys.getenv(paste0("TESTBENCH_", name), unset = ""), default)
}

config_value <- function(cfg, name, default = "") {
  value <- if (!is.null(cfg[[name]])) cfg[[name]] else default
  config_env(name, value)
}

as_scalar_character <- function(value) {
  if (is.null(value) || length(value) == 0) return("")
  paste(as.character(value), collapse = ",")
}

add_derived_paths <- function(cfg) {
  cfg$PATH_REPO <- config_value(cfg, "PATH_REPO")
  cfg$PATH_TMP <- config_value(cfg, "PATH_TMP", file.path(cfg$PATH_REPO, "tmp"))

  cfg$PATH_SRC <- config_value(cfg, "PATH_SRC", file.path(cfg$PATH_REPO, "src"))
  cfg$PATH_TESTS <- config_value(cfg, "PATH_TESTS", file.path(cfg$PATH_REPO, "tests"))
  cfg$PATH_CPP <- config_value(cfg, "PATH_CPP", file.path(cfg$PATH_REPO, "cpp"))
  cfg$PATH_DATA <- config_value(cfg, "PATH_DATA", file.path(cfg$PATH_REPO, "data"))

  cfg$PATH_RESULTS <- config_value(cfg, "PATH_RESULTS", file.path(cfg$PATH_REPO, "results"))
  cfg$PATH_IMAGES <- config_value(cfg, "PATH_IMAGES", file.path(cfg$PATH_REPO, "images"))
  cfg$PATH_TEST_DATA <- config_value(cfg, "PATH_TEST_DATA", file.path(cfg$PATH_REPO, "data/tests"))
  cfg$PATH_QUEUE <- config_value(cfg, "PATH_QUEUE", file.path(cfg$PATH_TMP, "queue"))
  cfg$PATH_LOGS <- config_value(cfg, "PATH_LOGS", file.path(cfg$PATH_TMP, "logs"))
  cfg$PATH_TMP_DATA <- config_value(cfg, "PATH_TMP_DATA", file.path(cfg$PATH_TMP, "data"))
  cfg$PATH_TMP_RESULTS <- config_value(cfg, "PATH_TMP_RESULTS", file.path(cfg$PATH_TMP, "results"))
  cfg$PATH_BUILD <- config_value(cfg, "PATH_BUILD", file.path(cfg$PATH_REPO, "build"))

  cfg
}

apply_overrides <- function(cfg) {
  for (name in names(cfg)) {
    cfg[[name]] <- config_env(name, cfg[[name]])
  }
  cfg
}


# Public API ----

available_profiles <- function() {
  if (!exists("TESTBENCH_CONFIG_PROFILES", envir = .GlobalEnv)) {
    return(character())
  }

  names(get("TESTBENCH_CONFIG_PROFILES", envir = .GlobalEnv))
}

get_config <- function(profile = Sys.getenv("TESTBENCH_PROFILE", "macbook")) {
  profile <- first_non_empty(profile, "macbook")

  if (!exists("TESTBENCH_CONFIG_PROFILES", envir = .GlobalEnv)) {
    stop("TESTBENCH_CONFIG_PROFILES is not defined. Source config.R first.", call. = FALSE)
  }

  profiles <- get("TESTBENCH_CONFIG_PROFILES", envir = .GlobalEnv)
  if (!profile %in% names(profiles)) {
    stop(
      paste0(
        "Unknown testbench profile: ", profile, "\n",
        "Available profiles: ", paste(names(profiles), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  cfg <- profiles[[profile]]
  cfg$TESTBENCH_PROFILE <- profile
  cfg <- add_derived_paths(cfg)
  cfg <- apply_overrides(cfg)

  cfg
}

print_config <- function(cfg = get_config()) {
  width <- max(nchar(names(cfg)))
  for (name in names(cfg)) {
    cat(sprintf("%-*s = %s\n", width, name, as_scalar_character(cfg[[name]])))
  }
  invisible(cfg)
}

write_env <- function(cfg = get_config(),
                      env_file = file.path(cfg$PATH_REPO, ".env")) {
  env_values <- vapply(cfg, as_scalar_character, character(1))
  env_lines <- paste(names(env_values), shQuote(env_values, type = "sh"), sep = "=")

  dir.create(dirname(env_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(env_lines, con = env_file)

  invisible(env_file)
}


# CLI ----

config_usage <- function() {
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

parse_config_cli <- function(args) {
  out <- list(
    profile = Sys.getenv("TESTBENCH_PROFILE", "macbook"),
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

config_is_cli <- function(config_file = "config.R") {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) == 0) return(FALSE)

  cli_file <- normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)
  expected_file <- normalizePath(config_file, mustWork = FALSE)

  identical(cli_file, expected_file)
}

config_main <- function() {
  args <- parse_config_cli(commandArgs(trailingOnly = TRUE))

  if (args$help) {
    config_usage()
    return(invisible(NULL))
  }

  cfg <- get_config(args$profile)

  if (!args$print && !args$write_env) {
    config_usage()
    return(invisible(NULL))
  }

  if (args$print) {
    print_config(cfg)
  }

  if (args$write_env) {
    env_file <- first_non_empty(args$env_file, file.path(cfg$PATH_REPO, ".env"))
    write_env(cfg, env_file)
    cat("Wrote environment file:", env_file, "\n")
  }

  invisible(cfg)
}
