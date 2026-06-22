# = ========================================================================== =
# - Script: test_groups.R
# - Desc: Helpers for grouped tests and per-test threading metadata.
# = ========================================================================== =


load_test_suite_config_env <- function(test_suite) {
  config_file <- file.path("tests", test_suite, "config.R")
  env <- new.env(parent = globalenv())
  if (file.exists(config_file)) {
    source(config_file, local = env)
  }
  env
}


if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
  }
}


resolve_test_names <- function(test_suite, name_main_test) {
  env <- load_test_suite_config_env(test_suite)
  test_groups <- if (exists("test_groups", envir = env, inherits = FALSE)) {
    get("test_groups", envir = env)
  } else {
    list()
  }

  resolved <- unlist(lapply(name_main_test, function(test_name) {
    group <- test_groups[[test_name]]
    if (is.null(group)) test_name else group
  }), use.names = FALSE)

  unique(as.character(resolved))
}


test_threading_mode <- function(test_options, default = "single") {
  mode <- NULL
  if (!is.null(test_options$test_options)) {
    mode <- test_options$test_options$threading %||% test_options$test_options$thread_mode
  }
  mode <- mode %||% default
  mode <- tolower(as.character(mode[1]))
  if (!mode %in% c("single", "multi")) {
    stop("test_options$threading must be either 'single' or 'multi'.", call. = FALSE)
  }
  mode
}


queue_threading_mode <- function(path_queue, default = "single") {
  option_files <- sort(list.files(path_queue, pattern = "\\.json$", full.names = TRUE))
  if (length(option_files) == 0) return(default)

  modes <- vapply(option_files, function(file_options) {
    test_options <- jsonlite::fromJSON(file_options)
    test_threading_mode(test_options, default = default)
  }, character(1))

  modes <- unique(modes)
  if (length(modes) != 1) {
    stop(
      paste0(
        "Queue contains mixed threading modes: ",
        paste(modes, collapse = ", "),
        ". Split these options into separate tests."
      ),
      call. = FALSE
    )
  }

  modes
}
