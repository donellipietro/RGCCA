# = ========================================================================== =
# - Script: directories.R
# - Desc: Utility functions for managing directory structures used
#         throughout the testing framework.
# = ========================================================================== =


## Function: mkdir
# - Args:
#   * paths: character vector of directory paths to create
# - Desc:
#   Checks whether each directory in 'paths' exists.
#   If it does not, the function creates it.
mkdir <- function(paths) {
  for (path in paths) {
    if (!file.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
  }
}


## Function: load_config
# - Desc:
#   Loads the root configuration file if needed and returns the selected profile.
load_config <- function() {
  if (!exists("get_config", mode = "function")) {
    source("config.R")
  }

  get_config()
}


## Function: config_path
# - Desc:
#   Joins path components and keeps the trailing slash expected by older code.
config_path <- function(...) {
  path <- file.path(...)
  needs_slash <- !grepl("[/\\\\]$", path)
  path[needs_slash] <- paste0(path[needs_slash], .Platform$file.sep)
  path
}


## Function: create_paths
# - Args:
#   * test_suite: string, name of the test suite (used to create subdirectories)
# - Desc:
#   Creates all necessary directory structures for results, images,
#   queues, logs, and C++ data/mesh paths. Returns a named list
#   containing all created paths for downstream use.
create_paths <- function(test_suite) {
  cfg <- load_config()

  ## Directories for results
  mkdir(c(cfg$PATH_RESULTS, cfg$PATH_IMAGES, cfg$PATH_TEST_DATA))
  path_results <- config_path(cfg$PATH_RESULTS, test_suite)
  path_images <- config_path(cfg$PATH_IMAGES, test_suite)
  path_data <- config_path(cfg$PATH_TEST_DATA, test_suite)
  mkdir(c(path_results, path_images, path_data))

  ## Temporary directories
  mkdir(c(
    cfg$PATH_TMP,
    cfg$PATH_QUEUE,
    cfg$PATH_LOGS,
    cfg$PATH_TMP_DATA,
    cfg$PATH_TMP_RESULTS,
    cfg$PATH_BUILD
  ))
  path_queue <- config_path(cfg$PATH_QUEUE, test_suite)
  path_logs <- config_path(cfg$PATH_LOGS, test_suite)
  path_tmp_data <- config_path(cfg$PATH_TMP_DATA, test_suite)
  path_tmp_results <- config_path(cfg$PATH_TMP_RESULTS, test_suite)
  mkdir(c(path_queue, path_logs, path_tmp_data, path_tmp_results))

  ## Save all paths needed by the methods in a list
  path_list <- list(
    repo = config_path(cfg$PATH_REPO),
    cpp = config_path(cfg$PATH_CPP),
    build = config_path(cfg$PATH_BUILD),
    results = path_results,
    images = path_images,
    data = path_data,
    queue = path_queue,
    logs = path_logs,
    tmp_data = path_tmp_data,
    tmp_results = path_tmp_results
  )

  return(path_list)
}


## Function: update_paths
# - Args:
#   * path_list: list of paths as returned by create_paths()
#   * name_main_test: string, name of the main test (used for subdirectories)
#   * test_options: list containing the desired test options
# - Desc:
#   Updates the path list to include directories specific to a given
#   test. Creates result and image subdirectories accordingly and
#   returns the updated path list.
update_paths <- function(path_list, name_main_test, test_options) {
  name_test <- test_options$name_test
  if (is.null(name_test) || length(name_test) == 0 || !nzchar(name_test[1])) {
    name_test <- name_main_test
  }

  for (ext in c("images", "data")) {
    path <- path_list[[ext]]
    path <- config_path(path, name_main_test)
    mkdir(path)
    path_list[[ext]] <- path
  }

  for (ext in c("results", "tmp_data", "tmp_results")) {
    path <- path_list[[ext]]
    path <- config_path(path, name_main_test)
    mkdir(path)
    path <- config_path(path, name_test)
    mkdir(path)
    path_list[[ext]] <- path
  }

  ## Add cpp_scripts path to path_list
  path_list$cpp_script_source <- config_path(path_list$cpp, test_options$cpp_script)
  path_list$cpp_script <- config_path(path_list$build, test_options$cpp_script)

  ## Check if the C++ has been compiled
  compiled_files <- list.files(
    path = path_list$cpp_script,
    pattern = "^fit_model",   # regex: starts with "fit_model"
    full.names = TRUE
  )
  if (length(compiled_files) == 0) {
    stop(
      paste0(
        "The C++ model has not been compiled!\n",
        "Run: make compile MODEL=", test_options$cpp_script
      )
    )
  }

  return(path_list)
}


# - Function: get_script_path
# - Desc:
#   Determines and returns the absolute path of the currently running R script.
#   The function supports execution in different contexts:
#     1. When run via `Rscript`, it parses `--file=` arguments.
#     2. When sourced using `source()`, it reads from `sys.frames()`.
#     3. When executed inside RStudio, it queries the active editor via `rstudioapi`.
#   If none of these methods succeed, it returns `NULL`.
get_script_path <- function() {
  # Try Rscript
  args <- commandArgs(trailingOnly = FALSE)
  path <- sub("--file=", "", args[grep("--file=", args)])
  if (length(path) > 0) {
    return(paste0(dirname(normalizePath(path)), "/"))
  }

  # Try source()
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(paste0(dirname(normalizePath(sys.frames()[[1]]$ofile)), "/"))
  }

  # Try RStudio
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
    return(paste0(dirname(normalizePath(rstudioapi::getSourceEditorContext()$path)), "/"))
  }

  # Fallback
  return(NULL)
}

## Function: open
# - Args:
#   * path: string, file or directory path to open
# - Desc:
#   Opens the specified file or directory using the system’s default application.
#   On Windows, it calls `shell.exec`; on Unix-based systems (macOS, Linux),
#   it uses the `open` command through the system shell.
open <- function(path) {
  if (.Platform$OS.type == "windows") {
    shell.exec(path)
  } else {
    system(paste("open", path))
  }
}
open <- function(path) {
  if (.Platform$OS.type == "windows") {
    shell.exec(path)
  } else {
    system(paste("open", path))
  }
}
