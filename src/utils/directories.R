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
      dir.create(path)
    }
  }
}


## Function: create_paths
# - Args:
#   * test_suite: string, name of the test suite (used to create subdirectories)
# - Desc:
#   Creates all necessary directory structures for results, images,
#   queues, logs, and C++ data/mesh paths. Returns a named list
#   containing all created paths for downstream use.
create_paths <- function(test_suite) {
  ## Directories for results
  mkdir(c("results", "images", "data/tests"))
  path_results <- paste("results/", test_suite, "/", sep = "")
  path_images <- paste("images/", test_suite, "/", sep = "")
  path_data <- paste("data/tests/", test_suite, "/", sep = "")
  mkdir(c(path_results, path_images, path_data))

  ## Temporary directories
  mkdir(c("tmp/", "tmp/queue/", "tmp/logs/", "tmp/data/", "tmp/results/"))
  path_queue <- paste("tmp/queue/", test_suite, "/", sep = "")
  path_logs <- paste("tmp/logs/", test_suite, "/", sep = "")
  path_tmp_data <- paste("tmp/data/", test_suite, "/", sep = "")
  path_tmp_results <- paste("tmp/results/", test_suite, "/", sep = "")
  mkdir(c(path_queue, path_logs, path_tmp_data, path_tmp_results))

  ## Save all paths needed by the methods in a list
  path_list <- list(
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
  for (ext in c("images", "data")) {
    path <- path_list[[ext]]
    path <- paste(path, name_main_test, "/", sep = "")
    mkdir(path)
    path_list[[ext]] <- path
  }

  for (ext in c("results", "tmp_data", "tmp_results")) {
    path <- path_list[[ext]]
    path <- paste(path, name_main_test, "/", sep = "")
    mkdir(path)
    path <- paste(path, test_options$name_test, "/", sep = "")
    mkdir(path)
    path_list[[ext]] <- path
  }

  ## Add cpp_scripts path to path_list
  path_list$cpp_script <- paste0("cpp/", test_options$cpp_script, "/")

  ## Check if the C++ has been compiled
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
        "Run: make compile MODEL=", basename(path_list$cpp_script)
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
