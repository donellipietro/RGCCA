# = ========================================================================== =
# - Script: init.R
# - Desc: Initializes a test run by parsing CLI arguments, preparing
#         directories, and generating JSON option files for the selected test.
# = ========================================================================== =

# Libraries ----
suppressMessages(library(jsonlite))
suppressMessages(library(RColorBrewer))

# Sources ----
source("src/utils/options.R")
source("src/utils/directories.R")

# Select test ----

## Read arguments passed from the terminal
args <- commandArgs(trailingOnly = TRUE)

## Parse the arguments, if any
if (length(args) == 0) {
  ## Defaults
  test_suite     <- "example_data_decomposition"
  name_main_test <- "test1"
} else {
  ## Set the requested configuration
  test_suite     <- args[1]
  name_main_test <- args[2]
}

## Print selected test info
cat("\n")
cat(paste("Test suite:", test_suite, "\n"))
cat(paste("Test name:", name_main_test, "\n"))
cat("\n")

path_test_suite <- file.path("tests", test_suite)
path_generate_options <- file.path(path_test_suite, "utils", "generate_options.R")
if (!dir.exists(path_test_suite) || !file.exists(path_generate_options)) {
  available_suites <- basename(list.dirs("tests", recursive = FALSE, full.names = TRUE))
  stop(
    paste0(
      "Unknown or incomplete test suite: ", test_suite, "\n",
      "Expected option generator: ", path_generate_options, "\n",
      "Available test suites: ", paste(sort(available_suites), collapse = ", ")
    ),
    call. = FALSE
  )
}

# Generate options ----

## Update directories according to the selected test
cfg <- load_config()
mkdir(c(cfg$PATH_TMP, cfg$PATH_QUEUE))
path_queue <- config_path(cfg$PATH_QUEUE, test_suite)
mkdir(path_queue)
path_queue <- config_path(path_queue, name_main_test)
mkdir(path_queue)

## Load the option-generation function
source(path_generate_options)

## Generate all the options for the selected test
generate_options(test_suite, name_main_test, path_queue)
