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

# Generate options ----

## Update directories according to the selected test
mkdir(c("tmp/", "tmp/queue/"))
path_queue <- paste0("tmp/queue/", test_suite, "/")
mkdir(path_queue)
path_queue <- paste0(path_queue, name_main_test, "/")
mkdir(path_queue)

## Load the option-generation function
source(paste("tests/", test_suite, "/utils/generate_options.R", sep = ""))

## Generate all the options for the selected test
generate_options(test_suite, name_main_test, path_queue)