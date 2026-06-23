
## Global variables ----

## Test suite full name and acronym
# - TEST_SUITE is for printing only
# - test_suite will be used to create directories (no spaces, please)
TEST_SUITE <- "RGCCA Independent Sampling - 1D"
test_suite <- "RGCCA-IS-1D"

## Force fit/evaluation even if a fit is already available
FORCE_FIT <- FALSE
FORCE_EVALUATE <- FALSE

## Execution flow modifiers
RUN <- list()
RUN$tests <- TRUE
SMOKE_TEST <- FALSE
SMOKE_TEST <- isTRUE(SMOKE_TEST) ||
  tolower(Sys.getenv("SMOKE_TEST", "false")) %in% c("1", "true", "yes", "y")

## C++ output
IGNORE_CPP_OUTPUT = TRUE
IGNORE_R_OUTPUT = TRUE

## Defaults
name_main_test_default <- "testSensitivity"
test_groups <- list(
  testSensitivity = c("testSensitivitySingleThread", "testSensitivityMultiThread"),
  all = c(
    "testMultivariate",
    "testSensitivitySingleThread",
    "testSensitivityMultiThread",
    "testBootstrap"
  )
)
order <- c(1,2,3) # Boxplot grouping | Rows | Cols
