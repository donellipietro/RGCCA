
## Global variables ----

## Test suite full name and acronym
# - TEST_SUITE is for printing only
# - test_suite will be used to create directories (no spaces, please)
TEST_SUITE <- "RGCCA - 2D"
test_suite <- "RGCCA-2D"

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
name_main_test_default <- "testResampling"
test_groups <- list(
  testSensitivity = c("testSensitivitySingleThread", "testSensitivityMultiThread")
)
order <- c(1,3,2) # Boxplot grouping | Rows | Cols
