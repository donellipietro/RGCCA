
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

## C++ output
IGNORE_CPP_OUTPUT = TRUE
IGNORE_R_OUTPUT = TRUE

## Defaults
name_main_test_default <- "testResampling"
order <- c(1,3,2) # Boxplot grouping | Rows | Cols
