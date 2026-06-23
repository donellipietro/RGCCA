# = ========================================================================== =
# - Test: RGCCA - Naive - Aggregate results
# - Desc: Loads quantitative results for all options of a selected test and
#         returns them as a structured list, then aggregates the data in plots
# - Args:
#     [1] name_main_test : name of the main test to scan (e.g., "test1")
# = ========================================================================== =

rm(list = ls())
graphics.off()
options(warn = -1)

width <- 13
height <- 15

# README ----
# Assumes all batches for each option have already been run and saved.
# For each option JSON in the queue/<test>/, this script loads the corresponding
# quantitative results (via load_quantitative_results) and collects them.

# Configuration ----

## Load libraries ----
invisible(suppressMessages(sapply(c(
  # discretization
  "fdaPDE", "femR",
  # algebraic utils
  "pracma",
  # data manipulation
  "MASS", "tidyr", "dplyr",
  # keep plotting deps (not used here) to avoid missing imports in utils
  "ggplot2", "viridis", "stringr", "RColorBrewer", "grid", "gridExtra",
  # json
  "jsonlite",
  # sampling
  "sf", "sp", "raster"
), require, character.only = TRUE)))

## Load functions ----
source("src/utils/cat.R")
source("src/utils/directories.R")
source("src/utils/test_groups.R")
source("src/utils/options.R")
source("src/utils/load_results_utils.R")
source("src/utils/plotting_utils.R")

## Load configuration file
path_this <- get_script_path()
source(paste0(path_this, "config.R"))

## Load test-specific helpers
source(paste0("tests/", test_suite, "/utils/generate_options.R"))

## Create suite directories ----
path_list <- create_paths(test_suite)

# Loader ----

## Generate options ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  INTERACTIVE <- TRUE
  requested_main_tests <- name_main_test_default
} else {
  INTERACTIVE <- FALSE
  requested_main_tests <- args
}

name_main_tests <- resolve_test_names(test_suite, requested_main_tests)
name_output_test <- if (length(requested_main_tests) == 1) {
  requested_main_tests[1]
} else {
  paste(requested_main_tests, collapse = "_")
}

loaded_results <- NULL
expected_varying_options <- NULL

for (name_main_test in name_main_tests) {
  path_list_i <- path_list

  ## Prepare queue/log dirs for this test
  path_list_i$queue <- paste0(path_list_i$queue, name_main_test, "/")
  path_list_i$logs <- paste0(path_list_i$logs, name_main_test, "/")
  mkdir(c(path_list_i$queue, path_list_i$logs))

  ## Generate all option files for this test
  generate_options(test_suite, name_main_test, path_list_i$queue)

  ## Load results (all option combinations, all batches)
  loaded_results_i <- load_all_quantitative_results(path_list_i, name_main_test)

  if (is.null(expected_varying_options)) {
    expected_varying_options <- loaded_results_i$varying_options
  } else if (!identical(expected_varying_options, loaded_results_i$varying_options)) {
    stop(
      paste0(
        "Cannot aggregate tests with different varying_options: ",
        paste(name_main_tests, collapse = ", ")
      )
    )
  }

  loaded_results <- if (is.null(loaded_results)) {
    loaded_results_i
  } else {
    accumulate_results_struct(loaded_results, loaded_results_i)
  }
}


# Plot aggregated results ----

## Create the path for images
path_list$images <- paste0(path_list$images, name_output_test, "/")
mkdir(path_list$images)

## Define an order for the varying options
# loaded_results$varying_options
if (is.null(order) || length(order) != length(loaded_results$varying_options)) {
  order <- 1:length(loaded_results$varying_options) # default
}

finite_values <- function(data_plot) {
  if (is.null(data_plot)) {
    return(numeric(0))
  }
  values <- suppressWarnings(as.numeric(unlist(data_plot[loaded_results$model_names])))
  values[is.finite(values)]
}

metric_limits <- function(data_plot, lower = 0) {
  values <- finite_values(data_plot)
  if (length(values) == 0) {
    return(NULL)
  }
  c(lower, max(values, na.rm = TRUE))
}

plot_metric <- function(data_plot, title_prefix, values_name, limits = NULL,
                        plots_catalog = NULL) {
  if (length(finite_values(data_plot)) == 0) {
    return(invisible(FALSE))
  }
  plot.aggregated_data(
    loaded_results, data_plot, title_prefix, values_name,
    order = order, limits = limits, plots_catalog = plots_catalog
  )
  invisible(TRUE)
}

### Time complexity ----

## Set plots parameters
data_plot <- loaded_results$execution_time
title_prefix <- "Execution times w.r.t the"
values_name <- "Time [seconds]"
limits <- c(0, max(data_plot[loaded_results$model_names], na.rm = TRUE))

plots_catalog <- list(
  boxplots = TRUE,
  lines = FALSE,
  logx = FALSE,
  loglog = FALSE,
  normalized = FALSE
)


## Open a pdf where to save the plots
pdf(paste0(path_list$images, "time_complexity.pdf"), width = width, height = height)
plot.aggregated_data(
  loaded_results, data_plot, title_prefix, values_name,
  order = order, limits = limits, plots_catalog = plots_catalog
)
dev.off()


### Objective ----

## Open a pdf where to save the plots
pdf(paste0(path_list$images, "objective.pdf"), width = width, height = height)

#### Objective ----
data_plot <- loaded_results$objective
data_plot[, -c(1, 2, 3)] <- log10(data_plot[, -c(1, 2, 3)] + 0.1)
title_prefix <- paste0("log10(Objective) w.r.t")
values_name <- "log10(Obj)"
limits <- range(data_plot[loaded_results$model_names], na.rm = TRUE)

## Plot aggregated results
plot.aggregated_data(
  loaded_results, data_plot, title_prefix, values_name,
  order = order, limits = limits
)
dev.off()

### Diagnostics ----

if (!is.null(loaded_results$diagnostics)) {
  pdf(paste0(path_list$images, "diagnostics.pdf"), width = width, height = height)

  diagnostic_plots <- list(
    iterations = list(
      title = "Iterations w.r.t",
      values = "Iterations"
    ),
    bootstrap_resamples = list(
      title = "Effective bootstrap resamples w.r.t",
      values = "Resamples"
    )
  )

  for (metric_name in names(diagnostic_plots)) {
    data_plot <- loaded_results$diagnostics[[metric_name]]
    plot_metric(
      data_plot,
      diagnostic_plots[[metric_name]]$title,
      diagnostic_plots[[metric_name]]$values,
      metric_limits(data_plot)
    )
  }

  dev.off()
}

### Model selection ----

if (!is.null(loaded_results$model_selection)) {
  pdf(paste0(path_list$images, "model_selection.pdf"), width = width, height = height)

  plots_catalog_logy <- list(
    boxplots = TRUE,
    lines = FALSE,
    logx = FALSE,
    logy = FALSE,
    loglog = FALSE,
    normalized = FALSE,
    boxplot_logy = TRUE
  )

  data_plot <- loaded_results$model_selection$lambda_weights
  data_plot[loaded_results$model_names] <- lapply(
    data_plot[loaded_results$model_names],
    function(x) ifelse(is.na(x) | x <= 0, NaN, x)
  )
  if (any(is.finite(as.matrix(data_plot[loaded_results$model_names])))) {
    title_prefix <- "Optimal lambda w.r.t"
    values_name <- "Lambda"
    plot.aggregated_data(
      loaded_results, data_plot, title_prefix, values_name,
      order = order, limits = NULL, plots_catalog = plots_catalog_logy
    )
  }

  model_selection_plots <- list(
    active_blocks_accuracy = "Active-block accuracy",
    active_connections_accuracy = "Active-connection accuracy"
  )

  for (metric_name in names(model_selection_plots)) {
    data_plot <- loaded_results$model_selection[[metric_name]]
    title_prefix <- paste0(model_selection_plots[[metric_name]], " w.r.t")
    values_name <- model_selection_plots[[metric_name]]
    limits <- if (grepl("accuracy", metric_name)) c(0, 1) else NULL
    plot.aggregated_data(
      loaded_results, data_plot, title_prefix, values_name,
      order = order, limits = limits
    )
  }

  dev.off()
}


### RMSE ----


# #### RMSE[X] at locations ----
#
# ## Set plots parameters
data_plot <- loaded_results$rmse$X
# title_prefix <- "RMSE[X] at locations w.r.t the"
# values_name <- "RMSE"
# limits <- c(0, max(data_plot[loaded_results$model_names[1:2]]))
#
# ## Plot aggregated results
# plot.aggregated_data(
#   loaded_results, data_plot, title_prefix, values_name,
#   order = order, limits = limits
# )

## Set plots parameters
n_groups <- 4


for (g in 1:n_groups) {
  ## Open a pdf where to save the plots
  pdf(paste0(path_list$images, "rmse_g", g, ".pdf"), width = width, height = height)

  #### Cumulative RMSE[A] at locations ----
  data_plot <- loaded_results$rmse[[paste0("A", g, "_locs_all")]]
  title_prefix <- paste0("Cumulative RMSE[A", g, "] at locations w.r.t")
  values_name <- "RMSE"
  limits <- c(0, max(data_plot[loaded_results$model_names], na.rm = TRUE))

  ## Plot aggregated results
  plot.aggregated_data(
    loaded_results, data_plot, title_prefix, values_name,
    order = order, limits = limits
  )

  #### RMSE[A] at locations ----
  data_plot <- loaded_results$rmse[[paste0("A", g, "_locs")]]
  title_prefix <- paste0("RMSE[A", g, "] at locations w.r.t")
  values_name <- "RMSE"
  limits <- c(0, max(data_plot[loaded_results$model_names], na.rm = TRUE))

  ## Plot aggregated results
  plot.aggregated_data(
    loaded_results, data_plot, title_prefix, values_name,
    order = order, limits = limits
  )

  #### Cumulative RMSE[A_star] at locations ----
  data_plot <- loaded_results$rmse[[paste0("A_star", g, "_locs_all")]]
  title_prefix <- paste0("Cumulative RMSE[A_star", g, "] at locations w.r.t")
  values_name <- "RMSE"
  limits <- c(0, max(data_plot[loaded_results$model_names], na.rm = TRUE))

  ## Plot aggregated results
  plot.aggregated_data(
    loaded_results, data_plot, title_prefix, values_name,
    order = order, limits = limits
  )

  #### RMSE[A_star] at locations ----
  data_plot <- loaded_results$rmse[[paste0("A_star", g, "_locs")]]
  title_prefix <- paste0("RMSE[A_star", g, "] at locations w.r.t")
  values_name <- "RMSE"
  limits <- c(0, max(data_plot[loaded_results$model_names], na.rm = TRUE))

  ## Plot aggregated results
  plot.aggregated_data(
    loaded_results, data_plot, title_prefix, values_name,
    order = order, limits = limits
  )

  #### Cumulative RMSE[H] ----
  data_plot <- loaded_results$rmse[[paste0("H", g, "_all")]]
  title_prefix <- paste0("Cumulative RMSE[H", g, "] w.r.t")
  values_name <- "RMSE"
  limits <- c(0, max(data_plot[loaded_results$model_names], na.rm = TRUE))

  ## Plot aggregated results
  plot.aggregated_data(
    loaded_results, data_plot, title_prefix, values_name,
    order = order, limits = limits
  )

  #### RMSE[H] at locations ----
  data_plot <- loaded_results$rmse[[paste0("H", g)]]
  title_prefix <- paste0("RMSE[H", g, "] w.r.t")
  values_name <- "RMSE"
  limits <- c(0, max(data_plot[loaded_results$model_names], na.rm = TRUE))

  ## Plot aggregated results
  plot.aggregated_data(
    loaded_results, data_plot, title_prefix, values_name,
    order = order, limits = limits
  )

  dev.off()
}


## Open the results directory
open(path_list$images)
