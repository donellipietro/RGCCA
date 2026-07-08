rm(list = ls())
graphics.off()
options(warn = -1)

invisible(suppressMessages(sapply(c(
  "MASS", "clue", "jsonlite", "raster"
), require, character.only = TRUE)))

source("src/utils/cat.R")
source("src/utils/directories.R")
source("src/utils/options.R")
source("src/utils/mesh_utils.R")
source("src/utils/domain_utils.R")
source("src/utils/error_metrics.R")
source("src/utils/load_results_utils.R")

path_this <- get_script_path()
source(paste0(path_this, "config.R"))

source(paste0("tests/", test_suite, "/utils/wrappers.R"))
source(paste0("tests/", test_suite, "/utils/generate_data.R"))

n_comp <- 2
base_options <- list(
  cpp_script = "RGCCA",
  test_options = list(
    n_reps = 1
  ),
  domain_and_locations = list(
    name_mesh = "unit_interval",
    locs_eq_nodes = FALSE
  ),
  dimensions = list(
    n_groups = 4,
    n_nodes_D = c(101),
    n_nodes_HR_grid_D = 200,
    n = 1200,
    n_locs = 101
  ),
  model_options = rgcca_model_options(
    n_comp = n_comp,
    init_strategy = "Uniform",
    lambda_selection_weights = TRUE,
    block_deactivation = TRUE,
    connection_deactivation = TRUE,
    block_importance = TRUE,
    component_significance = TRUE,
    inactive_block_signal_test = TRUE
  ),
  bootstrap_options = rgcca_bootstrap_options(
    active_block_tol = 0.1,
    inactive_block_signal_resamples = 100,
    inactive_block_signal_alpha = 0.05
  ),
  noise = list(
    sigma_noise = 5
  ),
  regularization = list(
    lambda = -1,
    lambda_grid = 10^(-9:-2)
  )
)

active_blocks_label <- function(active_blocks) {
  paste(vapply(active_blocks, function(x) paste(which(x), collapse = ","), character(1)), collapse = " | ")
}

run_profile <- function(version) {
  test_options <- base_options
  test_options$name_test <- paste0("residual_signal_gate_", version)

  data <- generate_data_residual_signal_gate(test_options, seed = 0, fourth_component_version = version)
  expected <- if (version == "deactivation") list(c(TRUE, FALSE, FALSE, TRUE), c(TRUE, FALSE, FALSE, TRUE)) else
    list(c(TRUE, FALSE, FALSE, TRUE), c(TRUE, TRUE, FALSE, FALSE))
  stopifnot(identical(data$model_selection_truth$active_blocks, expected))

  path_list <- update_paths(create_paths("prova"), "residual_signal_gate", test_options)
  results <- fit_model("CPP_fGCCA_NN_cov_FEM", data, path_list, test_options)

  cat(paste0("\n", version, "\n"))
  cat(paste0("truth:    ", active_blocks_label(data$model_selection_truth$active_blocks), "\n"))
  cat(paste0("selected: ", active_blocks_label(results$model_selection$active_blocks), "\n"))
  invisible(results)
}

IGNORE_CPP_OUTPUT <- FALSE
results <- lapply(c("deactivation", "reactivation"), run_profile)
