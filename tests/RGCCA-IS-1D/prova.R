rm(list = ls())
graphics.off()
options(warn = -1)

invisible(suppressMessages(sapply(c(
  ## Competitors
  "RGCCA",
  # discretization
  "fdaPDE", "femR",
  # algebraic utils
  "pracma", "clue",
  # data manipulation
  "MASS", "tidyr", "dplyr",
  # visualization
  "ggplot2", "viridis", "stringr", "RColorBrewer", "grid", "gridExtra",
  # json
  "jsonlite",
  # sampling
  "sf", "sp", "raster"
), require, character.only = TRUE)))

## Load general utility functions
source("src/utils/cat.R")
source("src/utils/directories.R")
source("src/utils/options.R")
source("src/utils/mesh_utils.R")
source("src/utils/domain_utils.R")
source("src/utils/plotting_utils.R")
source("src/utils/error_metrics.R")
source("src/utils/load_results_utils.R")


## Load configuration file
path_this <- get_script_path()
source(paste0(path_this, "config.R"))

## Load test-specific functions
source(paste0("tests/", test_suite, "/utils/wrappers.R"))
# source(paste0("tests/", test_suite, "/utils/fit_and_evaluate.R"))
# source(paste0("tests/", test_suite, "/utils/adjust_results.R"))
source(paste0("tests/", test_suite, "/utils/generate_data.R"))
# source(paste0("tests/", test_suite, "/utils/model_evaluation.R"))


n_comp <- 3
test_options <- list(
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
    n = 200,
    n_locs = 101
  ),
  model_options = list(
    cache_covariances = TRUE,
    n_comp = n_comp,
    init_strategy = "uniform",
    scheme = "factorial",
    block_deactivation = TRUE,
    connection_deactivation = TRUE,
    component_significance = TRUE
  ),
  bootstrap_options = list(),
  noise = list(
    sigma_noise = 1
  )
)

## Define and create work directories for the test suite
path_list <- create_paths("prova")
path_list <- update_paths(path_list, "test1", test_options)


data <- generate_data(test_options, seed = 0)

# Fitto il modello con tutte e tre le componenti
IGNORE_CPP_OUTPUT <- FALSE
IGNORE_R_OUTPUT <- FALSE
test_options$regularization$lambda <- 0
test_options$model_options$n_comp <- n_comp
fit_model <- CPP_RGCCA("CPP_GCCA_NN_cov", data, test_options, path_list)

idx = 1
plot_list <- list()
for(h in 1:n_comp) {
  for(i in 1:4) {
    plot_list[[idx]] <- plot.curve(data$locations_D, fit_model$results$A_hat_locs[[i]][,h],
                                 true = data$A_locs[[i]][,h]) + std_plot_settings_curves()
    idx <- idx + 1
  }
}
plot <- arrangeGrob(grobs = plot_list, ncol = 4)
grid.arrange(plot)