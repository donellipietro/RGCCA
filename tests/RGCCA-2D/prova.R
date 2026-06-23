rm(list = ls())
graphics.off()
options(warn = -1)

invisible(suppressMessages(sapply(c(
  ## Competitors
  "RGCCA",
  # discretization
  "femR",
  # algebraic utils
  "pracma",
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
source("src/utils/svg_utils.R")


## Load configuration file
path_this <- get_script_path()
source(paste0(path_this, "config.R"))

## Load test-specific functions
source(paste0("tests/", test_suite, "/utils/wrappers.R"))
# source(paste0("tests/", test_suite, "/utils/fit_and_evaluate.R"))
# source(paste0("tests/", test_suite, "/utils/adjust_results.R"))
source(paste0("tests/", test_suite, "/utils/generate_data.R"))
# source(paste0("tests/", test_suite, "/utils/model_evaluation.R"))


test_options <- list(
  cpp_script = "RGCCA-2D",
  test_options = list(
    n_reps = 3
  ),
  domain_and_locations = list(
    name_mesh = paste0("region_", 1:4),
    T_sec = 200
  ),
  dimensions = list(
    n_groups = 4,
    n_nodes_T = c(51),
    n_nodes_HR_grid_D = c(4*1e3, 2*1e3, 2*1e3, 4*1e3),
    n_nodes_HR_grid_T = 501,
    n_locs_D = c(800),
    n_locs_mult = c(3, 2, 2, 3),
    n_times = c(1200)
  ),
  model_options = rgcca_model_options(
    n_comp = 3
  ),
  bootstrap_options = rgcca_bootstrap_options(),
  noise = list(
    sigma_noise = 1
  ),
  regularization = list(
    lambda = 1e-1
  )
)

## Define and create work directories for the test suite
path_list <- create_paths("prova")
path_list <- update_paths(path_list, "test1", test_options)


data <- generate_data(test_options, seed = 0)


# model_name <- "CPP_GCCA_cov"
# result <- CPP_RGCCA(model_name, data, test_options, path_list)
#
# model_name <- "CPP_fGCCA_cov"
# result <- CPP_RGCCA(model_name, data, test_options, path_list)

IGNORE_CPP_OUTPUT <- FALSE
model_name <- "CPP_fGCCA_NN_cov"
result <- CPP_RGCCA(model_name, data, test_options, path_list)


# points
id <- 1
plot_list <- list()
for(g in 1:4) {
  for(h in 1:3) {
    plot_list[[(g-1)*3 + h]] <- plot.field_points(
      data$locations_D[[g]],
      result$results$A_hat_locs[[g]][,h],
      boundary = data$domain_D[[g]]$boundary,
      size = 2
    ) + std_plot_settings_fields()
  }
}
plot <- arrangeGrob(grobs = plot_list, ncol = 3)
grid.arrange(plot)

# tile
id <- 1
plot_list <- list()
for(g in 1:4) {
  for(h in 1:3) {
    plot_list[[(g-1)*3 + h]] <- plot.field_tile(
      data$grid_D[[g]],
      result$results$A_hat_grid[[g]][,h],
      boundary = data$domain_D[[g]]$boundary
    ) + std_plot_settings_fields()
  }
}
plot <- arrangeGrob(grobs = plot_list, ncol = 3)
grid.arrange(plot)


plot_list <- list()
for(g in 1:4) {
  for(h in 1:3) {
    plot_list[[(g-1)*3 + h]] <- plot.curve_points(
      data$locations_T,
      result$results$E_hat_locs[[g]][,h],
      true = data$E_locs[[g]][,h]
    ) + std_plot_settings_curves()
  }
}
plot <- arrangeGrob(grobs = plot_list, ncol = 3)
grid.arrange(plot)
