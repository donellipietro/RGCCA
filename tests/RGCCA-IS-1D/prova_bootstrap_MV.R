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
  "sf", "sp", "raster",
  # strings
  "glue"
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
    n = 1200,
    n_locs = 101
  ),
  model_options = list(
    n_comp = 4,
    init = "Uniform",
    lambda_selection_weights = FALSE,
    block_deactivation = TRUE,
    connection_deactivation = TRUE,
    component_significance = TRUE
  ),
  bootstrap_options = list(
    save_bootstrap_resamples = TRUE
  ),
  noise = list(
    sigma_noise = 2
  ),
  regularization = list(
    lambda = 0
  )
)


## Define and create work directories for the test suite
path_list <- create_paths("prova")
path_list <- update_paths(path_list, "test1", test_options)


data <- generate_data(test_options, seed = 0)

n_comp <- 4

IGNORE_CPP_OUTPUT <- FALSE
test_options$model_options$n_comp <- n_comp
results <- fit_model("CPP_GCCA_cov", data, path_list, test_options)
model_selection <- test_options$model_options$lambda_selection_weights || test_options$model_options$block_deactivation || test_options$model_options$connection_deactivation


plot_list <- list()
idx <- 1
for (h in 1:n_comp) {
  index_lambda_opt <- 1
  for (g in 1:4) {
    W_boot <- results$model_selection$bootstrap[[h]]$w_boot_locs[[index_lambda_opt]][[g]]
    w_fit <- results$model_selection$bootstrap[[h]]$w_fit_locs[[index_lambda_opt]][[g]]
    w_min <- results$model_selection$bootstrap[[h]]$w_min_locs[[index_lambda_opt]][[g]]

    w_fit_final <- results$results$A_hat_locs[[g]][, h]

    conf_int <- results$model_selection$bootstrap[[h]]$w_ci_locs[[index_lambda_opt]][[g]]

    if (model_selection) {
      plot_list[[idx]] <- plot.curve_bootstrap(
        data$locations_D,
        W_boot,
        # fit = w_fit,
        fit = w_fit_final,
        # w_min = w_min,
        true = data$A_locs[[g]][, h],
        conf_int = conf_int
      ) + std_plot_settings_curves()
    } else {
      plot_list[[idx]] <- plot.curve(
        data$locations_D,
        w_fit_final,
        true = data$A_locs[[g]][, h],
      ) + std_plot_settings_curves()
    }
    idx <- idx + 1
  }
}
plot <- arrangeGrob(grobs = plot_list, nrow = n_comp)
plot <- labeled_plots_grid(plot,
  title = "Weights (optimal lambda)",
  labels_cols = paste("Block", 1:4),
  labels_rows = paste("Comp", 1:n_comp)
)
grid.arrange(plot)


# ## CI for the corr matrix
# if (model_selection) {
#   plot_list <- list()
#   idx <- 1
#
#   for (h in 1:n_comp) {
#     index_lambda_opt <- 1
#
#     est <- results$results$correlation_matrices[[h]]
#     min <- results$model_selection$bootstrap[[h]]$corr_min[[index_lambda_opt]]
#     low <- results$model_selection$bootstrap[[h]]$corr_ci_low[[index_lambda_opt]]
#     upp <- results$model_selection$bootstrap[[h]]$corr_ci_high[[index_lambda_opt]]
#
#     mats <- list(low, est, upp, min)
#
#     for (m in 1:length(mats)) {
#       df <- expand.grid(
#         x = 1:ncol(mats[[m]]),
#         y = 1:nrow(mats[[m]])
#       )
#
#       df$value <- as.vector(mats[[m]])
#
#       p <- ggplot(df, aes(x, y, fill = value)) +
#         geom_tile() +
#         geom_text(aes(label = round(value, 2)), size = 3) +
#         scale_fill_gradient2(
#           low = "blue", mid = "white", high = "red", midpoint = 0,
#           limits = c(-1, 1),
#           guide = "none"
#         ) +
#         scale_y_reverse() +
#         coord_fixed() +
#         labs(
#           x = NULL,
#           y = NULL
#         ) +
#         theme_minimal() +
#         theme(
#           axis.text = element_text(size = 10),
#           panel.grid = element_blank(),
#           plot.title = element_text(hjust = 0.5)
#         )
#
#       plot_list[[idx]] <- p
#       idx <- idx + 1
#     }
#   }
#
#   plot <- arrangeGrob(grobs = plot_list, ncol = 4)
#   plot <- labeled_plots_grid(plot,
#     title = "Correlation matrices CI",
#     labels_cols = c("Lower", "Estimate", "Upper", "Corrected"), # c("True", "Lower", "Estimate", "Upper"),
#     labels_rows = paste("Comp", 1:n_comp)
#   )
#
#   grid.arrange(plot)
# }
