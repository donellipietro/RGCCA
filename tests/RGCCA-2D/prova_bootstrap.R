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

n_comp <- 3
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
    n_nodes_HR_grid_D = c(4 * 1e3, 2 * 1e3, 2 * 1e3, 4 * 1e3),
    n_nodes_HR_grid_T = 501,
    n_locs_D = c(800),
    n_locs_mult = c(3, 2, 2, 3),
    n_times = c(1200)
  ),
  model_options = rgcca_model_options(
    n_comp = n_comp,
    init = "Uniform",
    lambda_selection_weights = TRUE
  ),
  bootstrap_options = rgcca_bootstrap_options(
    resampling_strategy = "Stationary",
    stationary_block_length = 0,
    save_bootstrap_resamples = TRUE
  ),
  noise = list(
    sigma_noise = 6
  ),
  regularization = list(
    lambda = 1e-4,
    lambda_grid = list(
      10^(-9:2), 10^(-9:2), 10^(-9:2)
    )
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
results <- CPP_RGCCA(model_name, data, test_options, path_list)


par(mfrow = c(n_comp, 1))
for (h in 1:n_comp) {
  lambda_grid <- results$model_selection$bootstrap[[h]]$lambda_grid
  lambda_opt <- results$model_selection$bootstrap[[h]]$lambda_opt
  index_lambda_opt <- which(lambda_opt == results$model_selection$bootstrap[[h]]$lambda_grid)

  colors <- rep("black", length(lambda_grid))
  pch <- rep(1, length(lambda_grid))
  colors[index_lambda_opt] <- "darkgreen"
  pch[index_lambda_opt] <- 19


  plot(
    log10(lambda_grid),
    results$model_selection$bootstrap[[h]]$criterion,
    ylab = "Score", xlab = "log10(lambda)", main = glue("Comp. {h}"),
    type = "b",
    col = colors, pch = pch
  )
}


# # tile
# id <- 1
# plot_list <- list()
# for(g in 1:4) {
#   for(h in 1:3) {
#     plot_list[[(g-1)*3 + h]] <- plot.field_tile(
#       data$grid_D[[g]],
#       results$results$A_hat_grid[[g]][,h],
#       boundary = data$domain_D[[g]]$boundary
#     ) + std_plot_settings_fields()
#   }
# }
# plot <- arrangeGrob(grobs = plot_list, ncol = 3)
# grid.arrange(plot)


plot_list_w_fit <- list()
plot_list_w_fit_final <- list()
plot_list_w_min <- list()
idx <- 1
for (h in 1:n_comp) {
  lambda_opt <- results$model_selection$bootstrap[[h]]$lambda_opt
  index_lambda_opt <- which(lambda_opt == results$model_selection$bootstrap[[h]]$lambda_grid)

  for (g in 1:4) {
    W_boot <- results$model_selection$bootstrap[[h]]$w_boot_grid[[index_lambda_opt]][[g]]
    w_fit <- results$model_selection$bootstrap[[h]]$w_fit_grid[[index_lambda_opt]][[g]]
    w_min <- results$model_selection$bootstrap[[h]]$w_min_grid[[index_lambda_opt]][[g]]
    w_fit_final <- results$results$A_hat_grid[[g]][, h]

    if (max(abs(w_min)) < 1e-8) {
      w_min <- w_min * 0
    }

    conf_int <- cbind(
      apply(W_boot, 1, quantile, probs = 0.025),
      apply(W_boot, 1, quantile, probs = 0.975)
    )

    plot_list_w_fit[[idx]] <- plot.field_tile(
      data$grid_D[[g]],
      w_fit,
      boundary = data$domain_D[[g]]$boundary
    ) + std_plot_settings_fields()

    plot_list_w_fit_final[[idx]] <- plot.field_tile(
      data$grid_D[[g]],
      w_fit_final,
      boundary = data$domain_D[[g]]$boundary
    ) + std_plot_settings_fields()

    plot_list_w_min[[idx]] <- plot.field_tile(
      data$grid_D[[g]],
      w_min,
      boundary = data$domain_D[[g]]$boundary
    ) + std_plot_settings_fields()
    idx <- idx + 1
  }
}
plot <- arrangeGrob(grobs = plot_list_w_fit, nrow = n_comp)
plot <- labeled_plots_grid(plot,
  title = "Weights",
  labels_cols = paste("Block", 1:4),
  labels_rows = paste("Comp", 1:n_comp)
)
grid.arrange(plot)

plot <- arrangeGrob(grobs = plot_list_w_fit_final, nrow = n_comp)
plot <- labeled_plots_grid(plot,
  title = "Weights final",
  labels_cols = paste("Block", 1:4),
  labels_rows = paste("Comp", 1:n_comp)
)
grid.arrange(plot)

plot <- arrangeGrob(grobs = plot_list_w_min, nrow = n_comp)
plot <- labeled_plots_grid(plot,
  title = "Min. envelope",
  labels_cols = paste("Block", 1:4),
  labels_rows = paste("Comp", 1:n_comp)
)
grid.arrange(plot)



# IGNORE_CPP_OUTPUT <- FALSE
# test_options$model_options$lambda_selection_weights <- FALSE
# test_options$regularization$lambda <- 0
# model_name <- "CPP_GCCA_cov"
# results_MV <- CPP_RGCCA(model_name, data, test_options, path_list)
#
#
# plot_list <- list()
# idx <- 1
# for(h in 1:n_comp) {
#   for(g in 1:4) {
#     plot_list[[idx]] <- plot.field_points(
#       data$locations_D[[g]],
#       results_MV$results$A_hat_locs[[g]][,h],
#       boundary = data$domain_D[[g]]$boundary,
#       size = 1
#     ) + std_plot_settings_fields()
#     idx <- idx +1
#   }
# }
# plot <- arrangeGrob(grobs = plot_list, nrow = n_comp)
# plot <- labeled_plots_grid(plot, title = "Weights",
#                           labels_cols = paste("Block", 1:4),
#                           labels_rows = paste("Comp", 1:n_comp))
# grid.arrange(plot)
#
