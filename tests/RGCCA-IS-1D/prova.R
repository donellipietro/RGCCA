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
    n = 100, # 1200
    n_locs = 101
  ),
  model_options = list(          
    n_comp = 3,
    init = "Uniform",
    lambda_selection_weights = FALSE
  ),
  noise = list(
    sigma_noise = 5
  ),
  regularization = list(
    lambda = 1e-4
  )
)

## Define and create work directories for the test suite
path_list <- create_paths("prova")
path_list <- update_paths(path_list, "test1", test_options)


data <- generate_data(test_options, seed = 0)


par(mfrow = c(1,4), mar = c(2,2,2,2))
matplot(data$locations_D, t(data$X[[1]]), type = "l", xlab = "", ylab = "", main = "Group 1")
matplot(data$locations_D, t(data$X[[2]]), type = "l", xlab = "", ylab = "", main = "Group 2")
matplot(data$locations_D, t(data$X[[3]]), type = "l", xlab = "", ylab = "", main = "Group 3")
matplot(data$locations_D, t(data$X[[4]]), type = "l", xlab = "", ylab = "", main = "Group 4")
# 
# par(mfrow = c(2,2), mar = c(2,2,2,2))
# matplot(data$locations_D, t(data$X_locs[[1]]), type = "l", xlab = "", ylab = "", main = "Group 1")
# matplot(data$locations_D, t(data$X_locs[[4]]), type = "l", xlab = "", ylab = "", main = "Group 4")
# matplot(data$locations_D, t(data$X_locs[[2]]), type = "l", xlab = "", ylab = "", main = "Group 2")
# matplot(data$locations_D, t(data$X_locs[[3]]), type = "l", xlab = "", ylab = "", main = "Group 3")

# plot.curve(data$grid_D, data$A_grid[[1]][,1]) + std_plot_settings_curves()
# plot.curve(data$grid_D, data$A_grid[[1]][,2]) + std_plot_settings_curves()
# plot.curve(data$grid_D, data$A_grid[[1]][,3]) + std_plot_settings_curves()

# plot.curve(data$grid_T, data$E_grid[[1]][,1]) + std_plot_settings_curves()
# plot.curve(data$grid_T, data$E_grid[[1]][,2]) + std_plot_settings_curves()
# plot.curve(data$grid_T, data$E_grid[[1]][,3]) + std_plot_settings_curves()




IGNORE_CPP_OUTPUT = FALSE
test_options$model_options$n_comp <- 3
test_options$model_options$lambda_selection_weights <- FALSE
results <- CPP_RGCCA("CPP_fGCCA_cov_FEM", data, test_options, path_list)
results <- CPP_RGCCA("CPP_fGCCA_cov_SPLINES", data, test_options, path_list)
results <- CPP_RGCCA("CPP_fGCCA_NN_cov_FEM", data, test_options, path_list)
results <- CPP_RGCCA("CPP_fGCCA_NN_cov_SPLINES", data, test_options, path_list)

h <- 2

plot_list <- list()
for(g in 1:4) {
  plot_list[[g]] <- plot.curve(data$grid_D, results$results$A_hat_grid[[g]][,h],
                               true = data$A_grid[[g]][,h]) + std_plot_settings_curves()
}
plot <- arrangeGrob(grobs = plot_list)
grid.arrange(plot)




# Fitto il modello con tutte e tre le componenti
test_options$regularization$lambda <- 0
results1 <- R_RGCCA("R_GCCA_cov", data, test_options)
results2 <- CPP_RGCCA("CPP_GCCA_cov", data, test_options, path_list)
results3 <- CPP_RGCCA("CPP_GCCA_NN_cov", data, test_options, path_list)

h <- 1

plot_list <- list()
for(i in 1:4) {
  plot_list[[i]] <- plot.curve(data$locations_D, results1$results$A_hat_locs[[i]][,h],
                               true = data$A_locs[[i]][,h]) + std_plot_settings_curves()
}
plot <- arrangeGrob(grobs = plot_list)
grid.arrange(plot)

plot_list <- list()
for(i in 1:4) {
  plot_list[[i]] <- plot.curve(data$locations_D, results2$results$A_hat_locs[[i]][,h],
                               true = data$A_locs[[i]][,h]) + std_plot_settings_curves()
}
plot <- arrangeGrob(grobs = plot_list)
grid.arrange(plot)

plot_list <- list()
for(i in 1:4) {
  plot_list[[i]] <- plot.curve(data$locations_D, results3$results$A_hat_locs[[i]][,h],
                               true = data$A_locs[[i]][,h]) + std_plot_settings_curves()
}
plot <- arrangeGrob(grobs = plot_list)
grid.arrange(plot)

max(c(max(abs(results1$results$A_hat_locs[[1]] - results2$results$A_hat_locs[[1]])),
      max(abs(results1$results$A_hat_locs[[2]] - results2$results$A_hat_locs[[2]])),
      max(abs(results1$results$A_hat_locs[[3]] - results2$results$A_hat_locs[[3]])),
      max(abs(results1$results$A_hat_locs[[4]] - results2$results$A_hat_locs[[4]]))))

max(c(max(abs(results1$results$A_star_hat_locs[[1]] - results2$results$A_star_hat_locs[[1]])),
      max(abs(results1$results$A_star_hat_locs[[2]] - results2$results$A_star_hat_locs[[2]])),
      max(abs(results1$results$A_star_hat_locs[[3]] - results2$results$A_star_hat_locs[[3]])),
      max(abs(results1$results$A_star_hat_locs[[4]] - results2$results$A_star_hat_locs[[4]]))))

max(c(max(abs(results1$results$H_hat[[1]] - results2$results$H_hat[[1]])),
      max(abs(results1$results$H_hat[[2]] - results2$results$H_hat[[2]])),
      max(abs(results1$results$H_hat[[3]] - results2$results$H_hat[[3]])),
      max(abs(results1$results$H_hat[[4]] - results2$results$H_hat[[4]]))))