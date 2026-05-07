rm(list = ls())
graphics.off()
options(warn = -1)

invisible(suppressMessages(sapply(c(
  ## Competitors
  "RGCCA",
  # discretization
  "fdaPDE", "femR",
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
    n = 100,
    n_locs = 501
  ),
  model_options = list(          
    n_comp = 2
  ),
  noise = list(
    sigma_noise = 0.5
  ),
  regularization = list(
    lambda = 1e-1
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

par(mfrow = c(2,2), mar = c(2,2,2,2))
matplot(data$locations_D, t(data$X_locs[[1]]), type = "l", xlab = "", ylab = "", main = "Group 1")
matplot(data$locations_D, t(data$X_locs[[4]]), type = "l", xlab = "", ylab = "", main = "Group 4")
matplot(data$locations_D, t(data$X_locs[[2]]), type = "l", xlab = "", ylab = "", main = "Group 2")
matplot(data$locations_D, t(data$X_locs[[3]]), type = "l", xlab = "", ylab = "", main = "Group 3")

# plot.curve(data$grid_D, data$A_grid[[1]][,1]) + std_plot_settings_curves()
# plot.curve(data$grid_D, data$A_grid[[1]][,2]) + std_plot_settings_curves()
# plot.curve(data$grid_D, data$A_grid[[1]][,3]) + std_plot_settings_curves()

# plot.curve(data$grid_T, data$E_grid[[1]][,1]) + std_plot_settings_curves()
# plot.curve(data$grid_T, data$E_grid[[1]][,2]) + std_plot_settings_curves()
# plot.curve(data$grid_T, data$E_grid[[1]][,3]) + std_plot_settings_curves()


# Fitto il modello con tutte e tre le componenti
results1 <- R_RGCCA("R_GCCA_cor", data, test_options)
results2 <- CPP_RGCCA("CPP_GCCA_cor", data, test_options, path_list)
results3 <- CPP_RGCCA("CPP_GCCA_NN_cor", data, test_options, path_list)

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

# 
# max(abs(results1$results$A_hat_locs[[1]][,1] - results2$results$A_hat_locs[[1]][,1]))
# max(abs(results1$results$A_hat_locs[[2]][,1] - results2$results$A_hat_locs[[2]][,1]))
# max(abs(results1$results$A_hat_locs[[3]][,1] - results2$results$A_hat_locs[[3]][,1]))
# max(abs(results1$results$A_hat_locs[[4]][,1] - results2$results$A_hat_locs[[4]][,1]))
# 
# max(abs(results1$results$H_hat[[1]][,1] - results2$results$H_hat[[1]][,1]))
# max(abs(results1$results$H_hat[[2]][,1] - results2$results$H_hat[[2]][,1]))
# max(abs(results1$results$H_hat[[3]][,1] - results2$results$H_hat[[3]][,1]))
# max(abs(results1$results$H_hat[[4]][,1] - results2$results$H_hat[[4]][,1]))
# 
# max(abs(results1$results$A_hat_locs[[1]][,2] - results2$results$A_hat_locs[[1]][,2]))
# max(abs(results1$results$A_hat_locs[[2]][,2] - results2$results$A_hat_locs[[2]][,2]))
# max(abs(results1$results$A_hat_locs[[3]][,2] - results2$results$A_hat_locs[[3]][,2]))
# max(abs(results1$results$A_hat_locs[[4]][,2] - results2$results$A_hat_locs[[4]][,2]))
# 
# max(abs(results1$results$H_hat[[1]][,2] - results2$results$H_hat[[1]][,2]))
# max(abs(results1$results$H_hat[[2]][,2] - results2$results$H_hat[[2]][,2]))
# max(abs(results1$results$H_hat[[3]][,2] - results2$results$H_hat[[3]][,2]))
# max(abs(results1$results$H_hat[[4]][,2] - results2$results$H_hat[[4]][,2]))

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


IGNORE_CPP_OUTPUT = FALSE
results_splines_cov <- CPP_RGCCA("CPP_fGCCA_cor_SPLINES", data, test_options, path_list)

h <- 1

plot_list <- list()
for(i in 1:4) {
  plot_list[[i]] <- plot.curve(data$locations_D, results_splines_cov$results$A_hat_locs[[i]][,h],
                               true = data$A_locs[[i]][,h]) + std_plot_settings_curves()
}
plot <- arrangeGrob(grobs = plot_list)
grid.arrange(plot)





# plot_list <- list()
# for(i in 1:4) {
#   plot_list[[i]] <- plot.curve(data$locations_D, results1$results$A_hat_locs[[i]][,1],
#                                true = data$A_locs[[i]][,1]) + std_plot_settings_curves()
# }
# plot <- arrangeGrob(grobs = plot_list)
# grid.arrange(plot)
# plot_list <- list()
# for(i in 1:4) {
#   plot_list[[i]] <- plot.curve(data$locations_D, results2$results$A_hat_locs[[i]][,1],
#                                true = data$A_locs[[i]][,1]) + std_plot_settings_curves()
# }
# plot <- arrangeGrob(grobs = plot_list)
# grid.arrange(plot)
# 
# 
# plot_list <- list()
# for(i in 1:4) {
#   plot_list[[i]] <- plot.curve(data$locations_D, results1$results$A_hat_locs[[i]][,2],
#                                true = data$A_locs[[i]][,2]) + std_plot_settings_curves()
# }
# plot <- arrangeGrob(grobs = plot_list)
# grid.arrange(plot)
# plot_list <- list()
# for(i in 1:4) {
#   plot_list[[i]] <- plot.curve(data$locations_D, results2$results$A_hat_locs[[i]][,2],
#                                true = data$A_locs[[i]][,2]) + std_plot_settings_curves()
# }
# plot <- arrangeGrob(grobs = plot_list)
# grid.arrange(plot)
# 
# 
# plot_list <- list()
# for(i in 1:4) {
#   plot_list[[i]] <- plot.curve(data$locations_D, results1$results$A_hat_locs[[i]][,3],
#                                true = data$A_locs[[i]][,3]) + std_plot_settings_curves()
# }
# plot <- arrangeGrob(grobs = plot_list)
# grid.arrange(plot)
# plot_list <- list()
# for(i in 1:4) {
#   plot_list[[i]] <- plot.curve(data$locations_D, results2$results$A_hat_locs[[i]][,3],
#                                true = data$A_locs[[i]][,3]) + std_plot_settings_curves()
# }
# plot <- arrangeGrob(grobs = plot_list)
# grid.arrange(plot)
# 
# 
# IGNORE_CPP_OUTPUT = FALSE
# IGNORE_R_OUTPUT = FALSE
# 
# data <- generate_data(
#   test_options = test_options,
#   seed = 0
# )
# 
# results_MV_R_cor <- R_RGCCA("R_GCCA_cor", data, test_options)
# results_MV_R_cov <- R_RGCCA("R_GCCA_cov", data, test_options)
# results_MV_R <- R_RGCCA("R_RGCCA", data, test_options)
# results_MV_CPP <- CPP_RGCCA("CPP_GCCA_cor", data, test_options, path_list)
# results_MV_imp <- CPP_RGCCA("CPP_RGCCA_improved", data, test_options, path_list)
# results_fem <- CPP_RGCCA("CPP_fRGCCA_FEM", data, test_options, path_list)
# results_splines_cor <- CPP_RGCCA("CPP_fRGCCA_cor", data, test_options, path_list)
# results_splines_cov <- CPP_RGCCA("CPP_fRGCCA_cov", data, test_options, path_list)
# results_splines <- CPP_RGCCA("CPP_fRGCCA_SPLINES", data, test_options, path_list)
# results_fgcca <- R_FGCCA("R_FGCCA_cov", data, test_options)
# 
# # R - cor
# plot_list <- list()
# for(i in 1:4) {
#   for(h in 1:3) {
#     plot_list[[3*(i-1) + h]] <- plot.curve(data$locations_D, results_MV_R_cor$results$A_hat_locs[[i]][,h],
#                                  true = data$A_locs[[i]][,h]) + std_plot_settings_curves()
#   }
# }
# plot <- arrangeGrob(grobs = plot_list)
# plot <- labled_plots_grid(plot, title = "R", labels_cols = paste0("h = ", 1:3), labels_rows = paste0("g", 1:4))
# grid.arrange(plot)
# 
# # R - cov
# plot_list <- list()
# for(i in 1:4) {
#   for(h in 1:3) {
#     plot_list[[3*(i-1) + h]] <- plot.curve(data$locations_D, results_MV_R_cov$results$A_hat_locs[[i]][,h],
#                                            true = data$A_locs[[i]][,h]) + std_plot_settings_curves()
#   }
# }
# plot <- arrangeGrob(grobs = plot_list)
# plot <- labled_plots_grid(plot, title = "R", labels_cols = paste0("h = ", 1:3), labels_rows = paste0("g", 1:4))
# grid.arrange(plot)
# 
# # R
# plot_list <- list()
# for(i in 1:4) {
#   for(h in 1:3) {
#     plot_list[[3*(i-1) + h]] <- plot.curve(data$locations_D, results_MV_R$results$A_hat_locs[[i]][,h],
#                                            true = data$A_locs[[i]][,h]) + std_plot_settings_curves()
#   }
# }
# plot <- arrangeGrob(grobs = plot_list)
# plot <- labled_plots_grid(plot, title = "R", labels_cols = paste0("h = ", 1:3), labels_rows = paste0("g", 1:4))
# grid.arrange(plot)
# 
# # CPP
# plot_list <- list()
# for(i in 1:4) {
#   for(h in 1:3) {
#     plot_list[[3*(i-1) + h]] <- plot.curve(data$locations_D, results_MV_CPP$results$A_hat_locs[[i]][,h],
#                                            true = data$A_locs[[i]][,h]) + std_plot_settings_curves()
#   }
# }
# plot <- arrangeGrob(grobs = plot_list)
# plot <- labled_plots_grid(plot, title = "CPP", labels_cols = paste0("h = ", 1:3), labels_rows = paste0("g", 1:4))
# grid.arrange(plot)
# 
# # Improved
# plot_list <- list()
# for(i in 1:4) {
#   for(h in 1:3) {
#     plot_list[[3*(i-1) + h]] <- plot.curve(data$locations_D, results_MV_imp$results$A_hat_locs[[i]][,h],
#                                            true = data$A_locs[[i]][,h]) + std_plot_settings_curves()
#   }
# }
# plot <- arrangeGrob(grobs = plot_list)
# plot <- labled_plots_grid(plot, title = "Improved", labels_cols = paste0("h = ", 1:3), labels_rows = paste0("g", 1:4))
# grid.arrange(plot)
# 
# # fem
# plot_list <- list()
# for(i in 1:4) {
#   for(h in 1:3) {
#     plot_list[[3*(i-1) + h]] <- plot.curve(data$locations_D, results_fem$results$A_hat_locs[[i]][,h],
#                                            true = data$A_locs[[i]][,h]) + std_plot_settings_curves()
#   }
# }
# plot <- arrangeGrob(grobs = plot_list)
# plot <- labled_plots_grid(plot, title = "FEM", labels_cols = paste0("h = ", 1:3), labels_rows = paste0("g", 1:4))
# grid.arrange(plot)
# 
# 
# # splines - cor
# plot_list <- list()
# for(i in 1:4) {
#   for(h in 1:3) {
#     plot_list[[3*(i-1) + h]] <- plot.curve(data$locations_D, results_splines_cor$results$A_hat_locs[[i]][,h],
#                                            true = data$A_locs[[i]][,h]) + std_plot_settings_curves()
#   }
# }
# plot <- arrangeGrob(grobs = plot_list)
# plot <- labled_plots_grid(plot, title = "Splines", labels_cols = paste0("h = ", 1:3), labels_rows = paste0("g", 1:4))
# grid.arrange(plot)
# 
# # splines - cov
# plot_list <- list()
# for(i in 1:4) {
#   for(h in 1:3) {
#     plot_list[[3*(i-1) + h]] <- plot.curve(data$locations_D, results_splines_cov$results$A_hat_locs[[i]][,h],
#                                            true = data$A_locs[[i]][,h]) + std_plot_settings_curves()
#   }
# }
# plot <- arrangeGrob(grobs = plot_list)
# plot <- labled_plots_grid(plot, title = "Splines", labels_cols = paste0("h = ", 1:3), labels_rows = paste0("g", 1:4))
# grid.arrange(plot)
# 
# 
# # splines
# plot_list <- list()
# for(i in 1:4) {
#   for(h in 1:3) {
#     plot_list[[3*(i-1) + h]] <- plot.curve(data$locations_D, results_splines$results$A_hat_locs[[i]][,h],
#                                            true = data$A_locs[[i]][,h]) + std_plot_settings_curves()
#   }
# }
# plot <- arrangeGrob(grobs = plot_list)
# plot <- labled_plots_grid(plot, title = "Splines", labels_cols = paste0("h = ", 1:3), labels_rows = paste0("g", 1:4))
# grid.arrange(plot)
# 
# # fgcca
# plot_list <- list()
# for(i in 1:4) {
#   for(h in 1:3) {
#     plot_list[[3*(i-1) + h]] <- plot.curve(data$grid_D, results_fgcca$results$A_hat_grid[[i]][,h],
#     ) + std_plot_settings_curves()
#   }
# }
# plot <- arrangeGrob(grobs = plot_list)
# plot <- labled_plots_grid(plot, title = "fgcca", labels_cols = paste0("h = ", 1:3), labels_rows = paste0("g", 1:4))
# grid.arrange(plot)
# 
# 
# seed <- 0
# n = 1000
# rho <- 0.8 
# Sca1 <- diag(1, 4, 4)
# Cor1 <- diag(1, 4, 4)
# Cor1[1,3] <- Cor1[3,1] <- rho
# Cor1[2,4] <- Cor1[4,2] <- -rho
# set.seed(seed)
# H1 <- mvrnorm(n = n, mu = rep(0,4), Sigma = 1.2 * Sca1 %*% Cor1 %*% Sca1, empirical = TRUE)
# Sca2 <- diag(1, 4, 4)
# Sca2[3,3] <- Sca2[4,4] <- 0
# Cor2 <- diag(1, 4, 4)
# Cor2[1,2] <- Cor2[2,1] <- rho
# H2 <- mvrnorm(n = n, mu = rep(0,4), Sigma = 1.1 * Sca2 %*% Cor2 %*% Sca2, empirical = TRUE)
# Sca3 <- diag(1, 4, 4)
# Sca3[2,2] <- Sca3[3,3] <- 0
# Cor3 <- diag(1, 4, 4)
# H3 <- mvrnorm(n = n, mu = rep(0,4), Sigma = Sca3 %*% Cor3 %*% Sca3, empirical = TRUE)
# 
# H <- cbind(H1, H2, H3)
# 
# Sca <- diag(1, 12, 12)
# Cor <- diag(1, 12, 12)
# Cor[1,3] <- Cor[3,1] <- rho
# Cor[2,4] <- Cor[4,2] <- -rho
# Sca[4+3,4+3] <- Sca[4+4,4+4] <- 0
# Cor[4+1,4+2] <- Cor[4+2,4+1] <- rho
# Sca[8+2,8+2] <- Sca[8+3,8+3] <- 0
# seed <- 0
# HH <- mvrnorm(n = n, mu = rep(0,12), Sigma = Sca %*% Cor %*% Sca, empirical = TRUE)
# 
# pairs(H1)
# pairs(H2)
# pairs(H3)
# pairs(H)
# pairs(HH)
# 
# abs(t(H) %*% H) / 1000 > 1e-15
# abs(t(HH) %*% HH) / 1000 > 1e-14
# 
# pairs()
# 
