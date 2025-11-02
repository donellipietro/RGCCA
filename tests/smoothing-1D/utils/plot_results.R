# = ========================================================================== =
# - Script: plot_results.R
# - Desc: Provides visualization utilities for simulation results. Includes
#         quantitative summaries (e.g., RMSE, time) and qualitative comparisons
#         of reconstructed loadings, both at locations and on high-resolution
#         grids, for different functional PCA approaches.
# = ========================================================================== =


## Function: plot_quantitative_results
# - Args:
#   * loaded_results: list containing aggregated quantitative results and model info.
#       Expected fields include:
#         - $model_names, $model_labels, $model_colors
#         - $execution_time, $rmse (with nested lists for various metrics)
# - Desc:
#   Generates boxplots summarizing execution times and RMSE-based performance metrics
#   for all models. Separate figures are produced for reconstruction accuracy,
#   score orthogonality deviation, and component-wise RMSE for loadings and scores.
plot_quantitative_results <- function(loaded_results) {
  ## Get models details
  model_names <- loaded_results$model_names
  model_labels <- loaded_results$model_labels
  model_colors <- loaded_results$model_colors
  
  ## Time ----
  times <- loaded_results$execution_time
  indexes <- which(!is.nan(colSums(times[, model_names])))
  plot <- plot.grouped_boxplots(
    times[, c("Group", names(indexes))],
    values_name = "Time [seconds]",
    group_name = "",
    group_labels = "",
    subgroup_name = "Approaches",
    subgroup_labels = model_labels[indexes],
    subgroup_colors = model_colors[indexes]
  ) + std_plot_settings() + ggtitle("Time")
  print(plot)
  
  # ## RMSE ----
  # ## Overall measures
  # rmses <- loaded_results$rmse
  # names <- c("reconstruction_locs", "scores_orth")
  # titles <- c("Reconstruction at locations", "Deviation from scores orthogonality")
  # 
  # for (i in seq_along(names)) {
  #   name <- names[i]
  #   title <- titles[i]
  #   indexes <- which(!is.nan(colSums(rmses[[name]][, model_names])))
  #   plot <- plot.grouped_boxplots(
  #     rmses[[name]][, c("Group", names(indexes))],
  #     values_name = "RMSE",
  #     group_name = "",
  #     group_labels = "",
  #     subgroup_name = "Approaches",
  #     subgroup_labels = model_names[indexes],
  #     subgroup_colors = model_colors[indexes]
  #   ) + std_plot_settings() + ggtitle(title)
  #   print(plot)
  # }
  
}


## Function: plot_qualitative_results
# - Args:
#   * quantitative_results: list containing quantitative summaries (for selecting representative samples)
#   * qualitative_results: list containing qualitative outputs for each model, including:
#       - $loadings, $f_locs, $f_grid
#       - $f_true, $f_true_locs, $f_true_grid
#       - $domain, $grid, $locations, $knots, and $boundary
# - Desc:
#   Produces qualitative visual comparisons of true and reconstructed loadings across
#   models and principal components. For each method, three representative replicates
#   are selected based on RMSE quantiles (min, median, max). Each component’s fields
#   are plotted at knots, evaluation locations, and high-resolution grids, both with
#   and without isolines. Results are arranged in labeled grids for clarity.
plot_qualitative_results <- function(quantitative_results, qualitative_results) {
  ## Debugging helpers
  # quantitative_results <- loaded_qnt_results
  # qualitative_results  <- loaded_qlt_results
  
  ## Get fitted quantities
  f <- qualitative_results$f
  f_locs <- qualitative_results$f_locs
  f_grid <- qualitative_results$f_grid
  
  ## Get true f
  f_true <- qualitative_results$f_true
  f_true_locs <- qualitative_results$f_true_locs
  f_true_grid <- qualitative_results$f_true_grid
  
  ## Get infos
  domain <- qualitative_results$domain
  boundary <- domain$boundary
  
  ## Models info
  model_names <- quantitative_results$model_names
  model_labels <- qualitative_results$model_labels
  
  ## Labels for plot grids
  labels_cols <- c("True", "Quantile 0", "Quantile 0.5", "Quantile 1")
  labels_rows <- model_labels
  
  ## Room for plots
  plot_list_locs <- list()
  plot_list_grid <- list()
  
  ## Generate figures for each model
  for (m in seq_along(model_names)) { # m <- 1
    
    ## Model details
    name_model <- model_names[m]
    
    ## Select representative replicates (min, median, max RMSE)
    indexes <- tapply(
      quantitative_results$rmse$f_locs[[name_model]],
      quantitative_results$rmse$f_locs$Group,
      function(x) {
        sapply(quantile(x, c(0, 0.5, 1), na.rm = TRUE), function(q) which.min(abs(x - q)))
      }
    )
    i <- 1 ## only one component 
    
    ## Compute limits
    limits_grid <- apply(do.call(rbind, unlist(f_grid, recursive = FALSE)), 2, range)
    
    limits <- range(c(f_true_grid, limits_grid))
    breaks <- seq(limits[1], limits[2], length = 10)
    
    ## True f
    plot_list_locs[[4*(m-1) + 1]] <- plot.curve_points(
      qualitative_results$locations, f_true_locs, size = 1 # ,  LEGEND = FALSE
    ) + std_plot_settings_curves()
    
    plot_list_grid[[4*(m-1) + 1]] <- plot.curve(
      qualitative_results$grid, f_true_grid# , LEGEND = FALSE
    ) + std_plot_settings_curves()
    
    ## Reconstructed f for quantile-based replicates
    for (j in 1:3) {
      plot_list_locs[[4*(m-1) + j + 1]] <- plot.curve_points(
        qualitative_results$locations,
        f_locs[[name_model]][[indexes[[i]][j]]],
        true = f_true_locs,
        size = 1
      ) + std_plot_settings_curves()
      
      plot_list_grid[[4*(m-1) + j + 1]] <- plot.curve(
        qualitative_results$grid,
        f_grid[[name_model]][[indexes[[i]][j]]], 
        true = f_true_grid,
        limits = limits
      ) + std_plot_settings_curves()
    }
    
  }
  
  ## Arrange labeled grids for each display mode
  plots_locs <- labled_plots_grid(arrangeGrob(grobs = plot_list_locs, ncol = 4), "f at location", labels_cols, labels_rows)
  plots_grid <- labled_plots_grid(arrangeGrob(grobs = plot_list_grid, ncol = 4), "f at HR grid", labels_cols, labels_rows)
  
  ## Display plots sequentially
  grid.arrange(plots_locs)
  grid.arrange(plots_grid)
  
  
  #####
  
  
  ## Room for plots
  plot_list_locs <- list()
  plot_list_grid <- list()
  
  ## Generate figures for each model
  for (m in seq_along(model_names)) { # m <- 1
    
    ## Model details
    name_model <- model_names[m]
    
    ## Compute limits
    limits_grid <- apply(do.call(rbind, unlist(f_grid, recursive = FALSE)), 2, range)
    
    limits <- range(c(f_true_grid, limits_grid))
    breaks <- seq(limits[1], limits[2], length = 10)
    
    ## True f
    plot_list_locs[[4*(m-1) + 1]] <- plot.curve_points(
      qualitative_results$locations, f_true_locs, size = 1# , LEGEND = FALSE
    ) + std_plot_settings_curves()
    
    plot_list_grid[[4*(m-1) + 1]] <- plot.curve(
      qualitative_results$grid, f_true_grid# , LEGEND = FALSE
    ) + std_plot_settings_curves()
    
    ## Reconstructed f for quantile-based replicates
    for (j in 1:3) {
      plot_list_locs[[4*(m-1) + j + 1]] <- plot.curve_points(
        qualitative_results$locations,
        f_locs[[name_model]],
        true = f_true_locs,
        size = 1
      ) + std_plot_settings_curves()
      
      plot_list_grid[[4*(m-1) + j + 1]] <- plot.curve(
        qualitative_results$grid,
        f_grid[[name_model]], 
        true = f_true_grid,
        limits = limits
      ) + std_plot_settings_curves()
    }
    
  }
  
  ## Arrange labeled grids for each display mode
  plots_locs <- labled_plots_grid(arrangeGrob(grobs = plot_list_locs, ncol = 4), "f at location", labels_cols, labels_rows)
  plots_grid <- labled_plots_grid(arrangeGrob(grobs = plot_list_grid, ncol = 4), "f at HR grid", labels_cols, labels_rows)
  
  ## Display plots sequentially
  grid.arrange(plots_locs)
  grid.arrange(plots_grid)
}
