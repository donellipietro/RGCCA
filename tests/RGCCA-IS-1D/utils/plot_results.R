# = ========================================================================== =
# - Script: plot_results.R
# - Desc: Provides visualization utilities for simulation results. Includes
#         quantitative summaries (e.g., RMSE, time) and qualitative comparisons
#         of reconstructed loadings, both at locations and on high-resolution
#         grids, for different functional PCA approaches.
# = ========================================================================== =


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
  
  ## RMSE ----
  ## Overall measures
  rmses <- loaded_results$rmse
  names <- c("H1", "H2", "H2", "H4", "A1_locs", "A2_locs", "A3_locs", "A4_locs")
  titles <- c("H1", "H2", "H2", "H4", "A1_locs", "A2_locs", "A3_locs", "A4_locs")

  for (i in seq_along(names)) {
    name <- names[i]
    title <- titles[i]
    indexes <- which(!is.nan(colSums(rmses[[name]][, model_names])))
    plot <- plot.grouped_boxplots(
      rmses[[name]][, c("Group", names(indexes))],
      values_name = "RMSE",
      group_name = "",
      group_labels = "",
      subgroup_name = "Approaches",
      subgroup_labels = model_names[indexes],
      subgroup_colors = model_colors[indexes]
    ) + std_plot_settings() + ggtitle(title)
    print(plot)
  }
  
}


plot_qualitative_results <- function(group, quantitative_results, qualitative_results) {
  ## Debugging helpers
  # group <- 1
  # quantitative_results <- loaded_qnt_results
  # qualitative_results  <- loaded_qlt_results
  
  n_comp <- as.numeric(max(quantitative_results$rmse$A1_locs$Group))
  
  ## Get fitted quantities
  A_locs <- qualitative_results$A_locs
  A_grid <- qualitative_results$A_grid
  A_star_locs <- qualitative_results$A_star_locs
  A_star_grid <- qualitative_results$A_star_grid
  # E_locs <- qualitative_results$E_locs
  # E_grid <- qualitative_results$E_grid
  
  ## Get true quantities
  A_true_locs <- qualitative_results$A_true_locs
  A_true_grid <- qualitative_results$A_true_grid
  # E_true_locs <- qualitative_results$E_true_locs
  # E_true_grid <- qualitative_results$E_true_grid
  
  ## Get infos
  domain_D <- qualitative_results$domain_D
  
  ## Models info
  model_names <- quantitative_results$model_names
  model_labels <- qualitative_results$model_labels
  
  ## Labels for plot grids
  labels_cols <- c("True", "Quantile 0", "Quantile 0.5", "Quantile 1")
  labels_rows <- paste("comp.", 1:n_comp)
  
  ## Generate figures for each model
  for (m in seq_along(model_names)) { # m <- 4
    
    ## Room for plots
    plot_list <- list()
    plot_list <- list()
    
    ## Model details
    name_model <- model_names[m]
    label_model <- model_labels[m]
    
    for (i in 1:n_comp) { # i <- 1
      
      ## Select representative replicates (min, median, max RMSE)
      indexes <- tapply(
        quantitative_results$rmse[[paste0("A", group, "_locs")]][[name_model]],
        quantitative_results$rmse[[paste0("A", group, "_locs")]]$Group,
        function(x) {
          sapply(quantile(x, c(0, 0.5, 1), na.rm = TRUE), function(q) which.min(abs(x - q)))
        }
      )
      
      ## Compute limits
      limits_grid <- apply(do.call(rbind, unlist(A_locs[[group]], recursive = FALSE)), 2, range)
      
      limits <- range(c(A_true_grid[[group]], limits_grid))
      
      ## True f
      # plot_list[[4*(i-1) + 1]] <- plot.curve_points(
      #   qualitative_results$locations_D, A_true_locs[[group]][, i], size = 1
      # ) + std_plot_settings_curves()
      
      plot_list[[4*(i-1) + 1]] <- plot.curve(
        qualitative_results$grid_D, A_true_grid[[group]][, i]# , LEGEND = FALSE
      ) + std_plot_settings_curves()
      
      ## Reconstructed f for quantile-based replicates
      for (j in 1:3) {
        
        if(length(A_grid)==0 || is.null(A_grid[[group]][[name_model]][[indexes[[i]][j]]])) {
          plot_list[[4*(i-1) + j + 1]] <- plot.curve_points(
            qualitative_results$locations_D,
            A_locs[[group]][[name_model]][[indexes[[i]][j]]][, i],
            true = A_true_locs[[group]][, i],
            size = 1
          ) + std_plot_settings_curves()
        } else {
          plot_list[[4*(i-1) + j + 1]] <- plot.curve(
            qualitative_results$grid_D,
            A_grid[[group]][[name_model]][[indexes[[i]][j]]][, i],
            true = A_true_grid[[group]][, i]
          ) + std_plot_settings_curves()
        }
      }
      
    }
    
    ## Arrange labeled grids for each display mode
    plots_locs <- labled_plots_grid(arrangeGrob(grobs = plot_list, ncol = 4), paste("A - model:", label_model), labels_cols, labels_rows)
    # plots_grid <- labled_plots_grid(arrangeGrob(grobs = plot_list, ncol = 4), "A at HR grid", labels_cols, labels_rows)
    
    ## Display plots sequentially
    grid.arrange(plots_locs)
    # grid.arrange(plots_grid)
    
  }
  
  
  for (m in seq_along(model_names)) { # m <- 4
    
    ## Room for plots
    plot_list <- list()
    plot_list <- list()
    
    ## Model details
    name_model <- model_names[m]
    label_model <- model_labels[m]
    
    for (i in 1:n_comp) { # i <- 1
      
      ## Select representative replicates (min, median, max RMSE)
      indexes <- tapply(
        quantitative_results$rmse[[paste0("A_star", group, "_locs")]][[name_model]],
        quantitative_results$rmse[[paste0("A_star", group, "_locs")]]$Group,
        function(x) {
          sapply(quantile(x, c(0, 0.5, 1), na.rm = TRUE), function(q) which.min(abs(x - q)))
        }
      )
      
      ## Compute limits
      limits_grid <- apply(do.call(rbind, unlist(A_star_locs[[group]], recursive = FALSE)), 2, range)
      
      limits <- range(c(A_true_grid[[group]], limits_grid))
      
      ## True f
      # plot_list[[4*(i-1) + 1]] <- plot.curve_points(
      #   qualitative_results$locations_D, A_true_locs[[group]][, i], size = 1
      # ) + std_plot_settings_curves()
      
      plot_list[[4*(i-1) + 1]] <- plot.curve(
        qualitative_results$grid_D, A_true_grid[[group]][, i]# , LEGEND = FALSE
      ) + std_plot_settings_curves()
      
      ## Reconstructed f for quantile-based replicates
      for (j in 1:3) {
        
        if(length(A_star_grid)==0 || is.null(A_star_grid[[group]][[name_model]][[indexes[[i]][j]]])) {
          plot_list[[4*(i-1) + j + 1]] <- plot.curve_points(
            qualitative_results$locations_D,
            A_star_locs[[group]][[name_model]][[indexes[[i]][j]]][, i],
            true = A_true_locs[[group]][, i],
            size = 1
          ) + std_plot_settings_curves()
        } else {
          plot_list[[4*(i-1) + j + 1]] <- plot.curve(
            qualitative_results$grid_D,
            A_star_grid[[group]][[name_model]][[indexes[[i]][j]]][, i],
            true = A_true_grid[[group]][, i]
          ) + std_plot_settings_curves()
        }
      }
      
    }
    
    ## Arrange labeled grids for each display mode
    plots_locs <- labled_plots_grid(arrangeGrob(grobs = plot_list, ncol = 4), paste("Astar - model:", label_model), labels_cols, labels_rows)
    # plots_grid <- labled_plots_grid(arrangeGrob(grobs = plot_list, ncol = 4), "A at HR grid", labels_cols, labels_rows)
    
    ## Display plots sequentially
    grid.arrange(plots_locs)
    # grid.arrange(plots_grid)
    
  }
}
