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

plot_qualitative_results <- function(group, quantitative_results, qualitative_results) {
  ## Debugging helpers
  # group <- 1
  # quantitative_results <- loaded_qnt_results
  # qualitative_results  <- loaded_qlt_results
  
  n_comp <- as.numeric(max(quantitative_results$rmse$E1_locs$Group))
  
  ## Get fitted quantities
  A_locs <- qualitative_results$A_locs
  A_grid <- qualitative_results$A_grid
  E_locs <- qualitative_results$E_locs
  E_grid <- qualitative_results$E_grid
  
  ## Get true quantities
  A_true_locs <- qualitative_results$A_true_locs
  A_true_grid <- qualitative_results$A_true_grid
  E_true_locs <- qualitative_results$E_true_locs
  E_true_grid <- qualitative_results$E_true_grid
  
  ## Get infos
  domain_D <- qualitative_results$domain_D
  
  ## Models info
  model_names <- quantitative_results$model_names
  model_labels <- qualitative_results$model_labels
  
  ## Labels for plot grids
  labels_cols <- c("True", "Quantile 0", "Quantile 0.5", "Quantile 1")
  labels_rows <- paste("comp.", 1:n_comp)
  
  ## Generate figures for each model
  for (m in seq_along(model_names)) { # m <- 2
    
    ## Room for plots
    plot_list_locs <- list()
    plot_list_grid <- list()
    
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
      breaks <- seq(limits[1], limits[2], length = 10)
      
      ## True f
      plot_list_locs[[4*(i-1) + 1]] <- plot.curve_points(
        qualitative_results$locations_D, A_true_locs[[group]][, i], size = 1
      ) + std_plot_settings_curves()
      
      # plot_list_grid[[4*(i-1) + 1]] <- plot.curve(
      #   qualitative_results$grid_D, A_true_grid[[group]][, i]# , LEGEND = FALSE
      # ) + std_plot_settings_curves()
      
      ## Reconstructed f for quantile-based replicates
      for (j in 1:3) {
        plot_list_locs[[4*(i-1) + j + 1]] <- plot.curve_points(
          qualitative_results$locations_D,
          A_locs[[group]][[name_model]][[indexes[[i]][j]]][, i],
          true = A_true_locs[[group]][, i],
          size = 1
        ) + std_plot_settings_curves()
        
        # plot_list_grid[[4*(i-1) + j + 1]] <- plot.curve(
        #   qualitative_results$grid_D,
        #   A_grid[[group]][[name_model]][[indexes[[i]][j]]][, i],
        #   true = A_true_grid[[group]][, i],
        #   limits = limits
        # ) + std_plot_settings_curves()
      }
      
    }
    
    ## Arrange labeled grids for each display mode
    plots_locs <- labled_plots_grid(arrangeGrob(grobs = plot_list_locs, ncol = 4), paste("A at location - model:", label_model), labels_cols, labels_rows)
    # plots_grid <- labled_plots_grid(arrangeGrob(grobs = plot_list_grid, ncol = 4), "A at HR grid", labels_cols, labels_rows)
    
    ## Display plots sequentially
    grid.arrange(plots_locs)
    # grid.arrange(plots_grid)
    
    for (i in 1:n_comp) { # i <- 1
      
      ## Select representative replicates (min, median, max RMSE)
      indexes <- tapply(
        quantitative_results$rmse[[paste0("E", group, "_locs")]][[name_model]],
        quantitative_results$rmse[[paste0("E", group, "_locs")]]$Group,
        function(x) {
          sapply(quantile(x, c(0, 0.5, 1), na.rm = TRUE), function(q) which.min(abs(x - q)))
        }
      )
      
      ## Compute limits
      limits_grid <- apply(do.call(rbind, unlist(E_locs[[group]], recursive = FALSE)), 2, range)
      
      limits <- range(c(E_true_grid[[group]], limits_grid))
      breaks <- seq(limits[1], limits[2], length = 10)
      
      ## True f
      plot_list_locs[[4*(i-1) + 1]] <- plot.curve_points(
        qualitative_results$locations_T, E_true_locs[[group]][, i], size = 1
      ) + std_plot_settings_curves()
      
      # plot_list_grid[[4*(i-1) + 1]] <- plot.curve(
      #   qualitative_results$grid_D, E_true_grid[[group]][, i]# , LEGEND = FALSE
      # ) + std_plot_settings_curves()
      
      ## Reconstructed f for quantile-based replicates
      for (j in 1:3) {
        plot_list_locs[[4*(i-1) + j + 1]] <- plot.curve_points(
          qualitative_results$locations_T,
          E_locs[[group]][[name_model]][[indexes[[i]][j]]][, i],
          true = E_true_locs[[group]][, i],
          size = 1
        ) + std_plot_settings_curves()
        
        # plot_list_grid[[4*(i-1) + j + 1]] <- plot.curve(
        #   qualitative_results$grid_T,
        #   E_grid[[group]][[name_model]][[indexes[[i]][j]]][, i],
        #   true = E_true_grid[[group]][, i],
        #   limits = limits
        # ) + std_plot_settings_curves()
      }
      
    }
    
    ## Arrange labeled grids for each display mode
    plots_locs <- labled_plots_grid(arrangeGrob(grobs = plot_list_locs, ncol = 4), paste("E at location - model:", label_model), labels_cols, labels_rows)
    # plots_grid <- labled_plots_grid(arrangeGrob(grobs = plot_list_grid, ncol = 4), "A at HR grid", labels_cols, labels_rows)
    
    ## Display plots sequentially
    grid.arrange(plots_locs)
    # grid.arrange(plots_grid)
    
  }
  
  
  #####
  
  
  # ## Room for plots
  # plot_list_locs <- list()
  # plot_list_grid <- list()
  # 
  # ## Generate figures for each model
  # for (m in seq_along(model_names)) { # m <- 1
  #   
  #   ## Model details
  #   name_model <- model_names[m]
  #   
  #   ## Compute limits
  #   limits_grid <- apply(do.call(rbind, unlist(f_grid, recursive = FALSE)), 2, range)
  #   
  #   limits <- range(c(f_true_grid, limits_grid))
  #   breaks <- seq(limits[1], limits[2], length = 10)
  #   
  #   ## True f
  #   plot_list_locs[[4*(m-1) + 1]] <- plot.curve_points(
  #     qualitative_results$locations, f_true_locs, size = 1# , LEGEND = FALSE
  #   ) + std_plot_settings_curves()
  #   
  #   plot_list_grid[[4*(m-1) + 1]] <- plot.curve(
  #     qualitative_results$grid, f_true_grid# , LEGEND = FALSE
  #   ) + std_plot_settings_curves()
  #   
  #   ## Reconstructed f for quantile-based replicates
  #   for (j in 1:3) {
  #     plot_list_locs[[4*(m-1) + j + 1]] <- plot.curve_points(
  #       qualitative_results$locations,
  #       A_locs[[name_model]],
  #       true = f_true_locs,
  #       size = 1
  #     ) + std_plot_settings_curves()
  #     
  #     plot_list_grid[[4*(m-1) + j + 1]] <- plot.curve(
  #       qualitative_results$grid,
  #       f_grid[[name_model]], 
  #       true = f_true_grid,
  #       limits = limits
  #     ) + std_plot_settings_curves()
  #   }
  #   
  # }
  # 
  # ## Arrange labeled grids for each display mode
  # plots_locs <- labled_plots_grid(arrangeGrob(grobs = plot_list_locs, ncol = 4), "f at location", labels_cols, labels_rows)
  # plots_grid <- labled_plots_grid(arrangeGrob(grobs = plot_list_grid, ncol = 4), "f at HR grid", labels_cols, labels_rows)
  # 
  # ## Display plots sequentially
  # grid.arrange(plots_locs)
  # grid.arrange(plots_grid)
}
