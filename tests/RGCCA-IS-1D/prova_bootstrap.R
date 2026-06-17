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

plot.curve_bootstrap <- function(
    locations, f,
    true = NULL,
    fit = NULL,
    w_min = NULL,
    conf_int = NULL,
    limits = NULL,
    LEGEND = FALSE,
    colors_boot = "grey70"
) {
  
  if (is.null(f)) return(ggplot() + theme_void())
  
  # bootstrap curves: sotto a tutto
  M <- as_curve_matrix(locations, f, prefix = "boot")
  data_long <- to_long(locations, M)
  
  plot <- ggplot() +
    geom_line(
      data = data_long,
      aes(x = x, y = y, group = curve),
      color = colors_boot,
      linewidth = 0.25,
      alpha = 0.75
    )
  
  # confidence band opzionale
  if (!is.null(conf_int)) {
    CI <- as.data.frame(conf_int)
    colnames(CI) <- c("lower", "upper")
    CI$x <- locations
    
    plot <- plot +
      geom_ribbon(
        data = CI,
        aes(x = x, ymin = lower, ymax = upper),
        alpha = 0.18,
        fill = "blue",
        inherit.aes = FALSE
      )
  }
  
  # fit sui dati veri: nero spesso
  if (!is.null(fit)) {
    Fm <- as_curve_matrix(locations, fit, prefix = "fit")
    data_fit <- to_long(locations, Fm)
    
    plot <- plot +
      geom_line(
        data = data_fit,
        aes(x = x, y = y),
        color = "black",
        linewidth = 1.0
      )
  }
  
  # w_min: rosso, normalizzato fuori o dentro
  if (!is.null(w_min)) {
    Wm <- as_curve_matrix(locations, w_min, prefix = "w_min")
    data_wmin <- to_long(locations, Wm)
    
    plot <- plot +
      geom_line(
        data = data_wmin,
        aes(x = x, y = y),
        color = "red",
        linewidth = 0.9
      )
  }
  
  # true: verde tratteggiato
  if (!is.null(true)) {
    Tm <- as_curve_matrix(locations, true, prefix = "true")
    data_true <- to_long(locations, Tm)
    
    plot <- plot +
      geom_line(
        data = data_true,
        aes(x = x, y = y),
        linetype = "dashed",
        color = "darkgreen",
        linewidth = 0.9
      )
  }
  
  if (!is.null(limits)) {
    plot <- plot + ylim(limits[1], limits[2])
  }
  
  if (!LEGEND) {
    plot <- plot + theme(legend.position = "none")
  }
  
  plot
}



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
    n_comp = 3,
    init = "Uniform",
    lambda_selection_weights = TRUE,
    n_bootstrap_samples = 5000
  ),
  noise = list(
    sigma_noise = 5
  ),
  regularization = list(
    lambda = 1e-4,
    lambda_grid = list(
      10^(-9:-2), 10^(-9:-2), 10^(-9:-2)
    )
  )
)


## Define and create work directories for the test suite
path_list <- create_paths("prova")
path_list <- update_paths(path_list, "test1", test_options)


data <- generate_data(test_options, seed = 0)

n_comp <- 3

IGNORE_CPP_OUTPUT = FALSE
test_options$model_options$n_comp <- n_comp
test_options$model_options$lambda_selection_weights <- TRUE
results <- fit_model("CPP_fGCCA_cov_FEM", data, path_list, test_options)


par(mfrow = c(n_comp,1))

for(h in 1:n_comp) {
  lambda_grid <- results$results$bootstrap_selection[[h]]$lambda_grid
  lambda_opt <- results$results$bootstrap_selection[[h]]$lambda_opt
  index_lambda_opt <- which(lambda_opt == results$results$bootstrap_selection[[h]]$lambda_grid)
  
  colors <- rep("black", length(lambda_grid))
  pch <- rep(1, length(lambda_grid))
  colors[index_lambda_opt] <- "darkgreen"
  pch[index_lambda_opt] <- 19
  
  
  plot(
    log10(lambda_grid), 
    results$results$bootstrap_selection[[h]]$criterion,
    ylab = "selection criterion", xlab = "log10(lambda)", main = glue("Comp. {h}"),
    type = "b",
    ylim = c(0, max(results$results$bootstrap_selection[[h]]$criterion)),
    col = colors, pch = pch
  )
}

plot_list <- list()
idx <- 1
for(h in 1:n_comp) {
  lambda_opt <- results$results$bootstrap_selection[[h]]$lambda_opt
  index_lambda_opt <- which(lambda_opt == results$results$bootstrap_selection[[h]]$lambda_grid)
  for(g in 1:4) {
    W_boot <- results$results$bootstrap_selection[[h]]$w_boot_grid[[index_lambda_opt]][[g]]
    w_fit  <- results$results$bootstrap_selection[[h]]$w_fit_grid[[index_lambda_opt]][[g]]
    w_min  <- results$results$bootstrap_selection[[h]]$w_min_grid[[index_lambda_opt]][[g]]
    
    w_fit_final <- results$results$A_hat_grid[[g]][, h]
    
    conf_int <- results$results$bootstrap_selection[[h]]$w_ci_grid[[index_lambda_opt]][[g]]
    
    plot_list[[idx]] <- plot.curve_bootstrap(
      data$grid_D,
      W_boot,
      # fit = w_fit,
      fit = w_fit_final,
      w_min = w_min,
      # true = data$A_grid[[g]][, h],
      conf_int = conf_int 
    ) + std_plot_settings_curves()
    idx <- idx +1
  }
}
plot <- arrangeGrob(grobs = plot_list, nrow = n_comp)
plot <- labled_plots_grid(plot, title = "Weights (optimal lambda)",
                          labels_cols = paste("Block", 1:4),
                          labels_rows = paste("Comp", 1:n_comp))
grid.arrange(plot)


## CI for the corr matrix
plot_list <- list()
idx <- 1

for(h in 1:n_comp) {
  
  lambda_opt <- results$results$bootstrap_selection[[h]]$lambda_opt
  index_lambda_opt <- which(lambda_opt == results$results$bootstrap_selection[[h]]$lambda_grid)

  est <- results$corr[[h]]
  min <- results$results$bootstrap_selection[[h]]$corr_min[[index_lambda_opt]]
  low <- results$results$bootstrap_selection[[h]]$corr_ci_low[[index_lambda_opt]]
  upp <- results$results$bootstrap_selection[[h]]$corr_ci_high[[index_lambda_opt]]

  mats <- list(low, est, upp, min)

  for(m in 1:length(mats)) {

    df <- expand.grid(
      x = 1:ncol(mats[[m]]),
      y = 1:nrow(mats[[m]])
    )

    df$value <- as.vector(mats[[m]])

    p <- ggplot(df, aes(x, y, fill = value)) +
      geom_tile() +
      geom_text(aes(label = round(value, 2)), size = 4) +
      scale_fill_gradient2(
        low = "blue", mid = "white", high = "red", midpoint = 0,
        limits = c(-1, 1),
        guide = "none"
      ) +
      scale_y_reverse() +
      coord_fixed() +
      labs(
        x = NULL,
        y = NULL
      ) +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 10),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )

    plot_list[[idx]] <- p
    idx <- idx + 1
  }
}

plot <- arrangeGrob(grobs = plot_list, ncol = 4)
plot <- labled_plots_grid(plot, title = "Correlation matrices CI",
                          labels_cols = c("Lower", "Estimate", "Upper", "Corrected"), # c("True", "Lower", "Estimate", "Upper"),
                          labels_rows = paste("Comp", 1:3))

grid.arrange(plot)
