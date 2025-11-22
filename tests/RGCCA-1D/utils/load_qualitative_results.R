# = ========================================================================== =
# - Script: load_qualitative_results.R
# - Desc: Loads model outputs from all simulation batches for qualitative analysis.
#         Reconstructs functional principal components (fPCs) at multiple spatial
#         resolutions — knots, observed locations, and a high-resolution grid —
#         for each model and repetition. Also computes true fPCs at the same grid
#         for visual comparison.
# = ========================================================================== =

load_qualitative_results <- function(test_options, data, path_list) {
  cat("\nLoading results for qualitative analysis ...\n")
  
  ## Locations and grid
  nodes_D <- data$domain_D$knots
  locations_D <- data$locations_D
  grid_D <- data$grid_D
  nodes_T <- data$domain_T$knots
  locations_T <- data$locations_T
  grid_T <- data$grid_T
  
  ## Room for solutions
  A_locs <- list()
  E_locs <- list()
  A_grid <- list()
  E_grid <- list()
  
  ## Load batches
  n_reps <- test_options$test_options$n_reps
  for (batch_index in seq_len(n_reps)) { # batch_index <- 1
    tryCatch(
      {
        path_batch <- paste0(path_list$results, "/", "batch_", batch_index, "/")
        
        for (name_model in test_options$model_names) { # name_model <- test_options$model_names[5]
          ## Load model file
          model_file <- paste0(
            path_batch,
            "batch_", batch_index, "_fitted_model_", name_model, ".RData"
          )
          
          load(model_file)
          model <- get(paste0("model_", name_model))
          
          ## Store results
          n_groups <- test_options$dimensions$n_groups
          for(g in 1:n_groups) {
            A_locs[[paste0(g)]][[name_model]][[batch_index]] <- model$results$A_hat_locs[[g]]
            E_locs[[paste0(g)]][[name_model]][[batch_index]] <- model$results$E_hat_locs[[g]]
            if ("A_hat_grid" %in% names(model$results)) {
              A_grid[[paste0(g)]][[name_model]][[batch_index]] <- model$results$A_hat_grid[[g]]
            }
            if ("E_hat_grid" %in% names(model$results)) {
              E_grid[[paste0(g)]][[name_model]][[batch_index]] <- model$results$E_hat_grid[[g]]
            }
          }
          

        }
      },
      error = function(e) {
        cat(
          paste(
            "Error in test ", test_options$name_test,
            " - batch ", batch_index, ": ",
            conditionMessage(e), "\n",
            sep = ""
          )
        )
      }
    )
    cat(paste("- Batch", batch_index, "loaded\n"))
  }
  
  ## Return results
  return(list(
    model_names = test_options$model_names,
    model_labels = test_options$model_labels,
    A_true_locs = data$A_locs,
    A_true_grid = data$A_grid,
    E_true_locs = data$E_locs,
    E_true_grid = data$E_grid,
    A_locs = A_locs,
    A_grid = A_grid,
    E_locs = E_locs,
    E_grid = E_grid,
    domain_D = data$domain_D,
    nodes_D = nodes_D,
    locations_D = locations_D,
    grid_D = grid_D,
    domain_T = data$domain_T,
    nodes_T = nodes_T,
    locations_T = locations_T,
    grid_T = grid_T
  ))
}
