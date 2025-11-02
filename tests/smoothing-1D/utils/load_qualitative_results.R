# = ========================================================================== =
# - Script: load_qualitative_results.R
# - Desc: Loads model outputs from all simulation batches for qualitative analysis.
#         Reconstructs functional principal components (fPCs) at multiple spatial
#         resolutions — knots, observed locations, and a high-resolution grid —
#         for each model and repetition. Also computes true fPCs at the same grid
#         for visual comparison.
# = ========================================================================== =


## Function: load_qualitative_results
# - Args:
#   * test_options: list containing model and simulation parameters, including:
#       - $model_names: vector of model identifiers
#       - $model_labels: vector of model display names
#       - $dimensions$n_knots_grid_grid: number of high-resolution grid points
#       - $test_options$n_reps: number of simulation repetitions (batches)
#   * data: list containing reference quantities and domain info:
#       - $domain: includes $fdapde_mesh and $boundary
#       - $locations: observation points
#       - $f_generator: function generating true fPC f
#       - $f_true, $f_true_locs
#   * path_list: list of paths where batch results and meshes are stored
#       - $results: directory containing batch subfolders
# - Desc:
#   Loads model outputs from each batch (scores, f, f at locations
#   and high-resolution grid) for all models. If available, it also evaluates
#   functional f on a regular grid using the domain mesh. Returns all
#   loaded data organized by model and batch, along with the true f and
#   evaluation domains.
load_qualitative_results <- function(test_options, data, path_list) {
  cat("\nLoading results for qualitative analysis ...\n")
  
  ## Locations and grid
  knots <- data$domain$knots
  locations <- data$locations
  grid <- data$grid
  
  ## Room for solutions
  f_locs <- list()
  f_grid <- list()
  
  ## Load batches
  n_reps <- test_options$test_options$n_reps
  for (batch_index in seq_len(n_reps)) {
    tryCatch(
      {
        path_batch <- paste0(path_list$results, "/", "batch_", batch_index, "/")
        
        for (name_model in test_options$model_names) {
          ## Load model file
          model_file <- paste0(
            path_batch,
            "batch_", batch_index, "_fitted_model_", name_model, ".RData"
          )
          
          load(model_file)
          model <- get(paste0("model_", name_model))
          
          ## Store results
          f_locs[[name_model]][[batch_index]] <- model$results$f_locs
          
          if ("f_grid" %in% names(model$results)) {
            f_grid[[name_model]][[batch_index]] <- model$results$f_grid
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
    f_true_locs = data$f_locs,
    f_true_grid = data$f_grid,
    f_locs = f_locs,
    f_grid = f_grid,
    domain = data$domain,
    knots = knots,
    locations = locations,
    grid = grid
  ))
}
