# = ========================================================================== =
# - Script: fit_and_evaluate_models.R
# - Desc: Fits one or more models via external C++ tools and evaluates results.
#         Handles data/mesh export, parameter JSON creation, caching of fits,
#         and storage of evaluation outputs for each batch.
# = ========================================================================== =


# - Function: fit_and_evaluate_models
# - Args:
#   * path_list: list of directories (expects $cpp_script, $batch, $tmp_data, $tmp_results)
#   * data: list with fields $X (matrix) and $locations (matrix/data.frame)
#   * domain: list with $fdapde_mesh (fdaPDE mesh)
#   * batch_index: integer, identifier for the current batch
#   * test_options: nested list with $model_names, $regularization, etc.
# - Desc:
#   Exports data/mesh, prepares parameters for C++ executables, runs model
#   fitting if required, optionally evaluates fitted models, and saves results.
fit_and_evaluate_models <- function(path_list, 
                                    data,
                                    domain,
                                    batch_index,
                                    test_options){
  
  # Room for results ----
  results_evaluation <- list()
  
  # Paths ----
  path_batch <- path_list$batch
  
  # Load results if available ----
  ## Reload previously saved evaluation results (if present)
  if (file.exists(paste0(path_batch, "batch_", batch_index, "_results_evaluation.RData"))) {
    load(paste0(path_batch, "batch_", batch_index, "_results_evaluation.RData"))
  }
  
  
  # Fit and Evaluate ----
  for (model_name in test_options$model_names) { # model_name <- test_options$model_names[1]
    
    ## Initialize empty model
    model <- NULL
    
    ## File name where the results should be found
    file_model <- paste(path_batch, "batch_", batch_index, "_fitted_model_", model_name, ".RData", sep = "")
    
    ## Fit the model only if necessary (no fit found or fit is forced)
    if (file.exists(file_model) && !FORCE_FIT) {
      if (FORCE_EVALUATE) {
        cat("- Loading fitted model:", model_name, "... \n")
        load(file_model)
      }
    } else {
      cat("- Fitting model:", model_name, "... ")
      
      ## Fit the model
      model <- fit_model(model_name, domain, data, path_list, test_options)
      
      ## Adjust results
      model <- adjust_results(model, data)
      
      ## Save fitted model
      assign(paste("model_", model_name, sep = ""), model)
      save(
        index_batch = batch_index,
        list = paste("model_", model_name, sep = ""),
        file = file_model
      )
      rm(list = paste("model_", model_name, sep = ""))
    }
    
    if (!is.null(model)) {
      ## Model evaluation ----
      results_evaluation[[model_name]] <- evaluate_results(model, data)
    }
  }
  
  # Save results of the evaluation ----
  save(
    index_batch = batch_index,
    results_evaluation,
    file = paste(path_batch, "batch_", batch_index, "_results_evaluation.RData", sep = "")
  )
  cat(paste("- Batch", batch_index, "completed.\n"))
}