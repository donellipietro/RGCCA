#' Fit all requested models for one batch and save their evaluations.
#'
#' @param path_list Named list of repository, output, queue, and temporary paths.
#' @param data Generated data and truth object.
#' @param domain Domain object associated with generated data.
#' @param batch_index Current simulation batch index.
#' @param test_options Nested option object loaded from JSON.
#' @return The value produced by `fit_and_evaluate_models`.
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
    eval_env <- new.env(parent = emptyenv())
    load(paste0(path_batch, "batch_", batch_index, "_results_evaluation.RData"), envir = eval_env)
    if (exists("results_evaluation", envir = eval_env, inherits = FALSE)) {
      results_evaluation <- eval_env$results_evaluation
    }
  }


  # Fit and Evaluate ----
  for (model_name in test_options$model_names) { # model_name <- test_options$model_names[1]

    ## Initialize empty model
    model <- NULL

    ## File name where the results should be found
    file_model <- paste(path_batch, "batch_", batch_index, "_fitted_model_", model_name, ".RData", sep = "")

    ## Fit the model only if necessary (no fit found or fit is forced)
    if (file.exists(file_model) && !FORCE_FIT) {
      if (FORCE_EVALUATE || is.null(results_evaluation[[model_name]])) {
        cat("- Loading fitted model:", model_name, "... \n")
        model_env <- new.env(parent = emptyenv())
        loaded_names <- load(file_model, envir = model_env)
        model_object_name <- paste0("model_", model_name)
        if (!model_object_name %in% loaded_names) {
          stop(paste("Could not find", model_object_name, "in", file_model))
        }
        model <- get(model_object_name, envir = model_env)
      }
    } else {
      cat("- Fitting model:", model_name, "... ")

      ## Fit the model
      model <- fit_model(model_name, data, path_list, test_options)

      if (is.null(model)) {
        cat("skipped.\n")
        next
      }

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
