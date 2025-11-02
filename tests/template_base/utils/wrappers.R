# = ========================================================================== =
# - Script: wrappers.R
# - Desc: Provides model fitting utilities. Includes:
#         * dispatcher for model selection,
#         * implementation of standard multivariate ...
#         * interface to functional ... solvers executed via external C++ code.
# = ========================================================================== =


## Function: fit_model
# - Desc:
#   Dispatches to the appropriate model-fitting routine depending on `model_name`.
fit_model <- function(model_name, domain, data, path_list, test_options) {
  switch(model_name,
         model1 = return(MV(data, test_options)),
         model2 = return(fdaPDE_model(model_name, domain, data, path_list, test_options)),
         ## ....
         {
           stop(paste("The model", model_name, "does not exist"))
         }
  )
}


## Function: MV...
# - Args:
#   * data: list containing at least $X (data matrix)
#   * test_options: list with field $model_options$n_comp (number of components)
# - Desc:
#   Fits a standard multivariate PCA model (via `prcomp`) to the observed data,
#   without spatial structure. Returns loadings, scores, reconstructions, and timing.
MV <- function(data, test_options) {
  
  ## Initialize empty model
  model <- list()
  
  ## Get all the necessary info form data and test_options
  ## ....
  
  # Fit multivariate PCA ----
  start.time <- Sys.time()

  ## ....
  
  end.time <- Sys.time()
  cat(paste("finished after", end.time - start.time, attr(end.time - start.time, "units"), "\n"))
  
  
  # Save results ----
  ## model$results$...
  model$results$execution_time <- end.time - start.time
  
  # Add flags ----
  model$model_traits$is_functional   <- FALSE
  model$model_traits$has_interpolator <- FALSE
  
  return(model)
}


## Function: fPCA
# - Args:
#   * model_name: string identifying the functional model variant
#   * domain: list containing mesh information ($fdapde_mesh)
#   * data: list containing at least $X and $locations
#   * path_list: list of paths for temporary data, results, and C++ scripts
#   * test_options: configuration list (includes regularization and model parameters)
# - Desc:
#   Fits a fdaPDE model using an external C++ solver. The function prepares
#   all required data and mesh files, writes configuration JSON for the solver,
#   calls the executable, and then reads the resulting outputs. 
#   Returns a structured model object.
fdaPDE_model <- function(model_name, domain, data, path_list, test_options) {
  
  ## Initialize empty model
  model <- list()
  
  # Paths ----
  path_cpp_script  <- path_list$cpp_script
  path_batch       <- path_list$batch
  path_tmp_data    <- path_list$tmp_data
  path_mesh        <- paste0(path_list$tmp_data, "mesh/")
  mkdir(path_mesh)
  path_tmp_results <- path_list$tmp_results
  
  # Write data for C++ scripts ----
  
  ## Data matrix and locations ----
  # write.csv(format(..., digits = 16), file = paste0(path_tmp_data, "... .csv"))
  
  ## Mesh ----
  mesh <- domain$fdapde_mesh
  write.csv(format(mesh$nodes, digits = 16), paste0(path_mesh, "points.csv"))
  write.csv(format(mesh$triangles, digits = 16), paste0(path_mesh, "elements.csv"))
  write.csv(format(1 * mesh$nodesmarkers, digits = 16), paste0(path_mesh, "boundary.csv"))
  write.csv(format(mesh$neighbors, digits = 16), paste0(path_mesh, "neigh.csv"))
  write.csv(format(mesh$edges, digits = 16), paste0(path_mesh, "edges.csv"))
  
  ## Write JSON arguments for the C++ solver ----
  cpp_script_arguments <- list()
  cpp_script_arguments$path_list <- list(
    mesh = path_mesh,
    data = path_tmp_data,
    results = path_tmp_results
  )
  cpp_script_arguments$options$solver <- model_name
  cpp_script_arguments$options$lambda_grid <- test_options$regularization$lambda_grid
  ## ....
  
  file_name_params <- paste0(
    test_options$name_test, "_", model_name, "_batch",
    test_options$batch_index, "_params.json"
  )
  
  write_json(
    path = paste0(path_cpp_script, file_name_params),
    cpp_script_arguments,
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = 10
  )
  
  # Run C++ executable ----
  start.time <- Sys.time()
  system(paste0("cd ", path_cpp_script, " && ", "./fit_model ", file_name_params), ignore.stdout = IGNORE_CPP_OUTPUT)
  end.time <- Sys.time()
  cat(paste("finished after", end.time - start.time, attr(end.time - start.time, "units"), "\n"))
  
  # Save results ----
  
  ## Load results ----
  # model$results$... <- as.matrix(read.csv(paste(path_tmp_results, "... .csv", sep = "")))
  model$results$execution_time <- end.time - start.time
  
  # Add flags ----
  model$model_traits$is_functional <- FALSE
  model$model_traits$has_interpolator <- FALSE
  
  return(model)
}