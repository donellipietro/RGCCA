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
fit_model <- function(model_name, data, test_options, path_list) {
  switch(model_name,
         fda_splines = return(fda_splines(data, test_options)),
         fdaPDE_splines_E = return(fdaPDE_smoothing(model_name, "splines", "Exact", data, test_options, path_list)),
         fdaPDE_splines_H = return(fdaPDE_smoothing(model_name, "splines", "Hutchinson", data, test_options, path_list)),
         fdaPDE_fem_E = return(fdaPDE_smoothing(model_name, "fem", "Exact", data, test_options, path_list)),
         fdaPDE_fem_H = return(fdaPDE_smoothing(model_name, "fem", "Hutchinson", data, test_options, path_list)),
         fdaPDE_graph_E = return(fdaPDE_smoothing(model_name, "graph", "Exact", data, test_options, path_list)),
         fdaPDE_graph_H = return(fdaPDE_smoothing(model_name, "graph", "Hutchinson", data, test_options, path_list)),
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
fda_splines <- function(data, test_options) {
  
  ## Initialize empty model
  model <- list()
  
  ## Get all the necessary info form data and test_options
  domain <- data$domain
  knots <- domain$knots
  locs <- data$locations
  z <- data$z
  lambda_grid <- test_options$regularization$lambda_grid
  
  # Fit multivariate PCA ----
  start.time <- Sys.time()
  
  ## Create a B-spline basis object
  basisobj <- create.bspline.basis(breaks = knots)
  
  ## Define a function to compute the GCV score and df for a given lambda
  compute_gcv <- function(lambda) {
    fdParobj  <- fdPar(basisobj, Lfdobj = 2, lambda = lambda)
    smooth_fd <- smooth.basis(locs, z, fdParobj)
    gcv_score <- smooth_fd$gcv
    df <- smooth_fd$df
    return(c(gcv = gcv_score, df = df))
  }
  
  if(length(lambda_grid) > 1){ 
    
    ## Compute the GCV score for each lambda
    res <- sapply(lambda_grid, compute_gcv)
    gcv_scores <- res[1, ]
    dfs <- res[2, ]
    
    ## Find the lambda that minimizes the GCV score
    optimal_lambda <- lambda_grid[which.min(gcv_scores)]
    
  } else {
    optimal_lambda <- lambda_grid[1]
    gcv_scores <- NaN
    dfs <- NaN
  }
  
  ## Use the optimal lambda to compute the smoothing spline
  fdParobj_optimal <- fdPar(basisobj, 2, optimal_lambda)
  smooth.fd_optimal <- smooth.basis(locs, z, fdParobj_optimal)
  fd_hat <- smooth.fd_optimal$fd
  
  ## Coefficients of the fitted spline
  f_coeffs <- fd_hat$coefs
  
  ## Values at the original locations
  f_locs <- eval.fd(locs, fd_hat)
  
  ## Values on a dense grid for plotting, etc.
  f_grid <- eval.fd(grid, fd_hat)  
  
  end.time <- Sys.time()
  cat(paste("finished after", end.time - start.time, attr(end.time - start.time, "units"), "\n"))
  
  
  # Save results ----
  model$results$f_coeffs <- f_coeffs
  model$results$f_locs <- f_locs
  model$results$f_grid <- f_grid
  model$results$execution_time <- end.time - start.time
  model$results$lambda <- optimal_lambda
  model$results$gcv_scores <- gcv_scores
  model$results$dfs <- dfs
  
  # Add flags ----
  model$model_traits$is_functional <- TRUE
  model$model_traits$has_interpolator <- TRUE
  model$model_traits$discretization <- "splines"
  
  return(model)
}


## Function: fPCA
# - Args:
#   * data: list containing at least $X and $locations
#   * test_options: configuration list (includes regularization and model parameters)
#   * path_list: list of paths for temporary data, results, and C++ scripts
# - Desc:
#   Fits a fdaPDE model using an external C++ solver. The function prepares
#   all required data and mesh files, writes configuration JSON for the solver,
#   calls the executable, and then reads the resulting outputs. 
#   Returns a structured model object.
fdaPDE_smoothing <- function(model_name, discretization, trace_mode, data, test_options, path_list) { # model_name <- "fdaPDE_splines"
  
  ## Initialize empty model
  model <- list()
  
  ## Get the data
  z <- data$z
  locs <- data$locations
  n_locs <- length(locs)
  lambda_grid <- test_options$regularization$lambda_grid
  
  # Paths ----
  path_cpp_script  <- path_list$cpp_script
  path_batch       <- path_list$batch
  path_tmp_data    <- path_list$tmp_data
  path_mesh        <- paste0(path_list$tmp_data, "mesh/")
  mkdir(path_mesh)
  path_tmp_results <- path_list$tmp_results
  
  # Write data for C++ scripts ----
  
  ## Data matrix and locations ----
  write.csv(format(locs, digits = 16), file = paste0(path_tmp_data, "locs.csv"))
  write.csv(format(grid, digits = 16), file = paste0(path_tmp_data, "grid.csv"))
  write.csv(format(z, digits = 16), file = paste0(path_tmp_data, "z.csv"))
  
  ## Mesh ----
  knots <- domain$knots
  write.csv(format(knots, digits = 16), paste0(path_mesh, "points.csv"))
  
  ## Write JSON arguments for the C++ solver ----
  cpp_script_arguments <- list()
  cpp_script_arguments$path_list <- list(
    mesh = path_mesh,
    data = path_tmp_data,
    results = path_tmp_results
  )
  cpp_script_arguments$options$lambda_grid <- lambda_grid/n_locs
  cpp_script_arguments$options$trace_mode <- trace_mode
  cpp_script_arguments$options$calibration <- ifelse(length(lambda_grid)==1, "none", "gcv")
  
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
  switch (discretization,
          splines = system(paste0("cd ", path_cpp_script, " && ", "./fit_model_splines ", file_name_params), ignore.stdout = IGNORE_CPP_OUTPUT),
          fem = system(paste0("cd ", path_cpp_script, " && ", "./fit_model_fem ", file_name_params), ignore.stdout = IGNORE_CPP_OUTPUT),
          graph = system(paste0("cd ", path_cpp_script, " && ", "./fit_model_graph ", file_name_params), ignore.stdout = IGNORE_CPP_OUTPUT),
  )
  end.time <- Sys.time()
  cat(paste("finished after", end.time - start.time, attr(end.time - start.time, "units"), "\n"))
  
  # Save results ----
  
  ## Load results ----
  model$results$f_coeffs <- as.matrix(read.csv(paste(path_tmp_results, "f_coeffs.csv", sep = "")))
  model$results$f_locs <- as.matrix(read.csv(paste(path_tmp_results, "f_locs.csv", sep = "")))
  if(discretization != "graph") {
    model$results$f_grid <- as.matrix(read.csv(paste(path_tmp_results, "f_grid.csv", sep = "")))
  } else {
    model$results$f_grid <- rep(NULL, length(grid))
  }
  model$results$lambda <- as.matrix(read.csv(paste(path_tmp_results, "lambda.csv", sep = "")))[1,1] * n_locs
  model$results$gcv_scores <- as.matrix(read.csv(paste(path_tmp_results, "gcv_scores.csv", sep = "")))
  model$results$execution_time <- end.time - start.time
  
  # Add flags ----
  model$model_traits$is_functional <- TRUE
  model$model_traits$has_interpolator <- TRUE
  model$model_traits$discretization <- discretization
  
  return(model)
}