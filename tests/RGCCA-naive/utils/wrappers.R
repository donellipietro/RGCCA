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
         R_RGCCA = return(R_RGCCA(data, test_options)),
         CPP_RGCCA = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         # model2 = return(fdaPDE_model(model_name, domain, data, path_list, test_options)),
         ## ....
         {
           stop(paste("The model", model_name, "does not exist"))
         }
  )
}


R_RGCCA <- function(data, test_options) {
  
  ## Initialize empty model
  model <- list()
  
  ## Get all the necessary info form data and test_options
  n_groups <- test_options$dimensions$n_groups
  n_comp <- test_options$model_options$n_comp
  blocks <- data$X
  C <- data$C
  
  # Fit multivariate PCA ----
  start.time <- Sys.time()

  fit_rgcca <- rgcca(
    blocks,
    method      = "rgcca",
    scale_block = FALSE,
    scale       = FALSE,
    bias        = FALSE,
    connection  = C,
    init        = test_options$model_options$init,
    superblock  = FALSE,
    tau         = test_options$model_options$tau,
    ncomp       = n_comp,
    scheme      = test_options$model_options$scheme,
    comp_orth   = TRUE,
    sparsity    = 1,
    verbose     = FALSE,
    quiet       = FALSE,
    tol         = 1e-8
  )
  
  end.time <- Sys.time()
  cat(paste("finished after", end.time - start.time, attr(end.time - start.time, "units"), "\n"))
  
  E_locs <- fit_rgcca$Y
  A_locs <- fit_rgcca$astar
  
  ## Post-process results ----
  X_locs <- list()
  for(g in 1:n_groups) {
    for(h in 1:n_comp) {
      norm <- sqrt(var(E_locs[[g]][, h]))
      norm <- ifelse(norm < 1e-9, 1, norm)
      sign <- 1
      if(mean(A_locs[[g]][, h]) < 0) sign = -1
      E_locs[[g]][, h] <- sign * E_locs[[g]][, h] / norm
      A_locs[[g]][, h] <- sign * A_locs[[g]][, h] / norm
    }
    X_locs[[g]] <- E_locs[[g]] %*% t(A_locs[[g]])
  }
  
  # Save results ----
  model$results$A_hat_locs <- A_locs
  model$results$E_hat_locs <- E_locs
  # model$results$X_hat_locs <- X_locs
  model$results$execution_time <- end.time - start.time
  model$lambdas <- list(rep(0, n_comp), rep(0, n_comp), rep(0, n_comp), rep(0, n_comp))
  
  # Add flags ----
  model$model_traits$is_functional   <- FALSE
  model$model_traits$has_interpolator <- FALSE
  
  return(model)
}


CPP_RGCCA <- function(model_name, data, test_options, path_list) {
  
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
  write.csv(format(data$X[[1]], digits = 16), file = paste0(path_tmp_data, "X1.csv"))
  write.csv(format(data$X[[3]], digits = 16), file = paste0(path_tmp_data, "X2.csv"))
  write.csv(format(data$X[[4]], digits = 16), file = paste0(path_tmp_data, "X3.csv"))
  write.csv(format(data$X[[4]], digits = 16), file = paste0(path_tmp_data, "X4.csv"))
  
  ## Mesh ----
  # mesh <- domain$fdapde_mesh
  # write.csv(format(mesh$nodes, digits = 16), paste0(path_mesh, "points.csv"))
  # write.csv(format(mesh$triangles, digits = 16), paste0(path_mesh, "elements.csv"))
  # write.csv(format(1 * mesh$nodesmarkers, digits = 16), paste0(path_mesh, "boundary.csv"))
  # write.csv(format(mesh$neighbors, digits = 16), paste0(path_mesh, "neigh.csv"))
  # write.csv(format(mesh$edges, digits = 16), paste0(path_mesh, "edges.csv"))
  
  ## Write JSON arguments for the C++ solver ----
  cpp_script_arguments <- list()
  cpp_script_arguments$path_list <- list(
    mesh = path_mesh,
    data = path_tmp_data,
    results = path_tmp_results
  )
  cpp_script_arguments$options$solver <- model_name
  cpp_script_arguments$options$lambda_grid <- test_options$regularization$lambda_grid
  cpp_script_arguments$options$n_obs <- test_options$dimensions$n_nodes_T
  cpp_script_arguments$options$n_comp <- test_options$model_options$n_comp
  cpp_script_arguments$options$sd_noise <- test_options$noise$sigma_noise
  # cpp_script_arguments$options$init <- test_options$model_options$init
  # cpp_script_arguments$options$tau <- test_options$model_options$tau
  # cpp_script_arguments$options$scheme <- test_options$model_options$scheme
  
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
  system(paste0("cd ", path_cpp_script, " && ", "./fit_model_RGCCA ", file_name_params), ignore.stdout = IGNORE_CPP_OUTPUT)
  end.time <- Sys.time()
  cat(paste("finished after", end.time - start.time, attr(end.time - start.time, "units"), "\n"))
  
  # Save results ----
  
  ## Load results ----
  n_groups <- test_options$dimensions$n_groups
  E_hat_locs <- list()
  A_hat_locs <- list()
  for(g in 1:n_groups) {
    E_hat_locs[[g]] <- as.matrix(read.csv(paste(path_tmp_results, "E", g, "_hat_locs.csv", sep = "")))
    A_hat_locs[[g]] <- as.matrix(read.csv(paste(path_tmp_results, "A", g, "_hat_locs.csv", sep = "")))
  }
  
  
  model$results$E_hat_locs <- E_hat_locs
  model$results$A_hat_locs <- A_hat_locs
  model$results$execution_time <- end.time - start.time
  
  # Add flags ----
  model$model_traits$is_functional <- FALSE
  model$model_traits$has_interpolator <- FALSE
  
  return(model)
}