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
fit_model <- function(model_name, data, path_list, test_options) {
  switch(model_name,
         R_GCCA_cor = return(R_RGCCA(model_name, data, test_options)),
         R_RGCCA = return(R_RGCCA(model_name, data, test_options)),
         R_GCCA_cov = return(R_RGCCA(model_name, data, test_options)),
         
         CPP_GCCA_cor = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_RGCCA = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_GCCA_cov = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         
         CPP_GCCA_imp_cor = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_RGCCA_imp = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_GCCA_imp_cov = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         
         CPP_fGCCA_cor = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_fRGCCA = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_fGCCA_cov = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         
         CPP_tfGCCA_cor = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_tfRGCCA = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_tfGCCA_cov = return(CPP_RGCCA(model_name, data, test_options, path_list)),

         ## ....
         {
           stop(paste("The model", model_name, "does not exist"))
         }
  )
}


R_RGCCA <- function(model_name, data, test_options) {
  
  ## Initialize empty model
  model <- list()
  
  ## Get all the necessary info form data and test_options
  n_groups <- test_options$dimensions$n_groups
  n_comp <- test_options$model_options$n_comp
  blocks <- data$X
  C <- data$C
  
  switch (model_name,
          R_GCCA_cor = {tau = 0},
          R_RGCCA = {tau = "optimal"},
          R_GCCA_cov = {tau = 1})
  
  # Fit multivariate PCA ----
  start.time <- Sys.time()
  
  fit_rgcca <- rgcca(
    blocks,
    method      = "rgcca",
    scale_block = FALSE,
    scale       = FALSE,
    bias        = TRUE,
    connection  = C,
    init        = test_options$model_options$init,
    superblock  = FALSE,
    tau         = tau, # test_options$model_options$tau,
    ncomp       = n_comp,
    scheme      = test_options$model_options$scheme,
    comp_orth   = TRUE,
    sparsity    = 1,
    verbose     = !IGNORE_R_OUTPUT,
    quiet       = FALSE,
    tol         = 1e-8
  )
  
  if(!IGNORE_R_OUTPUT) {
    print(fit_rgcca$call$tau)
  }
  
  end.time <- Sys.time()
  cat(paste("finished after", end.time - start.time, attr(end.time - start.time, "units"), "\n"))
  
  E_locs <- fit_rgcca$Y
  A_locs <- fit_rgcca$astar
  
  ## Post-process results ----
  library(clue)
  X_locs <- list()
  for(g in 1:n_groups) {
    for(h in 1:n_comp) {
      norm_E <- var(E_locs[[g]][, h])
      norm_E <- if (is.na(norm_E) || norm_E == 0) 1 else sqrt(norm_E)
      norm_A <- norm_l2(A_locs[[g]][, h])
      norm_A <- if (is.na(norm_A) || norm_A == 0) 1 else norm_A
      sign <- 1
      if(mean(A_locs[[g]][, h]) < 0) sign = -1
      E_locs[[g]][, h] <- sign * E_locs[[g]][, h] / norm_E
      A_locs[[g]][, h] <- sign * A_locs[[g]][, h] / norm_A
    }
    sim <- abs(var(data$A_locs[[g]][,1:n_comp], A_locs[[g]]))
    perm <- solve_LSAP(sim, maximum = TRUE)
    A_locs[[g]] <- A_locs[[g]][, perm, drop = FALSE]
    E_locs[[g]] <- E_locs[[g]][, perm, drop = FALSE]
  }
  
  # Save results ----
  model$results$A_hat_locs <- A_locs
  model$results$E_hat_locs <- E_locs
  # model$results$X_hat_locs <- X_locs
  model$results$execution_time <- end.time - start.time
  model$lambdas <- list(rep(0, n_comp), rep(0, n_comp), rep(0, n_comp), rep(0, n_comp))
  model$objective <- c()
  for(h in 1:n_comp){
    n_last <- length(fit_rgcca$crit[[h]])
    model$objective <- c(model$objective, fit_rgcca$crit[[h]][n_last])
  }
  
  # Add flags ----
  model$model_traits$is_functional   <- FALSE
  model$model_traits$has_interpolator <- FALSE
  
  return(model)
}


CPP_RGCCA <- function(model_name, data, test_options, path_list) {
  
  ## Info
  n_comp <- test_options$model_options$n_comp
  
  switch (model_name,
          CPP_GCCA_cor = {tau = 0},
          CPP_GCCA_imp_cor = {tau = 0},
          CPP_fGCCA_cor = {tau = 0},
          CPP_tfGCCA_cor = {tau = 0},
          
          CPP_RGCCA = {tau = -1},
          CPP_RGCCA_imp = {tau = -1},
          CPP_fRGCCA = {tau = -1},
          CPP_tfRGCCA = {tau = -1},
          
          CPP_GCCA_cov = {tau = 1},
          CPP_GCCA_imp_cov = {tau = 1},
          CPP_fGCCA_cov = {tau = 1},
          CPP_tfGCCA_cov = {tau = 1}
  )
  
  ## Initialize empty model
  model <- list()
  
  # Paths ----
  path_cpp_script  <- path_list$cpp_script
  path_batch       <- path_list$batch
  path_tmp_data    <- path_list$tmp_data
  path_tmp_results <- path_list$tmp_results
  path_tmp_mesh    <- paste0(path_list$tmp_data, "mesh/")
  mkdir(path_tmp_mesh)
  
  # Write data for C++ scripts ----
  
  ## Data matrix and locations ----
  for(g in 1:data$dimensions$n_groups) {
    write.csv(format(data$locations_D[[g]], digits = 16), file = paste0(path_tmp_data, "locs_D_", g, ".csv"))
    write.csv(format(data$grid_D[[g]], digits = 16), file = paste0(path_tmp_data, "grid_D_", g, ".csv"))
  }
  write.csv(format(data$locations_T, digits = 16), file = paste0(path_tmp_data, "locs_T.csv"))
  write.csv(format(data$grid_T, digits = 16), file = paste0(path_tmp_data, "grid_T.csv"))
  write.csv(format(as.matrix(data$X[[1]]), digits = 16), file = paste0(path_tmp_data, "X1.csv"))
  write.csv(format(as.matrix(data$X[[2]]), digits = 16), file = paste0(path_tmp_data, "X2.csv"))
  write.csv(format(as.matrix(data$X[[3]]), digits = 16), file = paste0(path_tmp_data, "X3.csv"))
  write.csv(format(as.matrix(data$X[[4]]), digits = 16), file = paste0(path_tmp_data, "X4.csv"))
  
  ## Mesh ----
  for(g in 1:data$dimensions$n_groups) {
    write.csv(format(data$domain_D[[g]]$nodes, digits = 16), paste0(path_tmp_mesh, "points_D_", g, ".csv"))
    write.csv(format(data$domain_D[[g]]$triangles, digits = 16), paste0(path_tmp_mesh, "elements_D_", g, ".csv"))
    write.csv(format(data$domain_D[[g]]$nodesmarkers, digits = 16), paste0(path_tmp_mesh, "boundary_D_", g, ".csv"))
  }
  write.csv(format(data$domain_T$knots, digits = 16), paste0(path_tmp_mesh, "knots_T.csv"))
  
  ## Write JSON arguments for the C++ solver ----
  cpp_script_arguments <- list()
  cpp_script_arguments$path_list <- list(
    mesh = path_tmp_mesh,
    data = path_tmp_data,
    results = path_tmp_results
  )
  cpp_script_arguments$options$solver <- model_name
  cpp_script_arguments$options$lambda_grid <- test_options$regularization$lambda_grid
  cpp_script_arguments$options$n_obs <- data$dimensions$n_locs_T
  cpp_script_arguments$options$n_comp <- test_options$model_options$n_comp
  
  if(model_name %in% c("CPP_GCCA_cor", "CPP_RGCCA", "CPP_GCCA_cov")) {
    cpp_script_arguments$options$sd_noise <- -1
  } else if(test_options$noise$sigma_noise <= 1e-6) {
    cpp_script_arguments$options$sd_noise <- 1e-6
  } else {
    cpp_script_arguments$options$sd_noise <- test_options$noise$sigma_noise
  }
  cpp_script_arguments$options$tau <- tau
  # cpp_script_arguments$options$init <- test_options$model_options$init
  # cpp_script_arguments$options$scheme <- test_options$model_options$scheme
  
  file_name_params <- paste0(
    test_options$name_test, "_", model_name, "_batch_",
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
  grid_D <- grid_T <- FALSE
  switch (model_name,
          
          CPP_GCCA_cor = system(
            paste0("cd ", path_cpp_script, " && ", "./fit_model_RGCCA ", file_name_params), 
            ignore.stdout = IGNORE_CPP_OUTPUT),
          CPP_RGCCA = system(
            paste0("cd ", path_cpp_script, " && ", "./fit_model_RGCCA ", file_name_params), 
            ignore.stdout = IGNORE_CPP_OUTPUT),
          CPP_GCCA_cov = system(
            paste0("cd ", path_cpp_script, " && ", "./fit_model_RGCCA ", file_name_params), 
            ignore.stdout = IGNORE_CPP_OUTPUT),
          
          CPP_GCCA_imp_cor = system(
            paste0("cd ", path_cpp_script, " && ", "./fit_model_RGCCA ", file_name_params), 
            ignore.stdout = IGNORE_CPP_OUTPUT),
          CPP_RGCCA_imp = system(
            paste0("cd ", path_cpp_script, " && ", "./fit_model_RGCCA ", file_name_params), 
            ignore.stdout = IGNORE_CPP_OUTPUT),
          CPP_GCCA_imp_cov = system(
            paste0("cd ", path_cpp_script, " && ", "./fit_model_RGCCA ", file_name_params), 
            ignore.stdout = IGNORE_CPP_OUTPUT),
          
          CPP_fGCCA_cor = { 
            system(
              paste0("cd ", path_cpp_script, " && ", "./fit_model_fRGCCA ", file_name_params), 
              ignore.stdout = IGNORE_CPP_OUTPUT
            )
            grid_D <- TRUE
          },
          CPP_fRGCCA = { 
            system(
              paste0("cd ", path_cpp_script, " && ", "./fit_model_fRGCCA ", file_name_params), 
              ignore.stdout = IGNORE_CPP_OUTPUT
            )
            grid_D <- TRUE
          },
          CPP_fGCCA_cov = { 
            system(
              paste0("cd ", path_cpp_script, " && ", "./fit_model_fRGCCA ", file_name_params), 
              ignore.stdout = IGNORE_CPP_OUTPUT
            )
            grid_D <- TRUE
          },
          
          CPP_tfGCCA_cor = { 
            system(
              paste0("cd ", path_cpp_script, " && ", "./fit_model_tfRGCCA ", file_name_params), 
              ignore.stdout = IGNORE_CPP_OUTPUT
            )
            grid_T <- TRUE
            grid_D <- TRUE
          },
          CPP_tfRGCCA = { 
            system(
              paste0("cd ", path_cpp_script, " && ", "./fit_model_tfRGCCA ", file_name_params), 
              ignore.stdout = IGNORE_CPP_OUTPUT
            )
            grid_T <- TRUE
            grid_D <- TRUE
          },
          CPP_tfGCCA_cov = { 
            system(
              paste0("cd ", path_cpp_script, " && ", "./fit_model_tfRGCCA ", file_name_params), 
              ignore.stdout = IGNORE_CPP_OUTPUT
            )
            grid_T <- TRUE
            grid_D <- TRUE
          }
  )
  
  
  end.time <- Sys.time()
  cat(paste("finished after", end.time - start.time, attr(end.time - start.time, "units"), "\n"))
  
  # Save results ----
  
  ## Load results ----
  n_groups <- test_options$dimensions$n_groups
  E_locs <- list()
  E_grid <- list()
  A_locs <- list()
  A_grid <- list()
  for(g in 1:n_groups) {
    E_locs[[g]] <- as.matrix(read.csv(paste(path_tmp_results, "E", g, "_hat_locs.csv", sep = "")))
    A_locs[[g]] <- as.matrix(read.csv(paste(path_tmp_results, "A", g, "_hat_locs.csv", sep = "")))
    if(grid_D) A_grid[[g]] <- as.matrix(read.csv(paste(path_tmp_results, "A", g, "_hat_grid.csv", sep = "")))
    if(grid_T) E_grid[[g]] <- as.matrix(read.csv(paste(path_tmp_results, "E", g, "_hat_grid.csv", sep = "")))
    for(h in 1:n_comp) {
      norm_E <- var(E_locs[[g]][, h])
      norm_E <- if (is.na(norm_E) || norm_E == 0) 1 else sqrt(norm_E)
      norm_A <- norm_l2(A_locs[[g]][, h])
      norm_A <- if (is.na(norm_A) || norm_A == 0) 1 else norm_A
      sign <- 1
      if(mean(A_locs[[g]][, h]) < 0) sign = -1
      E_locs[[g]][, h] <- sign * E_locs[[g]][, h] / norm_E
      A_locs[[g]][, h] <- sign * A_locs[[g]][, h] / norm_A
      if(grid_T) E_grid[[g]][, h] <- sign * E_grid[[g]][, h] / norm_E
      if(grid_D) A_grid[[g]][, h] <- sign * A_grid[[g]][, h] / norm_A
    }
    sim <- abs(var(data$A_locs[[g]][,1:n_comp], A_locs[[g]]))
    perm <- solve_LSAP(sim, maximum = TRUE)
    A_locs[[g]] <- A_locs[[g]][, perm, drop = FALSE]  
    if(grid_D) A_grid[[g]] <- A_grid[[g]][, perm, drop = FALSE]  
    E_locs[[g]] <- E_locs[[g]][, perm, drop = FALSE]
    if(grid_T) E_grid[[g]] <- E_grid[[g]][, perm, drop = FALSE]
  }
  
  ## Save results ----
  model$results$E_hat_locs <- E_locs
  model$results$A_hat_locs <- A_locs
  if(grid_D) model$results$A_hat_grid <- A_grid
  if(grid_T) model$results$E_hat_grid <- E_grid
  model$results$execution_time <- end.time - start.time
  
  # Add flags ----
  model$model_traits$is_functional <- FALSE
  model$model_traits$has_interpolator <- FALSE
  
  for(h in 1:n_comp){
    objective_h <- as.matrix(read.csv(paste(path_tmp_results, "objective", h, ".csv", sep = "")))
    n_last <- length(objective_h)
    model$objective <- c(model$objective, objective_h[n_last])
  }
  
  return(model)
}