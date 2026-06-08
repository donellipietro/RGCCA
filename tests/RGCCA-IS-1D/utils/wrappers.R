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
         
         R_FGCCA_cor = return(R_FGCCA(model_name, data, test_options)),
         R_FGCCA_cov = return(R_FGCCA(model_name, data, test_options)),
         
         CPP_GCCA_cor = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_RGCCA = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_GCCA_cov = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         
         CPP_GCCA_NN_cor = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_RGCCA_NN = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_GCCA_NN_cov = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         
         CPP_fGCCA_cor_FEM = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_fRGCCA_FEM = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_fGCCA_cov_FEM = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         
         CPP_fGCCA_NN_cor_FEM = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_fRGCCA_NN_FEM = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_fGCCA_NN_cov_FEM = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         
         CPP_fGCCA_cor_SPLINES = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_fRGCCA_SPLINES = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_fGCCA_cov_SPLINES = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         
         CPP_fGCCA_NN_cor_SPLINES = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_fRGCCA_NN_SPLINES = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         CPP_fGCCA_NN_cov_SPLINES = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         
         CPP_tfRGCCA = return(CPP_RGCCA(model_name, data, test_options, path_list)),
         R_FGCCA_cov = return(R_FGCCA(model_name, data, test_options)),
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
    bias        = TRUE, test_options$model_options$bias,
    connection  = C,
    init        = "svd",
    superblock  = FALSE,
    tau         = tau,
    ncomp       = n_comp,
    scheme      = "factorial",
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
  
  H <- fit_rgcca$Y
  A_locs <- fit_rgcca$a
  A_star_locs <- fit_rgcca$astar
  
  ## Post-process results ----
  library(clue)
  X_locs <- list()
  for(g in 1:n_groups) {
    sim <- abs(var(data$A_locs[[g]][,1:n_comp], A_locs[[g]]))
    perm <- solve_LSAP(sim, maximum = TRUE)
    A_locs[[g]] <- A_locs[[g]][, perm, drop = FALSE]
    A_star_locs[[g]] <- A_star_locs[[g]][, perm, drop = FALSE]
    H[[g]] <- H[[g]][, perm, drop = FALSE]
    for(h in 1:n_comp) {
      norm_H <- var(H[[g]][, h])
      norm_H <- if (is.na(norm_H) || norm_H == 0) 1 else sqrt(norm_H)
      norm_A <- norm_l2(A_locs[[g]][, h])
      norm_A <- if (is.na(norm_A) || norm_A == 0) 1 else norm_A
      norm_A_star <- norm_l2(A_star_locs[[g]][, h])
      norm_A_star <- if (is.na(norm_A_star) || norm_A_star == 0) 1 else norm_A_star
      sign <- sign(cov(A_locs[[g]][, h], data$A_locs[[g]][, h]))
      if(sign == 0) sign <- 1
      H[[g]][, h] <- sign * H[[g]][, h] / norm_H
      A_locs[[g]][, h] <- sign * A_locs[[g]][, h] / norm_A
      A_star_locs[[g]][, h] <- sign * A_star_locs[[g]][, h] / norm_A_star
    }
  }
  
  # Save results ----
  model$results$A_hat_locs <- A_locs
  model$results$A_star_hat_locs <- A_star_locs
  model$results$H_hat <- H
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
  
  lambda <- test_options$regularization$lambda
  lambda_grid <- test_options$regularization$lambda_grid
  if(model_name %in% c("CPP_GCCA_cor", "CPP_RGCCA", "CPP_GCCA_cov", "CPP_GCCA_NN_cor", "CPP_RGCCA_NN", "CPP_GCCA_NN_cov")){
    if(!is.na(lambda) && lambda != 0) { return() }
  } else {
    if(!is.na(lambda) && lambda == 0) { return() }
  }
  
  
  non_negative_weights = FALSE
  switch (model_name,
          CPP_GCCA_cor = {tau = 0},
          CPP_fGCCA_cor_FEM = {tau = 0},
          CPP_fGCCA_cor_SPLINES = {tau = 0},
          CPP_GCCA_NN_cor = {tau = 0; non_negative_weights = TRUE},
          CPP_fGCCA_NN_cor_FEM = {tau = 0; non_negative_weights = TRUE},
          CPP_fGCCA_NN_cor_SPLINES = {tau = 0; non_negative_weights = TRUE},
          
          CPP_RGCCA = {tau = -1},
          CPP_fRGCCA_FEM = {tau = -1},
          CPP_fRGCCA_SPLINES = {tau = -1},
          CPP_RGCCA_NN = {tau = -1; non_negative_weights = TRUE},
          CPP_fRGCCA_NN_FEM = {tau = -1; non_negative_weights = TRUE},
          CPP_fRGCCA_NN_SPLINES = {tau = -1; non_negative_weights = TRUE},
          
          CPP_GCCA_cov = {tau = 1},
          CPP_fGCCA_cov_FEM = {tau = 1},
          CPP_fGCCA_cov_SPLINES = {tau = 1},
          CPP_GCCA_NN_cov = {tau = 1; non_negative_weights = TRUE},
          CPP_fGCCA_NN_cov_FEM = {tau = 1; non_negative_weights = TRUE},
          CPP_fGCCA_NN_cov_SPLINES = {tau = 1; non_negative_weights = TRUE},
  )
  
  ## Initialize empty model
  model <- list()
  
  # Paths ----
  path_cpp_script  <- path_list$cpp_script
  path_batch       <- path_list$batch
  path_tmp_data    <- path_list$tmp_data
  path_tmp_results <- path_list$tmp_results
  # path_tmp_data    <- paste0(path_list$tmp_data, model_name, "/")
  # path_tmp_results <- paste0(path_list$tmp_results, model_name, "/")
  # mkdir(path_tmp_data, path_tmp_results)
  path_tmp_mesh    <- paste0(path_list$tmp_data, "mesh/")
  mkdir(path_tmp_mesh)
  
  # Write data for C++ scripts ----
  
  ## Data matrix and locations ----
  write.csv(format(data$locations_D, digits = 16), file = paste0(path_tmp_data, "locs_D.csv"))
  write.csv(format(data$grid_D, digits = 16), file = paste0(path_tmp_data, "grid_D.csv"))
  write.csv(format(data$X[[1]], digits = 16), file = paste0(path_tmp_data, "X1.csv"))
  write.csv(format(data$X[[2]], digits = 16), file = paste0(path_tmp_data, "X2.csv"))
  write.csv(format(data$X[[3]], digits = 16), file = paste0(path_tmp_data, "X3.csv"))
  write.csv(format(data$X[[4]], digits = 16), file = paste0(path_tmp_data, "X4.csv"))
  
  ## Mesh ----
  write.csv(format(data$domain_D$knots, digits = 16), paste0(path_tmp_mesh, "knots_D.csv"))
  
  ## Write JSON arguments for the C++ solver ----
  lambda <- test_options$regularization$lambda
  lambda_selection_weights <- test_options$model_options$lambda_selection_weights
  cpp_script_arguments <- list()
  cpp_script_arguments$path_list <- list(
    mesh = path_tmp_mesh,
    data = path_tmp_data,
    results = path_tmp_results
  )
  cpp_script_arguments$options$solver <- model_name
  cpp_script_arguments$options$lambda <- ifelse(lambda < 0, 1e-12, lambda)
  cpp_script_arguments$options$n_obs <- data$dimensions$n
  cpp_script_arguments$options$n_comp <- test_options$model_options$n_comp
  cpp_script_arguments$options$non_negative_weights <- non_negative_weights
  cpp_script_arguments$options$tau <- tau
  lambda_selection_weights <- ifelse(lambda < 0 || lambda_selection_weights, TRUE, FALSE)
  cpp_script_arguments$options$lambda_selection_weights <- lambda_selection_weights
  cpp_script_arguments$options$n_bootstrap_samples <- test_options$model_options$n_bootstrap_samples
  cpp_script_arguments$options$lambda_grid <- lambda_grid
  
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
  Sys.unsetenv("DYLD_LIBRARY_PATH")
  Sys.setenv(
    OMP_NUM_THREADS = "1",
    OPENBLAS_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1"
  )
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
          CPP_GCCA_NN_cor = system(
            paste0("cd ", path_cpp_script, " && ", "./fit_model_RGCCA ", file_name_params),
            ignore.stdout = IGNORE_CPP_OUTPUT),
          CPP_RGCCA_NN = system(
            paste0("cd ", path_cpp_script, " && ", "./fit_model_RGCCA ", file_name_params),
            ignore.stdout = IGNORE_CPP_OUTPUT),
          CPP_GCCA_NN_cov = system(
            paste0("cd ", path_cpp_script, " && ", "./fit_model_RGCCA ", file_name_params),
            ignore.stdout = IGNORE_CPP_OUTPUT),
          
          CPP_fGCCA_cor_FEM = {
            system(
              paste0("cd ", path_cpp_script, " && ", "./fit_model_fRGCCA_fem ", file_name_params),
              ignore.stdout = IGNORE_CPP_OUTPUT
            )
            grid_D <- TRUE
          },
          CPP_fRGCCA_FEM = {
            system(
              paste0("cd ", path_cpp_script, " && ", "./fit_model_fRGCCA_fem ", file_name_params),
              ignore.stdout = IGNORE_CPP_OUTPUT
            )
            grid_D <- TRUE
          },
          CPP_fGCCA_cov_FEM = {
            system(
              paste0("cd ", path_cpp_script, " && ", "./fit_model_fRGCCA_fem ", file_name_params),
              ignore.stdout = IGNORE_CPP_OUTPUT
            )
            grid_D <- TRUE
          },
          CPP_fGCCA_NN_cor_FEM = {
            system(
              paste0("cd ", path_cpp_script, " && ", "./fit_model_fRGCCA_fem ", file_name_params),
              ignore.stdout = IGNORE_CPP_OUTPUT
            )
            grid_D <- TRUE
          },
          CPP_fRGCCA_NN_FEM = {
            system(
              paste0("cd ", path_cpp_script, " && ", "./fit_model_fRGCCA_fem ", file_name_params),
              ignore.stdout = IGNORE_CPP_OUTPUT
            )
            grid_D <- TRUE
          },
          CPP_fGCCA_NN_cov_FEM = {
            system(
              paste0("cd ", path_cpp_script, " && ", "./fit_model_fRGCCA_fem ", file_name_params),
              ignore.stdout = IGNORE_CPP_OUTPUT
            )
            grid_D <- TRUE
          },
          
          
          CPP_fGCCA_cor_SPLINES = {
            system(
              paste0("cd ", path_cpp_script, " && ", "./fit_model_fRGCCA_splines ", file_name_params),
              ignore.stdout = IGNORE_CPP_OUTPUT
            )
            grid_D <- TRUE
          },
          CPP_fRGCCA_SPLINES = {
            system(
              paste0("cd ", path_cpp_script, " && ", "./fit_model_fRGCCA_splines ", file_name_params),
              ignore.stdout = IGNORE_CPP_OUTPUT
            )
            grid_D <- TRUE
          },
          CPP_fGCCA_cov_SPLINES = {
            system(
              paste0("cd ", path_cpp_script, " && ", "./fit_model_fRGCCA_splines ", file_name_params),
              ignore.stdout = IGNORE_CPP_OUTPUT
            )
            grid_D <- TRUE
          },
          CPP_fGCCA_NN_cor_SPLINES = {
            system(
              paste0("cd ", path_cpp_script, " && ", "./fit_model_fRGCCA_splines ", file_name_params),
              ignore.stdout = IGNORE_CPP_OUTPUT
            )
            grid_D <- TRUE
          },
          CPP_fRGCCA_NN_SPLINES = {
            system(
              paste0("cd ", path_cpp_script, " && ", "./fit_model_fRGCCA_splines ", file_name_params),
              ignore.stdout = IGNORE_CPP_OUTPUT
            )
            grid_D <- TRUE
          },
          CPP_fGCCA_NN_cov_SPLINES = {
            system(
              paste0("cd ", path_cpp_script, " && ", "./fit_model_fRGCCA_splines ", file_name_params),
              ignore.stdout = IGNORE_CPP_OUTPUT
            )
            grid_D <- TRUE
          }
  )
  
  
  end.time <- Sys.time()
  cat(paste("finished after", end.time - start.time, attr(end.time - start.time, "units"), "\n"))
  
  # Save results ----
  
  ## Load results ----
  n_groups <- test_options$dimensions$n_groups
  H <- list()
  E_grid <- list()
  A_locs <- list()
  A_star_locs <- list()
  A_grid <- list()
  A_star_grid <- list()
  for(g in 1:n_groups) {
    H[[g]] <- as.matrix(read.csv(paste(path_tmp_results, "E", g, "_hat_locs.csv", sep = "")))
    A_locs[[g]] <- as.matrix(read.csv(paste(path_tmp_results, "A", g, "_hat_locs.csv", sep = "")))
    A_star_locs[[g]] <- as.matrix(read.csv(paste(path_tmp_results, "A_star", g, "_hat_locs.csv", sep = "")))
    if(grid_D) A_grid[[g]] <- as.matrix(read.csv(paste(path_tmp_results, "A", g, "_hat_grid.csv", sep = "")))
    if(grid_D) A_star_grid[[g]] <- as.matrix(read.csv(paste(path_tmp_results, "A_star", g, "_hat_grid.csv", sep = "")))
    
    sim <- abs(var(data$A_locs[[g]][,1:n_comp], A_locs[[g]]))
    perm <- solve_LSAP(sim, maximum = TRUE)
    A_locs[[g]] <- A_locs[[g]][, perm, drop = FALSE]  
    A_star_locs[[g]] <- A_star_locs[[g]][, perm, drop = FALSE]  
    if(grid_D) A_grid[[g]] <- A_grid[[g]][, perm, drop = FALSE]  
    if(grid_D) A_star_grid[[g]] <- A_star_grid[[g]][, perm, drop = FALSE]  
    H[[g]] <- H[[g]][, perm, drop = FALSE]
    
    for(h in 1:n_comp) {
      norm_H <- var(H[[g]][, h])
      norm_H <- if (is.na(norm_H) || norm_H == 0) 1 else sqrt(norm_H)
      norm_A <- norm_l2(A_locs[[g]][, h])
      norm_A <- if (is.na(norm_A) || norm_A == 0) 1 else norm_A
      norm_A_star <- norm_l2(A_star_locs[[g]][, h])
      norm_A_star <- if (is.na(norm_A_star) || norm_A_star == 0) 1 else norm_A_star
      sign <- sign(cov(A_locs[[g]][, h], data$A_locs[[g]][, h]))
      if(sign == 0) sign <- 1
      H[[g]][, h] <- sign * H[[g]][, h] / norm_H
      A_locs[[g]][, h] <- sign * A_locs[[g]][, h] / norm_A
      A_star_locs[[g]][, h] <- sign * A_star_locs[[g]][, h] / norm_A_star
      if(grid_D) A_grid[[g]][, h] <- sign * A_grid[[g]][, h] / norm_A
      if(grid_D) A_star_grid[[g]][, h] <- sign * A_star_grid[[g]][, h] / norm_A_star
    }
  }
  
  ## Save results ----
  model$results$H_hat <- H
  model$results$A_hat_locs <- A_locs
  model$results$A_star_hat_locs <- A_star_locs
  if(grid_D) model$results$A_hat_grid <- A_grid
  if(grid_D) model$results$A_star_hat_grid <- A_star_grid
  model$results$execution_time <- end.time - start.time
  
  # Add flags ----
  model$model_traits$is_functional <- FALSE
  model$model_traits$has_interpolator <- FALSE
  
  for(h in 1:n_comp){
    objective_h <- as.matrix(read.csv(paste(path_tmp_results, "objective", h, ".csv", sep = "")))
    n_last <- length(objective_h)
    model$objective <- c(model$objective, objective_h[n_last])
  }
  
  ## Load bootstrap selection results ----
  if(lambda_selection_weights) {
    bootstrap <- vector("list", n_comp)
    
    for(h in 1:n_comp) {
      lambda_grid <- as.numeric(read.csv(
        paste0(path_tmp_results, "bootstrap_lambda_grid", h, ".csv")
      )[, 1])
      
      criterion <- as.numeric(read.csv(
        paste0(path_tmp_results, "bootstrap_criterion", h, ".csv")
      )[, 1])
      
      lambda_opt <- as.numeric(read.csv(
        paste0(path_tmp_results, "bootstrap_lambda_opt", h, ".csv")
      )[1, 1])
      
      w_fit_locs  <- vector("list", length(lambda_grid))
      w_fit_grid  <- vector("list", length(lambda_grid))
      w_boot_locs <- vector("list", length(lambda_grid))
      w_boot_grid <- vector("list", length(lambda_grid))
      w_min_locs  <- vector("list", length(lambda_grid))
      w_min_grid  <- vector("list", length(lambda_grid))
      
      for(i in seq_along(lambda_grid)) {
        if(criterion[i]>=0) {
          w_fit_locs[[i]]  <- vector("list", n_groups)
          w_fit_grid[[i]]  <- vector("list", n_groups)
          w_boot_locs[[i]] <- vector("list", n_groups)
          w_boot_grid[[i]] <- vector("list", n_groups)
          w_min_locs[[i]]  <- vector("list", n_groups)
          w_min_grid[[i]]  <- vector("list", n_groups)
          
          for(g in 1:n_groups) {
            for(type in c("fit", "boot", "wmin")) {
              W_locs <- as.matrix(read.csv(
                paste0(path_tmp_results,
                       "bootstrap_weights_", type,
                       "_comp", h,
                       "_lambda", i,
                       "_block", g,
                       "_locs.csv")
              ))
              
              W_grid <- as.matrix(read.csv(
                paste0(path_tmp_results,
                       "bootstrap_weights_", type,
                       "_comp", h,
                       "_lambda", i,
                       "_block", g,
                       "_grid.csv")
              ))
              
              ## normalize column-wise using location norm
              if(type != "wmin") {
                norms <- apply(W_locs, 2, norm_l2)
                norms[is.na(norms) | norms == 0] <- 1
                
                W_locs <- sweep(W_locs, 2, norms, "/")
                W_grid <- sweep(W_grid, 2, norms, "/")
              }
              
              if(type == "fit") {
                w_fit_locs[[i]][[g]] <- W_locs
                w_fit_grid[[i]][[g]] <- W_grid
              } else if(type == "boot") {
                w_boot_locs[[i]][[g]] <- W_locs
                w_boot_grid[[i]][[g]] <- W_grid
              } else if(type == "wmin") {
                w_min_locs[[i]][[g]] <- W_locs
                w_min_grid[[i]][[g]] <- W_grid
              }
            }
          }
        }
      }
      
      bootstrap[[h]] <- list(
        lambda_grid = lambda_grid,
        criterion = criterion,
        lambda_opt = lambda_opt,
        w_fit_locs = w_fit_locs,
        w_fit_grid = w_fit_grid,
        w_boot_locs = w_boot_locs,
        w_boot_grid = w_boot_grid,
        w_min_locs = w_min_locs,
        w_min_grid = w_min_grid
      )
    }
    
    model$results$bootstrap_selection <- bootstrap
  }
  
  return(model)
}



R_FGCCA <- function(model_name, data, test_options) {
  
  ## Initialize empty model
  model <- list()
  
  ## Get all the necessary info form data and test_options
  n_groups <- test_options$dimensions$n_groups
  n_comp <- test_options$model_options$n_comp
  blocks <- data$X
  C <- data$C
  
  switch (model_name,
          R_FGCCA_cor = {tau = 0},
          R_FGCCA_cov = {tau = 1})
  
  ## Data preparation
  Lys <- list()
  Lts <- list()
  for(g in 1:4){ 
    Lys[[g]] <- lapply(seq_len(nrow(data$X[[g]])), function(i) data$X[[g]][i,])
    Lts[[g]] <- lapply(seq_len(nrow(data$X[[g]])), function(i) data$locations_D) 
  }
  
  # Fit multivariate PCA ----
  start.time <- Sys.time()
  fit_fgcca <- fgcca(
    Lys,
    Lts,
    connection = data$C,
    tau = rep(tau, n_groups),
    scheme = "factorial",
    init = "svd",
    tol = 1e-8,
    deflType = "uncor",
    verbose = !IGNORE_R_OUTPUT,
    center = TRUE,
    scale = FALSE,
    ncomp = rep(3, n_groups)# ,
    # optns = list(gridSize = rep(data$dimensions$n_locs_D, J))
  )
  
  end.time <- Sys.time()
  cat(paste("finished after", end.time - start.time, attr(end.time - start.time, "units"), "\n"))
  
  H <- fit_fgcca$Y
  A_cg <- fit_fgcca$astar
  cg <- fit_fgcca$grids[[1]]
  
  n_locs <- length(data$locations_D)
  n_grid <- length(data$grid_D)
  
  A_locs <- list()
  A_grid <- list()
  for(g in 1:n_groups) {
    A_locs[[g]] <- matrix(0, n_locs, n_comp)
    A_grid[[g]] <- matrix(0, n_grid, n_comp)
    for (h in 1:n_comp) {
      A_locs[[g]][, h] <- approx(cg, A_cg[[g]][, h], xout = data$locations_D, rule = 2)$y
      A_grid[[g]][, h] <- approx(cg, A_cg[[g]][, h], xout = data$grid_D, rule = 2)$y
    }
  }
  
  n_groups <- test_options$dimensions$n_groups
  for(g in 1:n_groups) {
    for(h in 1:n_comp) {
      norm_H <- var(H[[g]][, h])
      norm_H <- if (is.na(norm_H) || norm_H == 0) 1 else sqrt(norm_H)
      norm_A <- norm_l2(A_locs[[g]][, h])
      norm_A <- if (is.na(norm_A) || norm_A == 0) 1 else norm_A
      sign <- 1
      if(mean(A_locs[[g]][, h]) < 0) sign = -1
      H[[g]][, h] <- sign * H[[g]][, h] / norm_H
      A_locs[[g]][, h] <- sign * A_locs[[g]][, h] / norm_A
      A_grid[[g]][, h] <- sign * A_grid[[g]][, h] / norm_A
    }
    sim <- abs(var(data$A_locs[[g]][,1:n_comp], A_locs[[g]]))
    perm <- solve_LSAP(sim, maximum = TRUE)
    A_locs[[g]] <- A_locs[[g]][, perm, drop = FALSE]  
    A_grid[[g]] <- A_grid[[g]][, perm, drop = FALSE]  
    H[[g]] <- H[[g]][, perm, drop = FALSE]
  }
  
  ## Save results ----
  model$results$H_hat <- H
  model$results$A_hat_locs <- A_locs
  model$results$A_hat_grid <- A_grid
  model$results$execution_time <- end.time - start.time
  model$lambdas <- list(rep(0, n_comp), rep(0, n_comp), rep(0, n_comp), rep(0, n_comp))
  model$objective <- c()
  for(h in 1:n_comp){
    n_last <- length(fit_fgcca$crit[[h]])
    model$objective <- c(model$objective, fit_fgcca$crit[[h]][n_last])
  }
  
  # Add flags ----
  model$model_traits$is_functional   <- FALSE
  model$model_traits$has_interpolator <- FALSE
  
  return(model)
}