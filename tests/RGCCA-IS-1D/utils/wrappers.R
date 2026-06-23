# = ========================================================================== =
# - Script: wrappers.R
# - Desc: Provides model fitting utilities. Includes:
#         * dispatcher for model selection,
#         * implementation of standard multivariate ...
#         * interface to functional ... solvers executed via external C++ code.
# = ========================================================================== =


if (!exists("collect_cpp_model_options") ||
    !exists("effective_rgcca_model_options") ||
    !exists("rgcca_model_options")) {
  source("src/utils/rgcca_options.R")
}
#' Dispatch one model name to the matching R or C++ fitting wrapper.
#'
#' @param model_name Model identifier from `test_options$model_names`.
#' @param data Generated data and truth object.
#' @param path_list Named list of repository, output, queue, and temporary paths.
#' @param test_options Nested option object loaded from JSON.
#' @return The value produced by `fit_model`.
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
    {
      stop(paste("The model", model_name, "does not exist"))
    }
  )
}


#' Fit a multivariate RGCCA model with the R package backend.
#'
#' @param model_name Model identifier from `test_options$model_names`.
#' @param data Generated data and truth object.
#' @param test_options Nested option object loaded from JSON.
#' @return The value produced by `R_RGCCA`.
R_RGCCA <- function(model_name, data, test_options) {
  ## Get all the necessary info from data and test_options
  n_groups <- test_options$dimensions$n_groups
  n_comp <- model_option(test_options, "n_comp", rgcca_model_option_defaults()$n_comp)
  blocks <- data$X
  C <- rgcca_C(test_options, data)

  switch(model_name,
    R_GCCA_cor = {
      tau <- 0
    },
    R_RGCCA = {
      tau <- "optimal"
    },
    R_GCCA_cov = {
      tau <- 1
    }
  )

  model <- new_rgcca_model(
    test_options,
    model_options = rgcca_model_options(
      solver = model_name,
      n_obs = nrow(blocks[[1]]),
      n_comp = n_comp,
      lambda = 0,
      tau = tau,
      non_negative_weights = FALSE,
      lambda_selection_weights = FALSE,
      include_defaults = FALSE
    ),
    C = C
  )

  # Fit multivariate PCA ----
  start.time <- Sys.time()

  fit_rgcca <- rgcca(
    blocks,
    method      = "rgcca",
    scale_block = FALSE,
    scale       = FALSE,
    bias        = TRUE,
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

  if (!IGNORE_R_OUTPUT) {
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
  for (g in 1:n_groups) {
    sim <- abs(var(data$A_locs[[g]][, 1:n_comp], A_locs[[g]]))
    perm <- solve_LSAP(sim, maximum = TRUE)
    A_locs[[g]] <- A_locs[[g]][, perm, drop = FALSE]
    A_star_locs[[g]] <- A_star_locs[[g]][, perm, drop = FALSE]
    H[[g]] <- H[[g]][, perm, drop = FALSE]

    for (h in 1:n_comp) {
      norm_H <- var(H[[g]][, h])
      norm_H <- if (is.na(norm_H) || norm_H == 0) 1 else sqrt(norm_H)
      norm_A <- norm_l2(A_locs[[g]][, h])
      norm_A <- if (is.na(norm_A) || norm_A == 0) 1 else norm_A
      norm_A_star <- norm_l2(A_star_locs[[g]][, h])
      norm_A_star <- if (is.na(norm_A_star) || norm_A_star == 0) 1 else norm_A_star

      sign <- sign(cov(A_locs[[g]][, h], data$A_locs[[g]][, h]))
      if (sign == 0) sign <- sign(mean(A_locs[[g]][, h]))
      if (sign == 0) sign <- 1

      H[[g]][, h] <- sign * H[[g]][, h] / norm_H
      A_locs[[g]][, h] <- sign * A_locs[[g]][, h] / norm_A
      A_star_locs[[g]][, h] <- sign * A_star_locs[[g]][, h] / norm_A_star
    }
  }

  # Save results ----
  model$results$A_hat_locs <- A_locs
  model$results$A_star_hat_locs <- A_star_locs
  model$results$H_hat <- H
  model$diagnostics$execution_time <- end.time - start.time
  model$model_selection$tau <- replicate(n_comp, fit_rgcca$call$tau, simplify = FALSE)
  model$model_selection$lambda_weights <- rep(0, n_comp)
  model$model_selection$lambda_components <- replicate(n_comp, rep(0, n_groups), simplify = FALSE)
  objective <- objective_from_crit(fit_rgcca$crit, n_comp)
  model$diagnostics$component_diagnostics <- component_diagnostics_from_objective(
    objective$objective
  )
  model$diagnostics$objective_history <- objective$objective_history

  # Add flags ----
  model$model_traits$backend <- "R"
  model$model_traits$is_functional <- FALSE
  model$model_traits$has_interpolator <- FALSE

  return(model)
}


#' Fit an RGCCA-family model through the compiled C++ backend.
#'
#' @param model_name Model identifier from `test_options$model_names`.
#' @param data Generated data and truth object.
#' @param test_options Nested option object loaded from JSON.
#' @param path_list Named list of repository, output, queue, and temporary paths.
#' @return The value produced by `CPP_RGCCA`.
CPP_RGCCA <- function(model_name, data, test_options, path_list) {
  ## Info
  n_comp <- model_option(test_options, "n_comp", rgcca_model_option_defaults()$n_comp)

  lambda <- test_options$regularization$lambda %||% 0
  lambda_grid <- test_options$regularization$lambda_grid %||% NULL
  if (model_name %in% c("CPP_GCCA_cor", "CPP_RGCCA", "CPP_GCCA_cov", "CPP_GCCA_NN_cor", "CPP_RGCCA_NN", "CPP_GCCA_NN_cov")) {
    if (!is.na(lambda) && lambda != 0) {
      return()
    }
  } else {
    if (!is.na(lambda) && lambda == 0) {
      return()
    }
  }


  non_negative_weights <- FALSE
  switch(model_name,
    CPP_GCCA_cor = {
      tau <- 0
    },
    CPP_fGCCA_cor_FEM = {
      tau <- 0
    },
    CPP_fGCCA_cor_SPLINES = {
      tau <- 0
    },
    CPP_GCCA_NN_cor = {
      tau <- 0
      non_negative_weights <- TRUE
    },
    CPP_fGCCA_NN_cor_FEM = {
      tau <- 0
      non_negative_weights <- TRUE
    },
    CPP_fGCCA_NN_cor_SPLINES = {
      tau <- 0
      non_negative_weights <- TRUE
    },
    CPP_RGCCA = {
      tau <- -1
    },
    CPP_fRGCCA_FEM = {
      tau <- -1
    },
    CPP_fRGCCA_SPLINES = {
      tau <- -1
    },
    CPP_RGCCA_NN = {
      tau <- -1
      non_negative_weights <- TRUE
    },
    CPP_fRGCCA_NN_FEM = {
      tau <- -1
      non_negative_weights <- TRUE
    },
    CPP_fRGCCA_NN_SPLINES = {
      tau <- -1
      non_negative_weights <- TRUE
    },
    CPP_GCCA_cov = {
      tau <- 1
    },
    CPP_fGCCA_cov_FEM = {
      tau <- 1
    },
    CPP_fGCCA_cov_SPLINES = {
      tau <- 1
    },
    CPP_GCCA_NN_cov = {
      tau <- 1
      non_negative_weights <- TRUE
    },
    CPP_fGCCA_NN_cov_FEM = {
      tau <- 1
      non_negative_weights <- TRUE
    },
    CPP_fGCCA_NN_cov_SPLINES = {
      tau <- 1
      non_negative_weights <- TRUE
    },
  )

  # Paths ----
  path_cpp_script <- path_list$cpp_script
  path_batch <- path_list$batch
  path_tmp_data <- path_list$tmp_data
  path_tmp_results <- path_list$tmp_results
  if (is.null(path_tmp_data) || !nzchar(path_tmp_data) ||
      is.null(path_tmp_results) || !nzchar(path_tmp_results)) {
    stop("C++ exchange paths tmp_data and tmp_results must be set.", call. = FALSE)
  }
  # path_tmp_data    <- paste0(path_list$tmp_data, model_name, "/")
  # path_tmp_results <- paste0(path_list$tmp_results, model_name, "/")
  # mkdir(path_tmp_data, path_tmp_results)
  path_tmp_mesh <- paste0(path_list$tmp_data, "mesh/")
  mkdir(c(path_tmp_data, path_tmp_results, path_tmp_mesh))

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
  lambda <- test_options$regularization$lambda %||% 0
  lambda_selection_weights <- as_option_bool(model_option(test_options, "lambda_selection_weights", FALSE))
  lambda_is_auto <- !is.na(lambda) && lambda < 0
  lambda_selection_weights <- isTRUE(lambda_is_auto || lambda_selection_weights)
  lambda_for_cpp <- if (lambda_is_auto) 1e-12 else ifelse(is.na(lambda), 0, lambda)
  C <- rgcca_C(test_options, data)
  cpp_script_arguments <- list()
  cpp_script_arguments$path_list <- list(
    mesh = path_tmp_mesh,
    data = path_tmp_data,
    results = path_tmp_results
  )
  cpp_script_arguments$model_options <- cpp_rgcca_model_options(
    test_options = test_options,
    data = data,
    model_name = model_name,
    n_obs = data$dimensions$n,
    tau = tau,
    non_negative_weights = non_negative_weights,
    lambda_selection_weights = lambda_selection_weights,
    lambda_for_cpp = lambda_for_cpp,
    lambda_grid = lambda_grid
  )
  cpp_script_arguments$bootstrap_options <- cpp_rgcca_bootstrap_options(
    test_options = test_options
  )
  if (!is.null(cpp_script_arguments$bootstrap_options$B_max) &&
      cpp_script_arguments$bootstrap_options$B_max < 0) {
    cpp_script_arguments$bootstrap_options$B_max <- 5000
    cpp_script_arguments$bootstrap_options$adaptive <- TRUE
  }

  model <- new_rgcca_model(
    test_options,
    model_options = cpp_script_arguments$model_options,
    bootstrap_options = cpp_script_arguments$bootstrap_options,
    C = C
  )

  name_test <- test_option_name(test_options, path_tmp_results)
  batch_index <- test_batch_index(test_options)
  file_name_params <- paste0(
    name_test, "_", model_name, "_batch_",
    batch_index, "_params.json"
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
  set_cpp_thread_env(test_options)
  #' Run the selected C++ executable for the current wrapper call.
  #'
  #' @param executable Compiled executable name.
  #' @return The value produced by `run_cpp`.
  run_cpp <- function(executable) {
    run_cpp_executable(path_list, path_cpp_script, executable, file_name_params)
  }

  switch(model_name,
    CPP_GCCA_cor = run_cpp("fit_model_RGCCA"),
    CPP_RGCCA = run_cpp("fit_model_RGCCA"),
    CPP_GCCA_cov = run_cpp("fit_model_RGCCA"),
    CPP_GCCA_NN_cor = run_cpp("fit_model_RGCCA"),
    CPP_RGCCA_NN = run_cpp("fit_model_RGCCA"),
    CPP_GCCA_NN_cov = run_cpp("fit_model_RGCCA"),
    CPP_fGCCA_cor_FEM = {
      run_cpp("fit_model_fRGCCA_fem")
      grid_D <- TRUE
    },
    CPP_fRGCCA_FEM = {
      run_cpp("fit_model_fRGCCA_fem")
      grid_D <- TRUE
    },
    CPP_fGCCA_cov_FEM = {
      run_cpp("fit_model_fRGCCA_fem")
      grid_D <- TRUE
    },
    CPP_fGCCA_NN_cor_FEM = {
      run_cpp("fit_model_fRGCCA_fem")
      grid_D <- TRUE
    },
    CPP_fRGCCA_NN_FEM = {
      run_cpp("fit_model_fRGCCA_fem")
      grid_D <- TRUE
    },
    CPP_fGCCA_NN_cov_FEM = {
      run_cpp("fit_model_fRGCCA_fem")
      grid_D <- TRUE
    },
    CPP_fGCCA_cor_SPLINES = {
      run_cpp("fit_model_fRGCCA_splines")
      grid_D <- TRUE
    },
    CPP_fRGCCA_SPLINES = {
      run_cpp("fit_model_fRGCCA_splines")
      grid_D <- TRUE
    },
    CPP_fGCCA_cov_SPLINES = {
      run_cpp("fit_model_fRGCCA_splines")
      grid_D <- TRUE
    },
    CPP_fGCCA_NN_cor_SPLINES = {
      run_cpp("fit_model_fRGCCA_splines")
      grid_D <- TRUE
    },
    CPP_fRGCCA_NN_SPLINES = {
      run_cpp("fit_model_fRGCCA_splines")
      grid_D <- TRUE
    },
    CPP_fGCCA_NN_cov_SPLINES = {
      run_cpp("fit_model_fRGCCA_splines")
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
  for (g in 1:n_groups) {
    H[[g]] <- as.matrix(read.csv(paste(path_tmp_results, "E", g, "_hat_locs.csv", sep = "")))
    A_locs[[g]] <- as.matrix(read.csv(paste(path_tmp_results, "A", g, "_hat_locs.csv", sep = "")))
    A_star_locs[[g]] <- as.matrix(read.csv(paste(path_tmp_results, "A_star", g, "_hat_locs.csv", sep = "")))
    if (grid_D) A_grid[[g]] <- as.matrix(read.csv(paste(path_tmp_results, "A", g, "_hat_grid.csv", sep = "")))
    if (grid_D) A_star_grid[[g]] <- as.matrix(read.csv(paste(path_tmp_results, "A_star", g, "_hat_grid.csv", sep = "")))

    sim <- abs(var(data$A_locs[[g]][, 1:n_comp], A_locs[[g]]))
    perm <- solve_LSAP(sim, maximum = TRUE)
    A_locs[[g]] <- A_locs[[g]][, perm, drop = FALSE]
    A_star_locs[[g]] <- A_star_locs[[g]][, perm, drop = FALSE]
    if (grid_D) A_grid[[g]] <- A_grid[[g]][, perm, drop = FALSE]
    if (grid_D) A_star_grid[[g]] <- A_star_grid[[g]][, perm, drop = FALSE]
    H[[g]] <- H[[g]][, perm, drop = FALSE]

    for (h in 1:n_comp) {
      norm_H <- var(H[[g]][, h])
      norm_H <- if (is.na(norm_H) || norm_H == 0) 1 else sqrt(norm_H)
      norm_A <- norm_l2(A_locs[[g]][, h])
      norm_A <- if (is.na(norm_A) || norm_A == 0) 1 else norm_A
      norm_A_star <- norm_l2(A_star_locs[[g]][, h])
      norm_A_star <- if (is.na(norm_A_star) || norm_A_star == 0) 1 else norm_A_star

      sign <- sign(cov(A_locs[[g]][, h], data$A_locs[[g]][, h]))
      if (sign == 0) sign <- sign(mean(A_locs[[g]][, h]))
      if (sign == 0) sign <- 1

      H[[g]][, h] <- sign * H[[g]][, h] / norm_H
      A_locs[[g]][, h] <- sign * A_locs[[g]][, h] / norm_A
      A_star_locs[[g]][, h] <- sign * A_star_locs[[g]][, h] / norm_A_star
      if (grid_D) A_grid[[g]][, h] <- sign * A_grid[[g]][, h] / norm_A
      if (grid_D) A_star_grid[[g]][, h] <- sign * A_star_grid[[g]][, h] / norm_A_star
    }
  }

  ## Save results ----
  model$results$H_hat <- H
  model$results$A_hat_locs <- A_locs
  model$results$A_star_hat_locs <- A_star_locs
  if (grid_D) model$results$A_hat_grid <- A_grid
  if (grid_D) model$results$A_star_hat_grid <- A_star_grid
  model$diagnostics$execution_time <- end.time - start.time

  # Add flags ----
  model$model_traits$backend <- "CPP"
  model$model_traits$is_functional <- FALSE
  model$model_traits$has_interpolator <- FALSE

  diagnostics <- load_cpp_diagnostics(path_tmp_results, n_comp)
  model <- attach_cpp_diagnostics(model, diagnostics, n_comp, data$dimensions$n_groups)

  ## Load bootstrap selection results ----
  bootstrap <- load_cpp_bootstrap_selection(path_tmp_results, n_comp, n_groups, grid_D)
  if (!is.null(bootstrap)) {
    model$model_selection$bootstrap <- bootstrap
  }

  return(model)
}


#' Fit a functional GCCA model with the R FGCCA backend.
#'
#' @param model_name Model identifier from `test_options$model_names`.
#' @param data Generated data and truth object.
#' @param test_options Nested option object loaded from JSON.
#' @return The value produced by `R_FGCCA`.
R_FGCCA <- function(model_name, data, test_options) {
  ## Get all the necessary info from data and test_options
  n_groups <- test_options$dimensions$n_groups
  n_comp <- model_option(test_options, "n_comp", rgcca_model_option_defaults()$n_comp)
  blocks <- data$X
  C <- rgcca_C(test_options, data)

  switch(model_name,
    R_FGCCA_cor = {
      tau <- 0
    },
    R_FGCCA_cov = {
      tau <- 1
    }
  )

  model <- new_rgcca_model(
    test_options,
    model_options = rgcca_model_options(
      solver = model_name,
      n_obs = nrow(blocks[[1]]),
      n_comp = n_comp,
      lambda = 0,
      tau = tau,
      non_negative_weights = FALSE,
      lambda_selection_weights = FALSE,
      include_defaults = FALSE
    ),
    C = C
  )

  ## Data preparation
  Lys <- list()
  Lts <- list()
  for (g in 1:4) {
    Lys[[g]] <- lapply(seq_len(nrow(data$X[[g]])), function(i) data$X[[g]][i, ])
    Lts[[g]] <- lapply(seq_len(nrow(data$X[[g]])), function(i) data$locations_D)
  }

  # Fit multivariate PCA ----
  start.time <- Sys.time()
  fit_fgcca <- fgcca(
    Lys,
    Lts,
    connection = C,
    tau = rep(tau, n_groups),
    scheme = "factorial",
    init = "svd",
    tol = 1e-8,
    deflType = "uncor",
    verbose = !IGNORE_R_OUTPUT,
    center = TRUE,
    scale = FALSE,
    ncomp = rep(n_comp, n_groups) # ,
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
  for (g in 1:n_groups) {
    A_locs[[g]] <- matrix(0, n_locs, n_comp)
    A_grid[[g]] <- matrix(0, n_grid, n_comp)
    for (h in 1:n_comp) {
      A_locs[[g]][, h] <- approx(cg, A_cg[[g]][, h], xout = data$locations_D, rule = 2)$y
      A_grid[[g]][, h] <- approx(cg, A_cg[[g]][, h], xout = data$grid_D, rule = 2)$y
    }
  }

  n_groups <- test_options$dimensions$n_groups
  for (g in 1:n_groups) {
    for (h in 1:n_comp) {
      norm_H <- var(H[[g]][, h])
      norm_H <- if (is.na(norm_H) || norm_H == 0) 1 else sqrt(norm_H)
      norm_A <- norm_l2(A_locs[[g]][, h])
      norm_A <- if (is.na(norm_A) || norm_A == 0) 1 else norm_A
      sign <- 1
      if (mean(A_locs[[g]][, h]) < 0) sign <- -1
      H[[g]][, h] <- sign * H[[g]][, h] / norm_H
      A_locs[[g]][, h] <- sign * A_locs[[g]][, h] / norm_A
      A_grid[[g]][, h] <- sign * A_grid[[g]][, h] / norm_A
    }
    sim <- abs(var(data$A_locs[[g]][, 1:n_comp], A_locs[[g]]))
    perm <- solve_LSAP(sim, maximum = TRUE)
    A_locs[[g]] <- A_locs[[g]][, perm, drop = FALSE]
    A_grid[[g]] <- A_grid[[g]][, perm, drop = FALSE]
    H[[g]] <- H[[g]][, perm, drop = FALSE]
  }

  ## Save results ----
  model$results$H_hat <- H
  model$results$A_hat_locs <- A_locs
  model$results$A_hat_grid <- A_grid
  model$diagnostics$execution_time <- end.time - start.time
  model$model_selection$tau <- replicate(n_comp, rep(tau, n_groups), simplify = FALSE)
  model$model_selection$lambda_weights <- rep(0, n_comp)
  model$model_selection$lambda_components <- replicate(n_comp, rep(0, n_groups), simplify = FALSE)
  objective <- objective_from_crit(fit_fgcca$crit, n_comp)
  model$diagnostics$component_diagnostics <- component_diagnostics_from_objective(
    objective$objective
  )
  model$diagnostics$objective_history <- objective$objective_history

  # Add flags ----
  model$model_traits$backend <- "R"
  model$model_traits$is_functional <- FALSE
  model$model_traits$has_interpolator <- FALSE

  return(model)
}
