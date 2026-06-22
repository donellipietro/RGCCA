# = ========================================================================== =
# - Script: rgcca_options.R
# - Desc: Shared helpers for RGCCA test option generation and C++ wrappers.
# = ========================================================================== =


`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}


run_cpp_executable <- function(path_list, path_cpp_script, executable, file_name_params) {
  runner <- normalizePath(file.path(path_list$cpp, "run.sh"), mustWork = TRUE)
  status <- system2(
    runner,
    args = c(
      "--workdir", path_cpp_script,
      "--quiet",
      "--",
      paste0("./", executable),
      file_name_params
    ),
    stdout = if (isTRUE(IGNORE_CPP_OUTPUT)) FALSE else "",
    stderr = ""
  )

  if (!is.null(status) && !is.na(status) && status != 0) {
    stop(
      paste("C++ executable failed:", executable, "(exit status", status, ")"),
      call. = FALSE
    )
  }

  invisible(status)
}


model_option <- function(test_options, name, default = NULL) {
  test_options$model_options[[name]] %||% default
}


bootstrap_option <- function(test_options, name, default = NULL) {
  test_options$bootstrap_options[[name]] %||% default
}


as_option_bool <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }
  if (is.logical(x)) {
    return(isTRUE(x[1]))
  }
  if (is.numeric(x)) {
    return(!is.na(x[1]) && x[1] != 0)
  }
  if (is.character(x)) {
    return(tolower(x[1]) %in% c("true", "t", "yes", "y", "1", "auto", "automatic"))
  }
  default
}


is_default_rgcca_token <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(TRUE)
  }
  if (!is.character(value)) {
    return(FALSE)
  }
  token <- tolower(gsub("[_ -]", "", value[1]))
  token %in% c("", "default")
}


rgcca_mode_from_tau <- function(tau) {
  if (is.character(tau)) {
    token <- tolower(gsub("[_ -]", "", tau[1]))
    if (token %in% c("optimal", "automatic", "auto", "regularized", "rgcca")) {
      return("Regularized")
    }
  }

  tau <- suppressWarnings(as.numeric(tau[1]))
  if (is.na(tau)) {
    return("Regularized")
  }
  if (tau < 0) {
    return("Regularized")
  }
  if (tau > 0.5) {
    return("CovMax")
  }
  "CorMax"
}


rgcca_weight_sign_constraint <- function(non_negative_weights) {
  if (isTRUE(non_negative_weights)) {
    return("NonNegative")
  }
  "None"
}


effective_rgcca_model_options <- function(model_options,
                                          tau = NULL,
                                          non_negative_weights = NULL,
                                          C = NULL) {
  if (!is.null(tau) && is_default_rgcca_token(model_options$mode)) {
    model_options$mode <- rgcca_mode_from_tau(tau)
  }
  if (!is.null(non_negative_weights) &&
      is_default_rgcca_token(model_options$weight_sign_constraint)) {
    model_options$weight_sign_constraint <-
      rgcca_weight_sign_constraint(non_negative_weights)
  }
  if (!is.null(C)) {
    model_options$C <- C
  }
  model_options
}


test_option_name <- function(test_options, path_results = NULL, default = "manual") {
  name <- test_options$name_test
  if (!is.null(name) && length(name) > 0 && nzchar(name[1])) {
    return(name[1])
  }
  if (!is.null(path_results) && length(path_results) > 0 && nzchar(path_results[1])) {
    normalized <- normalizePath(path_results[1], mustWork = FALSE)
    tail <- basename(normalized)
    if (nzchar(tail)) {
      return(tail)
    }
  }
  default
}


test_batch_index <- function(test_options, default = 1) {
  batch_index <- test_options$batch_index
  if (!is.null(batch_index) && length(batch_index) > 0 && !is.na(batch_index[1])) {
    return(batch_index[1])
  }
  default
}


normalize_rgcca_C <- function(C, n_blocks = NULL, default = NULL) {
  if (is.null(C) || length(C) == 0) {
    C <- default
  }
  if (is.null(C) || length(C) == 0) {
    if (is.null(n_blocks)) {
      stop("n_blocks is required when C is missing", call. = FALSE)
    }
    out <- matrix(1, n_blocks, n_blocks)
    diag(out) <- 0
    return(out)
  }

  if (is.character(C) && length(C) == 1) {
    token <- tolower(gsub("[_ -]", "", C))
    if (token %in% c("default", "data", "reference")) {
      return(normalize_rgcca_C(default, n_blocks = n_blocks))
    }
    if (token %in% c("full", "complete", "fullyconnected", "all")) {
      if (is.null(n_blocks)) {
        stop("n_blocks is required for a full C", call. = FALSE)
      }
      out <- matrix(1, n_blocks, n_blocks)
      diag(out) <- 0
      return(out)
    }
    stop(paste("Unknown RGCCA C token:", C), call. = FALSE)
  }

  out <- as.matrix(C)
  storage.mode(out) <- "numeric"

  if (nrow(out) == 1 || ncol(out) == 1) {
    values <- as.numeric(out)
    side <- sqrt(length(values))
    if (side != floor(side)) {
      stop("RGCCA C vector length must be a square number", call. = FALSE)
    }
    out <- matrix(values, nrow = side, byrow = TRUE)
  }

  if (nrow(out) != ncol(out)) {
    stop("RGCCA C matrix must be square", call. = FALSE)
  }
  if (!is.null(n_blocks) && nrow(out) != n_blocks) {
    stop("RGCCA C matrix size does not match n_blocks", call. = FALSE)
  }
  diag(out) <- 0
  (out != 0) + 0
}


rgcca_C <- function(test_options, data = NULL) {
  n_blocks <- test_options$dimensions$n_groups %||%
    if (!is.null(data$C)) nrow(data$C) else NULL
  default <- if (!is.null(data$C)) data$C else NULL
  C <- model_option(test_options, "C", NULL)

  normalize_rgcca_C(C, n_blocks = n_blocks, default = default)
}


C_to_json <- function(C) {
  C <- normalize_rgcca_C(C)
  lapply(seq_len(nrow(C)), function(i) as.integer(C[i, ] != 0))
}


rgcca_model_option_defaults <- function() {
  list(
    n_comp = 3,
    C = "data",
    max_iter = 1000,
    tol = 1e-8,
    verbose = FALSE,
    cache_covariances = TRUE,
    bias = TRUE,
    init_strategy = "svd",
    lambda_selection_weights = FALSE,
    lambda_selection_components = "Automatic",
    block_deactivation = FALSE,
    connection_deactivation = FALSE,
    component_significance = FALSE,
    mode = "Default",
    weight_sign_constraint = "Default",
    deflation_mode = "Scores",
    scheme = "Factorial"
  )
}


rgcca_bootstrap_option_defaults <- function() {
  list(
    B_max = 5000,
    B_min = 60,
    resampling_strategy = "Ordinary",
    stationary_block_length = 0,
    seed = 12345,
    max_threads = 12,
    B_per_thread_per_batch = 5,
    adaptive = TRUE,
    adaptive_tol = 1e-3,
    stable_batches_required = 3,
    active_block_tol = 1e-8,
    active_connection_sign_stability = 0.95,
    active_connection_min_abs_corr = 0.05,
    ci_level = 0.95,
    patience = 1,
    component_significance_resamples = 100,
    component_significance_alpha = 0.05,
    save_bootstrap_resamples = FALSE
  )
}


resolve_rgcca_model_options <- function(model_options = list()) {
  model_options <- modifyList(rgcca_model_option_defaults(), model_options %||% list(), keep.null = FALSE)
}

resolve_rgcca_bootstrap_options <- function(bootstrap_options = list()) {
  bootstrap_options <- modifyList(rgcca_bootstrap_option_defaults(), bootstrap_options %||% list(), keep.null = FALSE)
}


collect_cpp_model_options <- function(model_options) {
  option_names <- c(
    "max_iter", "tol", "verbose", "cache_covariances", "bias",
    "init", "init_strategy",
    "lambda_selection_components", "lambda_components",
    "block_deactivation", "connection_deactivation", "component_significance",
    "mode", "weight_sign_constraint", "deflation", "deflation_mode", "scheme"
  )

  model_options <- resolve_rgcca_model_options(model_options)
  out <- list()
  for (name in option_names) {
    value <- model_options[[name]]
    if (!is.null(value) && length(value) > 0) {
      out[[name]] <- value
    }
  }
  out
}

collect_cpp_bootstrap_options <- function(bootstrap_options) {
  option_names <- c(
    "seed", "max_threads", "B_min",
    "B_max", "B_per_thread_per_batch",
    "adaptive", "adaptive_tol",
    "stable_batches_required", "active_block_tol",
    "active_connection_sign_stability",
    "active_connection_min_abs_corr", "ci_level",
    "patience", "resampling_strategy",
    "stationary_block_length",
    "component_significance_resamples",
    "component_significance_alpha",
    "save_bootstrap_resamples"
  )

  bootstrap_options <- resolve_rgcca_bootstrap_options(bootstrap_options)
  out <- list()
  for (name in option_names) {
    value <- bootstrap_options[[name]]
    if (!is.null(value) && length(value) > 0) {
      out[[name]] <- value
    }
  }
  out
}


cpp_rgcca_model_options <- function(test_options, data,
                                    model_name,
                                    n_obs, tau,
                                    non_negative_weights,
                                    lambda_selection_weights,
                                    lambda_for_cpp,
                                    lambda_grid = NULL) {
  resolved_model_options <- resolve_rgcca_model_options(test_options$model_options)
  model_options <- collect_cpp_model_options(resolved_model_options)
  model_options$solver <- model_name
  model_options$lambda <- lambda_for_cpp
  model_options$n_obs <- n_obs
  model_options$n_comp <- resolved_model_options$n_comp
  model_options$non_negative_weights <- non_negative_weights
  model_options$tau <- tau
  model_options$lambda_selection_weights <- lambda_selection_weights
  model_options <- effective_rgcca_model_options(
    model_options,
    tau = tau,
    non_negative_weights = non_negative_weights
  )
  if (!is.null(lambda_grid) && length(lambda_grid) > 0) {
    model_options$lambda_grid <- lambda_grid
  }
  model_options$C <- C_to_json(rgcca_C(test_options, data))
  model_options
}

cpp_rgcca_bootstrap_options <- function(test_options) {
  collect_cpp_bootstrap_options(test_options$bootstrap_options)
}

rgcca_input_options <- function(test_options,
                                model_options = list(),
                                bootstrap_options = list(),
                                C = NULL) {
  options <- list()
  options$model_options <- modifyList(
    resolve_rgcca_model_options(test_options$model_options),
    model_options %||% list(),
    keep.null = FALSE
  )
  options$bootstrap_options <- modifyList(
    resolve_rgcca_bootstrap_options(test_options$bootstrap_options),
    bootstrap_options %||% list(),
    keep.null = FALSE
  )
  options$model_options <- effective_rgcca_model_options(
    options$model_options,
    tau = options$model_options$tau,
    non_negative_weights = options$model_options$non_negative_weights,
    C = C
  )
  return(options)
}


new_rgcca_model <- function(test_options,
                            model_options = list(),
                            bootstrap_options = list(),
                            C = NULL) {
  list(
    options = rgcca_input_options(
      test_options,
      model_options = model_options,
      bootstrap_options = bootstrap_options,
      C = C
    ),
    diagnostics = list(),
    model_selection = list(),
    results = list(),
    model_traits = list()
  )
}


read_matrix_if_exists <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  as.matrix(read.csv(path))
}


read_vector_if_exists <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  values <- suppressWarnings(as.numeric(unlist(
    read.csv(path, header = FALSE, check.names = FALSE),
    use.names = FALSE
  )))
  values[!is.na(values)]
}


read_data_frame_if_exists <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  read.csv(path, check.names = FALSE)
}


list_has_values <- function(x) {
  any(vapply(x, function(value) !is.null(value), logical(1)))
}


objective_from_crit <- function(crit, n_comp) {
  objective <- rep(NA_real_, n_comp)
  history <- vector("list", n_comp)

  for (h in seq_len(n_comp)) {
    if (length(crit) < h || is.null(crit[[h]])) {
      next
    }
    history[[h]] <- crit[[h]]
    if (length(history[[h]]) > 0) {
      objective[h] <- tail(history[[h]], 1)
    }
  }

  list(objective = objective, objective_history = history)
}


component_diagnostics_from_objective <- function(objective) {
  data.frame(
    component = seq_along(objective),
    objective = objective
  )
}


load_cpp_diagnostics <- function(path_results, n_comp) {
  out <- list(
    component_diagnostics = read_data_frame_if_exists(
      file.path(path_results, "component_diagnostics.csv")
    ),
    objective_history = vector("list", n_comp),
    covariance_matrices = vector("list", n_comp),
    correlation_matrices = vector("list", n_comp),
    tau = vector("list", n_comp),
    lambda_components = vector("list", n_comp),
    lambda_weights = rep(NA_real_, n_comp),
    C = vector("list", n_comp),
    active_blocks = vector("list", n_comp)
  )

  for (h in seq_len(n_comp)) {
    out$objective_history[[h]] <- read_vector_if_exists(
      file.path(path_results, paste0("objective", h, ".csv"))
    )
    out$covariance_matrices[[h]] <- read_matrix_if_exists(
      file.path(path_results, paste0("covariance_matrix", h, ".csv"))
    )
    out$correlation_matrices[[h]] <- read_matrix_if_exists(
      file.path(path_results, paste0("correlation_matrix", h, ".csv"))
    )
    out$tau[[h]] <- read_vector_if_exists(
      file.path(path_results, paste0("tau", h, ".csv"))
    )
    out$lambda_components[[h]] <- read_vector_if_exists(
      file.path(path_results, paste0("lambda_components", h, ".csv"))
    )
    lambda_weights_h <- read_vector_if_exists(
      file.path(path_results, paste0("lambda_weights", h, ".csv"))
    )
    if (!is.null(lambda_weights_h) && length(lambda_weights_h) > 0) {
      out$lambda_weights[h] <- lambda_weights_h[1]
    }
    out$C[[h]] <- read_matrix_if_exists(
      file.path(path_results, paste0("C", h, ".csv"))
    )

    active_blocks_h <- read_vector_if_exists(
      file.path(path_results, paste0("active_blocks", h, ".csv"))
    )
    if (!is.null(active_blocks_h)) {
      active_blocks_h <- as.logical(active_blocks_h)
    }
    out$active_blocks[[h]] <- active_blocks_h
  }

  out
}


attach_cpp_diagnostics <- function(model, diagnostics, n_comp, n_groups = NULL) {
  if (list_has_values(diagnostics$objective_history)) {
    model$diagnostics$objective_history <- diagnostics$objective_history
  }
  if (list_has_values(diagnostics$covariance_matrices)) {
    model$results$covariance_matrices <- diagnostics$covariance_matrices
  }
  if (list_has_values(diagnostics$correlation_matrices)) {
    model$results$correlation_matrices <- diagnostics$correlation_matrices
  }
  if (list_has_values(diagnostics$tau)) {
    model$model_selection$tau <- diagnostics$tau
  }
  if (list_has_values(diagnostics$lambda_components)) {
    model$model_selection$lambda_components <- diagnostics$lambda_components
  }
  model$model_selection$lambda_weights <- diagnostics$lambda_weights
  if (list_has_values(diagnostics$C)) {
    model$model_selection$C <- diagnostics$C
  }
  if (list_has_values(diagnostics$active_blocks)) {
    model$model_selection$active_blocks <- diagnostics$active_blocks
  }
  if (!is.null(diagnostics$component_diagnostics)) {
    model$diagnostics$component_diagnostics <- diagnostics$component_diagnostics
  }

  model
}


load_cpp_bootstrap_selection <- function(path_results, n_comp, n_groups, grid_D = FALSE) {
  bootstrap <- vector("list", n_comp)
  any_generated <- FALSE

  for (h in seq_len(n_comp)) {
    lambda_grid_path <- file.path(path_results, paste0("bootstrap_lambda_grid", h, ".csv"))
    lambda_grid <- read_vector_if_exists(lambda_grid_path)
    if (is.null(lambda_grid)) {
      next
    }
    any_generated <- TRUE

    criterion <- read_vector_if_exists(
      file.path(path_results, paste0("bootstrap_criterion", h, ".csv"))
    )
    lambda_opt <- read_vector_if_exists(
      file.path(path_results, paste0("bootstrap_lambda_opt", h, ".csv"))
    )
    metadata <- read_data_frame_if_exists(
      file.path(path_results, paste0("bootstrap_metadata", h, ".csv"))
    )
    B_used_by_lambda <- read_vector_if_exists(
      file.path(path_results, paste0("bootstrap_B_used", h, ".csv"))
    )
    w_fit_locs <- vector("list", length(lambda_grid))
    if (grid_D) w_fit_grid <- vector("list", length(lambda_grid))
    w_boot_locs <- vector("list", length(lambda_grid))
    if (grid_D) w_boot_grid <- vector("list", length(lambda_grid))
    w_min_locs <- vector("list", length(lambda_grid))
    if (grid_D) w_min_grid <- vector("list", length(lambda_grid))
    w_ci_locs <- vector("list", length(lambda_grid))
    if (grid_D) w_ci_grid <- vector("list", length(lambda_grid))
    corr_boot <- vector("list", length(lambda_grid))
    corr_min <- vector("list", length(lambda_grid))
    corr_ci_low <- vector("list", length(lambda_grid))
    corr_ci_high <- vector("list", length(lambda_grid))

    for (i in seq_along(lambda_grid)) {
      if (!is.null(criterion) && length(criterion) >= i && criterion[i] < 0) {
        next
      }

      corr_boot[[i]] <- read_matrix_if_exists(
        file.path(path_results, paste0("bootstrap_corr_boot_comp", h, "_lambda", i, ".csv"))
      )
      corr_min[[i]] <- read_matrix_if_exists(
        file.path(path_results, paste0("bootstrap_corr_min_comp", h, "_lambda", i, ".csv"))
      )
      corr_ci_low[[i]] <- read_matrix_if_exists(
        file.path(path_results, paste0("bootstrap_corr_ci_low_comp", h, "_lambda", i, ".csv"))
      )
      corr_ci_high[[i]] <- read_matrix_if_exists(
        file.path(path_results, paste0("bootstrap_corr_ci_high_comp", h, "_lambda", i, ".csv"))
      )

      w_fit_locs[[i]] <- vector("list", n_groups)
      if (grid_D) w_fit_grid[[i]] <- vector("list", n_groups)
      w_boot_locs[[i]] <- vector("list", n_groups)
      if (grid_D) w_boot_grid[[i]] <- vector("list", n_groups)
      w_min_locs[[i]] <- vector("list", n_groups)
      if (grid_D) w_min_grid[[i]] <- vector("list", n_groups)
      w_ci_locs[[i]] <- vector("list", n_groups)
      if (grid_D) w_ci_grid[[i]] <- vector("list", n_groups)

      for (g in seq_len(n_groups)) {
        w_fit_locs[[i]][[g]] <- read_matrix_if_exists(
          file.path(path_results, paste0(
            "bootstrap_weights_fit_comp", h, "_lambda", i,
            "_block", g, "_locs.csv"
          ))
        )
        w_boot_locs[[i]][[g]] <- read_matrix_if_exists(
          file.path(path_results, paste0(
            "bootstrap_weights_boot_comp", h, "_lambda", i,
            "_block", g, "_locs.csv"
          ))
        )
        w_min_locs[[i]][[g]] <- read_matrix_if_exists(
          file.path(path_results, paste0(
            "bootstrap_weights_wmin_comp", h, "_lambda", i,
            "_block", g, "_locs.csv"
          ))
        )
        w_ci_locs[[i]][[g]] <- read_matrix_if_exists(
          file.path(path_results, paste0(
            "bootstrap_weights_ci_comp", h, "_lambda", i,
            "_block", g, "_locs.csv"
          ))
        )

        if (grid_D) {
          w_fit_grid[[i]][[g]] <- read_matrix_if_exists(
            file.path(path_results, paste0(
              "bootstrap_weights_fit_comp", h, "_lambda", i,
              "_block", g, "_grid.csv"
            ))
          )
          w_boot_grid[[i]][[g]] <- read_matrix_if_exists(
            file.path(path_results, paste0(
              "bootstrap_weights_boot_comp", h, "_lambda", i,
              "_block", g, "_grid.csv"
            ))
          )
          w_min_grid[[i]][[g]] <- read_matrix_if_exists(
            file.path(path_results, paste0(
              "bootstrap_weights_wmin_comp", h, "_lambda", i,
              "_block", g, "_grid.csv"
            ))
          )
          w_ci_grid[[i]][[g]] <- read_matrix_if_exists(
            file.path(path_results, paste0(
              "bootstrap_weights_ci_comp", h, "_lambda", i,
              "_block", g, "_grid.csv"
            ))
          )
        }
      }
    }

    bootstrap[[h]] <- list(
      lambda_grid = lambda_grid,
      criterion = criterion,
      lambda_opt = if (!is.null(lambda_opt)) lambda_opt[1] else NULL,
      lambda_opt_index = if (!is.null(metadata) && "lambda_opt_index" %in% names(metadata)) {
        metadata$lambda_opt_index[1]
      } else {
        NULL
      },
      B = if (!is.null(metadata) && "B" %in% names(metadata)) metadata$B[1] else NULL,
      ci_level = if (!is.null(metadata) && "ci_level" %in% names(metadata)) {
        metadata$ci_level[1]
      } else {
        NULL
      },
      metadata = metadata,
      B_used_by_lambda = B_used_by_lambda,
      w_fit_locs = w_fit_locs,
      w_boot_locs = w_boot_locs,
      w_min_locs = w_min_locs,
      w_ci_locs = w_ci_locs,
      corr_boot = corr_boot,
      corr_min = corr_min,
      corr_ci_low = corr_ci_low,
      corr_ci_high = corr_ci_high
    )
    if (grid_D) bootstrap[[h]]$w_fit_grid <- w_fit_grid
    if (grid_D) bootstrap[[h]]$w_boot_grid <- w_boot_grid
    if (grid_D) bootstrap[[h]]$w_min_grid <- w_min_grid
    if (grid_D) bootstrap[[h]]$w_ci_grid <- w_ci_grid
  }

  if (!any_generated) {
    return(NULL)
  }
  bootstrap
}


rgcca_model_options <- function(...) {
  resolve_rgcca_model_options(list(...))
}


rgcca_bootstrap_options <- function(...) {
  resolve_rgcca_bootstrap_options(list(...))
}
