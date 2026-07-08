# = ========================================================================== =
# - Script: rgcca_options.R
# - Desc: Shared helpers for RGCCA test option generation and C++ wrappers.
# = ========================================================================== =


#' Describe the `%||%` helper used by this repository.
#'
#' @param x Input object.
#' @param y Fallback or comparison value.
#' @return The value produced by `%||%`.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}


#' Run a compiled C++ executable through the configured runtime wrapper.
#'
#' @param path_list Named list of repository, output, queue, and temporary paths.
#' @param path_cpp_script Directory containing the compiled C++ executable.
#' @param executable Compiled executable name.
#' @param file_name_params Name of the JSON parameter file passed to C++.
#' @return The value produced by `run_cpp_executable`.
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


#' Read a model option with a fallback value.
#'
#' @param test_options Nested option object loaded from JSON.
#' @param name Option or field name.
#' @param default Fallback value used when the option is missing.
#' @return The value produced by `model_option`.
model_option <- function(test_options, name, default = NULL) {
  test_options$model_options[[name]] %||% default
}


#' Read a bootstrap option with a fallback value.
#'
#' @param test_options Nested option object loaded from JSON.
#' @param name Option or field name.
#' @param default Fallback value used when the option is missing.
#' @return The value produced by `bootstrap_option`.
bootstrap_option <- function(test_options, name, default = NULL) {
  test_options$bootstrap_options[[name]] %||% default
}


#' Coerce a loose option value to a logical flag.
#'
#' @param x Input object.
#' @param default Fallback value used when the option is missing.
#' @return The value produced by `as_option_bool`.
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


#' Resolve the threading mode declared by a test option.
#'
#' @param test_options Nested option object loaded from JSON.
#' @param default Fallback value used when the option is missing.
#' @return The value produced by `test_threading_mode`.
test_threading_mode <- function(test_options, default = "single") {
  mode <- NULL
  if (!is.null(test_options$test_options)) {
    mode <- test_options$test_options$threading
  }
  mode <- mode %||% default
  mode <- tolower(as.character(mode[1]))
  if (!mode %in% c("single", "multi")) {
    stop("test_options$threading must be either 'single' or 'multi'.", call. = FALSE)
  }
  mode
}


#' Pin solver thread-count environment variables to one.
#'
#' @param test_options Nested option object loaded from JSON.
#' @return The value produced by `set_cpp_thread_env`.
set_cpp_thread_env <- function(test_options) {
  Sys.setenv(
    OMP_NUM_THREADS = "1",
    OPENBLAS_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1"
  )
  invisible("1")
}


#' Check whether an RGCCA option token means default behavior.
#'
#' @param value Value to process.
#' @return The value produced by `is_default_rgcca_token`.
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

#' Validate and canonicalize a string option.
#'
#' @param value String option value.
#' @param choices Named character vector mapping accepted tokens to output values.
#' @param name Option name used in error messages.
#' @return The canonical option value.
match_string_option <- function(value, choices, name) {
  if (!is.character(value) || length(value) != 1) {
    stop(paste0(name, " must be one of: ", paste(unique(choices), collapse = ", ")), call. = FALSE)
  }
  token <- tolower(gsub("[_ -]", "", value))
  choices[[match.arg(token, names(choices))]]
}


#' Infer the RGCCA mode corresponding to a tau value.
#'
#' @param tau RGCCA tau value or token.
#' @return The value produced by `rgcca_mode_from_tau`.
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


#' Map a non-negative-weight flag to the C++ option token.
#'
#' @param non_negative_weights Whether weights are constrained to be non-negative.
#' @return The value produced by `rgcca_weight_sign_constraint`.
rgcca_weight_sign_constraint <- function(non_negative_weights) {
  if (isTRUE(non_negative_weights)) {
    return("NonNegative")
  }
  "None"
}


#' Fill derived RGCCA model options from tau, signs, and connectivity.
#'
#' @param model_options Model-option list.
#' @param tau RGCCA tau value or token.
#' @param non_negative_weights Whether weights are constrained to be non-negative.
#' @param C RGCCA connection matrix or token.
#' @return The value produced by `effective_rgcca_model_options`.
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


#' Resolve the stable name for a test option.
#'
#' @param test_options Nested option object loaded from JSON.
#' @param path_results Directory containing result files.
#' @param default Fallback value used when the option is missing.
#' @return The value produced by `test_option_name`.
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


#' Resolve the current batch index from a test option.
#'
#' @param test_options Nested option object loaded from JSON.
#' @param default Fallback value used when the option is missing.
#' @return The value produced by `test_batch_index`.
test_batch_index <- function(test_options, default = 1) {
  batch_index <- test_options$batch_index
  if (!is.null(batch_index) && length(batch_index) > 0 && !is.na(batch_index[1])) {
    return(batch_index[1])
  }
  default
}


#' Normalize an RGCCA connection matrix into a square binary matrix.
#'
#' @param C RGCCA connection matrix or token.
#' @param n_blocks Expected number of RGCCA blocks.
#' @param default Fallback value used when the option is missing.
#' @return The value produced by `normalize_rgcca_C`.
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


#' Resolve the RGCCA connection matrix from test options and data defaults.
#'
#' @param test_options Nested option object loaded from JSON.
#' @param data Generated data and truth object.
#' @return The value produced by `rgcca_C`.
rgcca_C <- function(test_options, data = NULL) {
  n_blocks <- test_options$dimensions$n_groups %||%
    if (!is.null(data$C)) nrow(data$C) else NULL
  default <- if (!is.null(data$C)) data$C else NULL
  C <- model_option(test_options, "C", NULL)

  normalize_rgcca_C(C, n_blocks = n_blocks, default = default)
}


#' Convert a connection matrix to JSON-friendly row lists.
#'
#' @param C RGCCA connection matrix or token.
#' @return The value produced by `C_to_json`.
C_to_json <- function(C) {
  C <- normalize_rgcca_C(C)
  lapply(seq_len(nrow(C)), function(i) as.integer(C[i, ] != 0))
}


#' Return default model options for R and C++ RGCCA drivers.
#'
#' @return The value produced by `rgcca_model_option_defaults`.
rgcca_model_option_defaults <- function() {
  list(
    n_comp = 3,
    C = "data",
    max_iter = 1000,
    tol = 1e-8,
    cache_covariances = TRUE,
    bias = TRUE,
    init_strategy = "svd",
    lambda_selection_weights = FALSE,
    lambda_selection_components = "Automatic",
    block_deactivation = FALSE,
    connection_deactivation = FALSE,
    component_significance = FALSE,
    block_importance = FALSE,
    inactive_block_signal_test = FALSE,
    mode = "Default",
    weight_sign_constraint = "Default",
    deflation_mode = "Scores",
    scheme = "Factorial"
  )
}


#' Return default bootstrap-selection options for RGCCA drivers.
#'
#' @return The value produced by `rgcca_bootstrap_option_defaults`.
rgcca_max_threads <- function(default = 12) {
  cfg_threads <- NULL
  if (exists("load_config", mode = "function")) {
    cfg_threads <- tryCatch(load_config()$MULTITHREAD_CPUS, error = function(e) NULL)
  }
  values <- c(
    as.character(cfg_threads %||% ""),
    Sys.getenv("MULTITHREAD_CPUS", unset = ""),
    Sys.getenv("TESTBENCH_MULTITHREAD_CPUS", unset = ""),
    as.character(default)
  )
  value <- values[nzchar(values)][1]
  value <- suppressWarnings(as.integer(value[1]))
  if (is.na(value) || value < 1) default else value
}

rgcca_bootstrap_option_defaults <- function() {
  list(
    B_max = 5000,
    B_min = 60,
    resampling_strategy = "Ordinary",
    stationary_block_length = 0,
    seed = 12345,
    max_threads = rgcca_max_threads(),
    check_every = 5,
    check_every_block_deactivation = 1,
    check_every_connection_deactivation = 100,
    fit_max_iter = -1,
    adaptive = TRUE,
    adaptive_tol = 1e-3,
    stable_checks_required = 3,
    active_block_tol = 1e-8,
    active_connection_sign_stability = 0.95,
    active_connection_min_abs_corr = 0.05,
    ci_level = 0.95,
    patience = 1,
    component_significance_resamples = 100,
    component_significance_alpha = 0.05,
    block_importance_resamples = 100,
    block_importance_alpha = 0.05,
    inactive_block_signal_resamples = 100,
    inactive_block_signal_alpha = 0.05,
    save_bootstrap_resamples = FALSE
  )
}


#' Merge user model options with RGCCA defaults.
#'
#' @param model_options Model-option list.
#' @return The value produced by `resolve_rgcca_model_options`.
resolve_rgcca_model_options <- function(model_options = list()) {
  model_options <- modifyList(rgcca_model_option_defaults(), model_options %||% list(), keep.null = FALSE)
}

#' Merge user bootstrap options with RGCCA defaults.
#'
#' @param bootstrap_options Bootstrap-option list.
#' @return The value produced by `resolve_rgcca_bootstrap_options`.
resolve_rgcca_bootstrap_options <- function(bootstrap_options = list()) {
  bootstrap_options <- modifyList(rgcca_bootstrap_option_defaults(), bootstrap_options %||% list(), keep.null = FALSE)
}


#' Select model options that should be passed to C++ drivers.
#'
#' @param model_options Model-option list.
#' @return The value produced by `collect_cpp_model_options`.
collect_cpp_model_options <- function(model_options) {
  option_names <- c(
    "max_iter", "tol", "cache_covariances", "bias",
    "init_strategy",
    "lambda_selection_weights", "lambda_selection_components", "lambda_components",
    "block_deactivation", "connection_deactivation", "component_significance",
    "block_importance", "inactive_block_signal_test",
    "mode", "weight_sign_constraint", "deflation_mode", "scheme"
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

#' Select bootstrap options that should be passed to C++ drivers.
#'
#' @param bootstrap_options Bootstrap-option list.
#' @return The value produced by `collect_cpp_bootstrap_options`.
collect_cpp_bootstrap_options <- function(bootstrap_options) {
  option_names <- c(
    "seed", "max_threads", "B_min",
    "B_max", "check_every", "fit_max_iter",
    "check_every_block_deactivation",
    "check_every_connection_deactivation",
    "adaptive", "adaptive_tol",
    "stable_checks_required", "active_block_tol",
    "active_connection_sign_stability",
    "active_connection_min_abs_corr",
    "ci_level",
    "patience", "resampling_strategy",
    "stationary_block_length",
    "component_significance_resamples",
    "component_significance_alpha",
    "block_importance_resamples",
    "block_importance_alpha",
    "inactive_block_signal_resamples",
    "inactive_block_signal_alpha",
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


#' Build the model-option JSON block consumed by a C++ RGCCA executable.
#'
#' @param test_options Nested option object loaded from JSON.
#' @param data Generated data and truth object.
#' @param model_name Model identifier from `test_options$model_names`.
#' @param n_obs Number of observations.
#' @param tau RGCCA tau value or token.
#' @param non_negative_weights Whether weights are constrained to be non-negative.
#' @param lambda_selection_weights Whether C++ should select weight regularization.
#' @param lambda_for_cpp Lambda value passed to C++.
#' @param lambda_grid Optional lambda-search grid.
#' @return The value produced by `cpp_rgcca_model_options`.
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

#' Build the bootstrap-option JSON block consumed by C++ drivers.
#'
#' @param test_options Nested option object loaded from JSON.
#' @return The value produced by `cpp_rgcca_bootstrap_options`.
cpp_rgcca_bootstrap_options <- function(test_options) {
  collect_cpp_bootstrap_options(test_options$bootstrap_options)
}

#' Build the normalized option bundle stored inside a fitted model object.
#'
#' @param test_options Nested option object loaded from JSON.
#' @param model_options Model-option list.
#' @param bootstrap_options Bootstrap-option list.
#' @param C RGCCA connection matrix or token.
#' @return The value produced by `rgcca_input_options`.
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


#' Create the standard model result container used by wrappers.
#'
#' @param test_options Nested option object loaded from JSON.
#' @param model_options Model-option list.
#' @param bootstrap_options Bootstrap-option list.
#' @param C RGCCA connection matrix or token.
#' @return The value produced by `new_rgcca_model`.
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


#' Read a CSV matrix when the file exists.
#'
#' @param path File or directory path.
#' @return The value produced by `read_matrix_if_exists`.
read_matrix_if_exists <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  as.matrix(read.csv(path))
}


#' Read a numeric CSV vector when the file exists.
#'
#' @param path File or directory path.
#' @return The value produced by `read_vector_if_exists`.
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


#' Read a CSV data frame when the file exists.
#'
#' @param path File or directory path.
#' @return The value produced by `read_data_frame_if_exists`.
read_data_frame_if_exists <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  read.csv(path, check.names = FALSE)
}


#' Check whether a list contains at least one non-null entry.
#'
#' @param x Input object.
#' @return The value produced by `list_has_values`.
list_has_values <- function(x) {
  any(vapply(x, function(value) !is.null(value), logical(1)))
}


#' Extract final objectives and histories from RGCCA criterion traces.
#'
#' @param crit Criterion history returned by RGCCA.
#' @param n_comp Number of components.
#' @return The value produced by `objective_from_crit`.
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


#' Create a component diagnostics table from objective values.
#'
#' @param objective Objective value vector.
#' @return The value produced by `component_diagnostics_from_objective`.
component_diagnostics_from_objective <- function(objective) {
  data.frame(
    component = seq_along(objective),
    objective = objective
  )
}


#' Load diagnostics written by a C++ RGCCA executable.
#'
#' @param path_results Directory containing result files.
#' @param n_comp Number of components.
#' @return The value produced by `load_cpp_diagnostics`.
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
    active_blocks = vector("list", n_comp),
    block_importance = vector("list", n_comp),
    block_importance_p_values = vector("list", n_comp),
    block_importance_significant = vector("list", n_comp),
    inactive_block_signal_actions = vector("list", n_comp)
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

    out$block_importance[[h]] <- read_vector_if_exists(
      file.path(path_results, paste0("block_importance", h, ".csv"))
    )
    out$block_importance_p_values[[h]] <- read_vector_if_exists(
      file.path(path_results, paste0("block_importance_p_values", h, ".csv"))
    )
    block_importance_significant_h <- read_vector_if_exists(
      file.path(path_results, paste0("block_importance_significant", h, ".csv"))
    )
    if (!is.null(block_importance_significant_h)) {
      block_importance_significant_h <- as.logical(block_importance_significant_h)
    }
    out$block_importance_significant[[h]] <- block_importance_significant_h
    out$inactive_block_signal_actions[[h]] <- read_vector_if_exists(
      file.path(path_results, paste0("inactive_block_signal_actions", h, ".csv"))
    )
  }

  out
}


#' Attach C++ diagnostics to a model result container.
#'
#' @param model Fitted model object in the standard result-container format.
#' @param diagnostics Diagnostics loaded from C++ CSV outputs.
#' @param n_comp Number of components.
#' @param n_groups Number of data blocks.
#' @return The value produced by `attach_cpp_diagnostics`.
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
  if (list_has_values(diagnostics$block_importance)) {
    model$model_selection$block_importance <- diagnostics$block_importance
  }
  if (list_has_values(diagnostics$block_importance_p_values)) {
    model$model_selection$block_importance_p_values <- diagnostics$block_importance_p_values
  }
  if (list_has_values(diagnostics$block_importance_significant)) {
    model$model_selection$block_importance_significant <- diagnostics$block_importance_significant
  }
  if (list_has_values(diagnostics$inactive_block_signal_actions)) {
    model$model_selection$inactive_block_signal_actions <- diagnostics$inactive_block_signal_actions
  }
  if (!is.null(diagnostics$component_diagnostics)) {
    model$diagnostics$component_diagnostics <- diagnostics$component_diagnostics
  }

  model
}


#' Load bootstrap-selection artifacts written by a C++ RGCCA executable.
#'
#' @param path_results Directory containing result files.
#' @param n_comp Number of components.
#' @param n_groups Number of data blocks.
#' @param grid_D Whether spatial grid outputs should be loaded.
#' @return The value produced by `load_cpp_bootstrap_selection`.
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
      B_used = if (!is.null(B_used_by_lambda) &&
        !is.null(metadata) &&
        "lambda_opt_index" %in% names(metadata)) {
        idx <- metadata$lambda_opt_index[1]
        if (!is.na(idx) && idx >= 1 && idx <= length(B_used_by_lambda)) {
          B_used_by_lambda[idx]
        } else {
          NULL
        }
      } else {
        NULL
      },
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


#' Create a normalized RGCCA model-options list from named arguments.
#'
#' Arguments are explicit so IDEs can suggest available RGCCA options while
#' editing test fixtures and option generators.
#'
#' @param n_comp Number of components.
#' @param C RGCCA connection matrix or token.
#' @param max_iter Maximum number of solver iterations.
#' @param tol Solver tolerance.
#' @param cache_covariances Whether C++ drivers should cache covariances.
#' @param bias Whether covariance estimates are biased.
#' @param init_strategy Initialization strategy.
#' @param lambda_selection_weights Whether bootstrap weight selection is enabled.
#' @param lambda_selection_components Component-selection strategy.
#' @param lambda_components Optional component regularization token or values.
#' @param block_deactivation Whether bootstrap block deactivation is enabled.
#' @param connection_deactivation Whether bootstrap connection deactivation is enabled.
#' @param component_significance Whether component significance is estimated.
#' @param block_importance Whether block importance is estimated.
#' @param inactive_block_signal_test Whether inactive-block residual signal gating is enabled.
#' @param mode RGCCA mode token.
#' @param weight_sign_constraint Weight sign constraint token.
#' @param deflation_mode Deflation mode.
#' @param scheme RGCCA scheme.
#' @param solver Optional solver name stored in fitted-model metadata.
#' @param n_obs Optional observation count stored in fitted-model metadata.
#' @param lambda Optional lambda value stored in fitted-model metadata.
#' @param tau Optional tau value stored in fitted-model metadata.
#' @param non_negative_weights Optional non-negative-weight flag.
#' @param lambda_grid Optional lambda grid stored in fitted-model metadata.
#' @param include_defaults Whether missing options should be filled from defaults.
#' @return The value produced by `rgcca_model_options`.
rgcca_model_options <- function(n_comp = 3,
                                C = "data",
                                max_iter = 1000,
                                tol = 1e-8,
                                cache_covariances = TRUE,
                                bias = TRUE,
                                ...,
                                init_strategy = "svd",
                                lambda_selection_weights = FALSE,
                                lambda_selection_components = "Automatic",
                                lambda_components = NULL,
                                block_deactivation = FALSE,
                                connection_deactivation = FALSE,
                                component_significance = FALSE,
                                block_importance = FALSE,
                                inactive_block_signal_test = FALSE,
                                mode = "Default",
                                weight_sign_constraint = "Default",
                                deflation_mode = "Scores",
                                scheme = "Factorial",
                                solver = NULL,
                                n_obs = NULL,
                                lambda = NULL,
                                tau = NULL,
                                non_negative_weights = NULL,
                                lambda_grid = NULL,
                                include_defaults = TRUE) {
  extra_args <- list(...)
  if (length(extra_args) > 0) {
    stop(
      paste0("unused argument(s): ", paste(names(extra_args), collapse = ", ")),
      call. = FALSE
    )
  }
  supplied_options <- setdiff(
    names(as.list(match.call(expand.dots = FALSE)))[-1],
    c("include_defaults", "...")
  )
  init_strategy <- match_string_option(
    init_strategy,
    c(default = "Default", none = "None", svd = "SVD", uniform = "Uniform", warmstart = "WarmStart"),
    "init_strategy"
  )
  lambda_selection_components <- match_string_option(
    lambda_selection_components,
    c(default = "Default", manual = "Manual", auto = "Automatic", automatic = "Automatic"),
    "lambda_selection_components"
  )
  mode <- match_string_option(
    mode,
    c(
      default = "Default",
      cor = "CorMax", cormax = "CorMax", correlation = "CorMax",
      regularized = "Regularized", rgcca = "Regularized", reg = "Regularized",
      cov = "CovMax", covmax = "CovMax", covariance = "CovMax"
    ),
    "mode"
  )
  weight_sign_constraint <- match_string_option(
    weight_sign_constraint,
    c(default = "Default", none = "None", nonnegative = "NonNegative", nn = "NonNegative"),
    "weight_sign_constraint"
  )
  deflation_mode <- match_string_option(
    deflation_mode,
    c(default = "Default", none = "None", scores = "Scores", score = "Scores"),
    "deflation_mode"
  )
  scheme <- match_string_option(
    scheme,
    c(default = "Default", horst = "Horst", centroid = "Centroid", factorial = "Factorial"),
    "scheme"
  )
  model_options <- list(
    n_comp = n_comp,
    C = C,
    max_iter = max_iter,
    tol = tol,
    cache_covariances = cache_covariances,
    bias = bias,
    init_strategy = init_strategy,
    lambda_selection_weights = lambda_selection_weights,
    lambda_selection_components = lambda_selection_components,
    block_deactivation = block_deactivation,
    connection_deactivation = connection_deactivation,
    component_significance = component_significance,
    block_importance = block_importance,
    inactive_block_signal_test = inactive_block_signal_test,
    mode = mode,
    weight_sign_constraint = weight_sign_constraint,
    deflation_mode = deflation_mode,
    scheme = scheme
  )
  optional_options <- list(
    lambda_components = lambda_components,
    solver = solver,
    n_obs = n_obs,
    lambda = lambda,
    tau = tau,
    non_negative_weights = non_negative_weights,
    lambda_grid = lambda_grid
  )
  all_options <- modifyList(model_options, optional_options, keep.null = FALSE)
  if (!isTRUE(include_defaults)) {
    model_options <- all_options[intersect(supplied_options, names(all_options))]
    return(model_options)
  }
  model_options <- all_options
  resolve_rgcca_model_options(model_options)
}


#' Create a normalized RGCCA bootstrap-options list from named arguments.
#'
#' Arguments are explicit so IDEs can suggest available bootstrap options while
#' editing test fixtures and option generators.
#'
#' @param B_max Maximum number of bootstrap resamples.
#' @param B_min Minimum number of bootstrap resamples.
#' @param resampling_strategy Bootstrap resampling strategy.
#' @param stationary_block_length Stationary-resampling block length.
#' @param seed Bootstrap random seed.
#' @param max_threads Maximum number of bootstrap worker threads.
#' @param check_every Bootstrap resamples between adaptive checks.
#' @param check_every_block_deactivation Bootstrap checks between block-deactivation attempts.
#' @param check_every_connection_deactivation Bootstrap checks between connection-deactivation attempts.
#' @param fit_max_iter Bootstrap fit iteration cap, or -1 to use model max_iter.
#' @param adaptive Whether adaptive bootstrap stopping is enabled.
#' @param adaptive_tol Adaptive stopping tolerance.
#' @param stable_checks_required Number of stable checks required before stopping.
#' @param active_block_tol Active-block tolerance.
#' @param active_connection_sign_stability Sign-stability threshold for active connections.
#' @param active_connection_min_abs_corr Minimum absolute correlation for active connections.
#' @param ci_level Confidence interval level.
#' @param patience Adaptive-stopping patience.
#' @param component_significance_resamples Component-significance resample count.
#' @param component_significance_alpha Component-significance alpha.
#' @param block_importance_resamples Block-importance resample count.
#' @param block_importance_alpha Block-importance alpha.
#' @param inactive_block_signal_resamples Residual-signal gate resample count.
#' @param inactive_block_signal_alpha Residual-signal gate alpha.
#' @param save_bootstrap_resamples Whether bootstrap resamples are saved.
#' @param include_defaults Whether missing options should be filled from defaults.
#' @return The value produced by `rgcca_bootstrap_options`.
rgcca_bootstrap_options <- function(B_max = 15000,
                                    B_min = 200,
                                    resampling_strategy = "Ordinary",
                                    stationary_block_length = 0,
                                    seed = 12345,
                                    max_threads = rgcca_max_threads(),
                                    check_every = 100,
                                    check_every_block_deactivation = 1,
                                    check_every_connection_deactivation = 100,
                                    fit_max_iter = 100,
                                    adaptive = TRUE,
                                    adaptive_tol = 1e-3,
                                    stable_checks_required = 3,
                                    active_block_tol = 0.1,
                                    active_connection_sign_stability = 0.95,
                                    active_connection_min_abs_corr = 0.05,
                                    ci_level = 0.95,
                                    patience = 1,
                                    component_significance_resamples = 100,
                                    component_significance_alpha = 0.05,
                                    block_importance_resamples = 100,
                                    block_importance_alpha = 0.05,
                                    inactive_block_signal_resamples = 100,
                                    inactive_block_signal_alpha = 0.05,
                                    save_bootstrap_resamples = FALSE,
                                    include_defaults = TRUE) {
  supplied_options <- setdiff(
    names(as.list(match.call(expand.dots = FALSE)))[-1],
    "include_defaults"
  )
  resampling_strategy <- match_string_option(
    resampling_strategy,
    c(default = "Default", ordinary = "Ordinary", stationary = "Stationary"),
    "resampling_strategy"
  )
  bootstrap_options <- list(
    B_max = B_max,
    B_min = B_min,
    resampling_strategy = resampling_strategy,
    stationary_block_length = stationary_block_length,
    seed = seed,
    max_threads = max_threads,
    check_every = check_every,
    check_every_block_deactivation = check_every_block_deactivation,
    check_every_connection_deactivation = check_every_connection_deactivation,
    fit_max_iter = fit_max_iter,
    adaptive = adaptive,
    adaptive_tol = adaptive_tol,
    stable_checks_required = stable_checks_required,
    active_block_tol = active_block_tol,
    active_connection_sign_stability = active_connection_sign_stability,
    active_connection_min_abs_corr = active_connection_min_abs_corr,
    ci_level = ci_level,
    patience = patience,
    component_significance_resamples = component_significance_resamples,
    component_significance_alpha = component_significance_alpha,
    block_importance_resamples = block_importance_resamples,
    block_importance_alpha = block_importance_alpha,
    inactive_block_signal_resamples = inactive_block_signal_resamples,
    inactive_block_signal_alpha = inactive_block_signal_alpha,
    save_bootstrap_resamples = save_bootstrap_resamples
  )
  if (!isTRUE(include_defaults)) {
    bootstrap_options <- bootstrap_options[intersect(supplied_options, names(bootstrap_options))]
    return(bootstrap_options)
  }
  resolve_rgcca_bootstrap_options(bootstrap_options)
}
