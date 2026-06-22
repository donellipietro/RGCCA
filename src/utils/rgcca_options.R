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


connection_to_json <- function(C) {
  lapply(seq_len(nrow(C)), function(i) as.integer(C[i, ] != 0))
}


collect_cpp_model_options <- function(model_options) {
  option_names <- c(
    "max_iter", "tol", "verbose", "cache_covariances", "bias",
    "init", "init_strategy",
    "lambda_selection_components", "lambda_components",
    "block_deactivation", "connection_deactivation", "component_significance",
    "mode", "weight_sign_constraint", "deflation", "deflation_mode", "scheme",
    "resampling_strategy", "stationary_block_length",
    "bootstrap_seed", "bootstrap_max_threads", "bootstrap_B_min",
    "bootstrap_B_max", "bootstrap_B_per_thread_per_batch",
    "bootstrap_adaptive", "bootstrap_adaptive_tol",
    "bootstrap_stable_batches_required", "bootstrap_active_block_tol",
    "bootstrap_active_connection_sign_stability",
    "bootstrap_active_connection_min_abs_corr", "bootstrap_ci_level",
    "bootstrap_patience", "bootstrap_resampling_strategy",
    "bootstrap_stationary_block_length",
    "bootstrap_component_significance_resamples",
    "bootstrap_component_significance_alpha",
    "component_significance_resamples", "component_significance_alpha"
  )

  out <- list()
  for (name in option_names) {
    value <- model_options[[name]]
    if (!is.null(value) && length(value) > 0) {
      out[[name]] <- value
    }
  }
  out
}


read_matrix_if_exists <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  as.matrix(read.csv(path))
}


rgcca_model_options <- function(...) {
  defaults <- list(
    n_comp = 3,
    max_iter = 1000,
    tol = 1e-8,
    verbose = FALSE,
    cache_covariances = TRUE,
    bias = TRUE,
    init_strategy = "Default",
    lambda_selection_weights = FALSE,
    lambda_selection_components = "Automatic",
    block_deactivation = FALSE,
    connection_deactivation = FALSE,
    component_significance = FALSE,
    mode = "Default",
    weight_sign_constraint = "Default",
    deflation_mode = "Scores",
    scheme = "Factorial",
    n_bootstrap_samples = 0,
    resampling_strategy = "Ordinary",
    stationary_block_length = 0,
    bootstrap_seed = 12345,
    bootstrap_max_threads = 12,
    bootstrap_B_per_thread_per_batch = 5,
    bootstrap_adaptive = TRUE,
    bootstrap_adaptive_tol = 1e-3,
    bootstrap_stable_batches_required = 3,
    bootstrap_active_block_tol = 1e-8,
    bootstrap_active_connection_sign_stability = 0.95,
    bootstrap_active_connection_min_abs_corr = 0.05,
    bootstrap_ci_level = 0.95,
    bootstrap_patience = 1,
    component_significance_resamples = 100,
    component_significance_alpha = 0.05
  )

  modifyList(defaults, list(...), keep.null = FALSE)
}
