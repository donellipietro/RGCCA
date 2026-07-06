rm(list = ls())
graphics.off()
options(warn = -1)

invisible(suppressMessages(sapply(c(
  ## Competitors
  "RGCCA",
  # discretization
  "fdaPDE", "femR",
  # algebraic utils
  "pracma", "clue",
  # data manipulation
  "MASS",
  # json
  "jsonlite",
  # sampling
  "sf", "sp", "raster",
  # strings
  "glue"
), require, character.only = TRUE)))

## Load general utility functions
source("src/utils/cat.R")
source("src/utils/directories.R")
source("src/utils/options.R")
source("src/utils/mesh_utils.R")
source("src/utils/domain_utils.R")
source("src/utils/error_metrics.R")
source("src/utils/load_results_utils.R")

## Load configuration file
path_this <- get_script_path()
source(paste0(path_this, "config.R"))

## Load test-specific functions
source(paste0("tests/", test_suite, "/utils/wrappers.R"))
source(paste0("tests/", test_suite, "/utils/generate_data.R"))

#' Compute pointwise bootstrap confidence intervals from resamples.
#'
#' @param W_boot Bootstrap weight matrix.
#' @param conf.level Confidence level.
#' @return The value produced by `ci_from_boot`.
ci_from_boot <- function(W_boot, conf.level = 0.95) {
  alpha <- (1 - conf.level) / 2
  cbind(
    lower = apply(W_boot, 1, quantile, probs = alpha, na.rm = TRUE, names = FALSE, type = 7),
    upper = apply(W_boot, 1, quantile, probs = 1 - alpha, na.rm = TRUE, names = FALSE, type = 7)
  )
}

#' Compute the biased covariance used by the bootstrap check script.
#'
#' @param u First numeric vector.
#' @param v Second numeric vector.
#' @return The value produced by `biased_cov`.
biased_cov <- function(u, v) {
  (sum(u * v) - length(u) * mean(u) * mean(v)) / length(u)
}

#' Compute a correlation from two component score vectors.
#'
#' @param eta_j First score vector.
#' @param eta_k Second score vector.
#' @return The value produced by `corr_from_eta`.
corr_from_eta <- function(eta_j, eta_k) {
  var_j <- biased_cov(eta_j, eta_j)
  var_k <- biased_cov(eta_k, eta_k)

  if (var_j <= 0 || var_k <= 0) {
    return(0)
  }

  biased_cov(eta_j, eta_k) / sqrt(var_j * var_k)
}

#' Compute correlation confidence intervals from bootstrap weights.
#'
#' @param w_boot_locs Bootstrap weights evaluated at locations.
#' @param X_blocks List of data blocks.
#' @param conf.level Confidence level.
#' @return The value produced by `corr_ci_from_boot`.
corr_ci_from_boot <- function(w_boot_locs, X_blocks, conf.level = 0.95) {
  alpha <- (1 - conf.level) / 2
  eps <- 1e-12
  J <- length(w_boot_locs)
  B <- ncol(w_boot_locs[[1]])

  lower <- diag(1, J)
  upper <- diag(1, J)

  eta <- vector("list", J)
  for (j in seq_len(J)) {
    eta[[j]] <- X_blocks[[j]] %*% w_boot_locs[[j]]
  }

  for (j in seq_len(J - 1)) {
    for (k in (j + 1):J) {
      z_values <- numeric(B)

      for (b in seq_len(B)) {
        corr <- corr_from_eta(eta[[j]][, b], eta[[k]][, b])
        corr <- min(max(corr, -1 + eps), 1 - eps)
        z_values[b] <- atanh(corr)
      }

      z_low <- quantile(z_values, probs = alpha, na.rm = TRUE, names = FALSE, type = 7)
      z_high <- quantile(z_values, probs = 1 - alpha, na.rm = TRUE, names = FALSE, type = 7)

      lower[j, k] <- lower[k, j] <- tanh(z_low)
      upper[j, k] <- upper[k, j] <- tanh(z_high)
    }
  }

  list(lower = lower, upper = upper)
}

#' Load raw component CSV files from temporary C++ results.
#'
#' @param path_tmp_results Directory containing temporary C++ result files.
#' @param n_groups Number of data blocks.
#' @return The value produced by `load_raw_components`.
load_raw_components <- function(path_tmp_results, n_groups) {
  lapply(seq_len(n_groups), function(g) {
    as.matrix(read.csv(paste0(path_tmp_results, "E", g, "_hat_locs.csv")))
  })
}

#' Reconstruct blocks deflated up to a selected component.
#'
#' @param data Generated data and truth object.
#' @param raw_components Raw component matrices.
#' @param component Component index.
#' @return The value produced by `deflated_blocks_for_component`.
deflated_blocks_for_component <- function(data, raw_components, component) {
  X_blocks <- data$X

  if (component <= 1) {
    return(X_blocks)
  }

  for (h in seq_len(component - 1)) {
    for (g in seq_along(X_blocks)) {
      y <- raw_components[[g]][, h]
      yy <- sum(y^2)

      if (is.finite(yy) && yy > 0) {
        p <- as.vector(t(X_blocks[[g]]) %*% y / yy)
        X_blocks[[g]] <- X_blocks[[g]] - tcrossprod(y, p)
      }
    }
  }

  X_blocks
}

#' Compare confidence intervals from R and C++ calculations.
#'
#' @param r_ci Confidence interval computed in R.
#' @param cpp_ci Confidence interval computed in C++.
#' @param component Component index.
#' @param block Block index.
#' @param lambda_index Lambda-grid index.
#' @param lambda Lambda value.
#' @param scale Scale label.
#' @param source Source label.
#' @return The value produced by `compare_ci`.
compare_ci <- function(r_ci, cpp_ci, component, block, lambda_index, lambda, scale, source) {
  if (is.null(cpp_ci)) {
    return(data.frame(
      component = component,
      block = block,
      lambda_index = lambda_index,
      lambda = lambda,
      scale = scale,
      source = source,
      max_abs_diff = NA_real_,
      mean_abs_diff = NA_real_,
      rmse = NA_real_,
      status = "missing"
    ))
  }

  delta <- as.matrix(r_ci) - as.matrix(cpp_ci)
  finite <- is.finite(delta)

  if (!any(finite)) {
    return(data.frame(
      component = component,
      block = block,
      lambda_index = lambda_index,
      lambda = lambda,
      scale = scale,
      source = source,
      max_abs_diff = NA_real_,
      mean_abs_diff = NA_real_,
      rmse = NA_real_,
      status = "no_finite_values"
    ))
  }

  data.frame(
    component = component,
    block = block,
    lambda_index = lambda_index,
    lambda = lambda,
    scale = scale,
    source = source,
    max_abs_diff = max(abs(delta[finite])),
    mean_abs_diff = mean(abs(delta[finite])),
    rmse = sqrt(mean(delta[finite]^2)),
    status = "ok"
  )
}

test_options <- list(
  name_test = "prova_CI",
  batch_index = 1,
  cpp_script = "RGCCA",
  test_options = list(
    n_reps = 1
  ),
  domain_and_locations = list(
    name_mesh = "unit_interval",
    locs_eq_nodes = FALSE
  ),
  dimensions = list(
    n_groups = 4,
    n_nodes_D = c(101),
    n_nodes_HR_grid_D = 200,
    n = 1200,
    n_locs = 101
  ),
  model_options = rgcca_model_options(
    n_comp = 3,
    init_strategy = "Uniform",
    lambda_selection_weights = TRUE
  ),
  bootstrap_options = rgcca_bootstrap_options(
    save_bootstrap_resamples = TRUE
  ),
  noise = list(
    sigma_noise = 5
  ),
  regularization = list(
    lambda = 1e-4,
    lambda_grid = list(
      10^(-9:-1), 10^(-9:-1), 10^(-9:-1)
    )
  )
)

path_list <- create_paths("prova")
path_list <- update_paths(path_list, "test1", test_options)

data <- generate_data(test_options, seed = 0)

IGNORE_CPP_OUTPUT <- FALSE
results <- fit_model("CPP_fGCCA_cov_FEM", data, path_list, test_options)
raw_components <- load_raw_components(path_list$tmp_results, test_options$dimensions$n_groups)

summary_rows <- list()
idx <- 1

for (h in seq_along(results$model_selection$bootstrap)) {
  boot <- results$model_selection$bootstrap[[h]]
  lambda_index <- which.min(abs(boot$lambda_grid - boot$lambda_opt))
  lambda <- boot$lambda_grid[lambda_index]
  X_corr <- deflated_blocks_for_component(data, raw_components, h)

  for (g in seq_len(test_options$dimensions$n_groups)) {
    r_ci_locs <- ci_from_boot(boot$w_boot_locs[[lambda_index]][[g]])
    r_ci_grid <- ci_from_boot(boot$w_boot_grid[[lambda_index]][[g]])

    summary_rows[[idx]] <- compare_ci(
      r_ci_locs,
      boot$w_ci_locs[[lambda_index]][[g]],
      h, g, lambda_index, lambda,
      "locs",
      "cpp_fdapde_weight_ci"
    )
    idx <- idx + 1

    summary_rows[[idx]] <- compare_ci(
      r_ci_grid,
      boot$w_ci_grid[[lambda_index]][[g]],
      h, g, lambda_index, lambda,
      "grid",
      "cpp_fdapde_weight_ci"
    )
    idx <- idx + 1
  }

  r_corr_ci <- corr_ci_from_boot(boot$w_boot_locs[[lambda_index]], X_corr)

  summary_rows[[idx]] <- compare_ci(
    r_corr_ci$lower,
    boot$corr_ci_low[[lambda_index]],
    h, NA_integer_, lambda_index, lambda,
    "corr_low",
    "cpp_fdapde_corr_ci"
  )
  idx <- idx + 1

  summary_rows[[idx]] <- compare_ci(
    r_corr_ci$upper,
    boot$corr_ci_high[[lambda_index]],
    h, NA_integer_, lambda_index, lambda,
    "corr_high",
    "cpp_fdapde_corr_ci"
  )
  idx <- idx + 1
}

ci_summary <- do.call(rbind, summary_rows)
print(ci_summary)

summary_path <- paste0(path_list$results, "prova_CI_summary.csv")
write.csv(ci_summary, summary_path, row.names = FALSE)
cat(paste0("\nCI comparison summary written to ", summary_path, "\n"))

tolerance <- as.numeric(Sys.getenv("RGCCA_CI_TOL", "1e-10"))
ci_rows <- ci_summary[ci_summary$source %in% c("cpp_fdapde_weight_ci", "cpp_fdapde_corr_ci"), ]
ok_rows <- ci_rows[ci_rows$status == "ok", ]

if (nrow(ok_rows) == 0) {
  stop("No fdaPDE library CI files were loaded. Recompile with: make compile MODEL=RGCCA")
}

missing_rows <- ci_rows[ci_rows$status != "ok", ]
if (nrow(missing_rows) > 0) {
  print(missing_rows)
  stop("Some fdaPDE library CI files were missing or had no finite values.")
}

rows_over_tol <- ok_rows[ok_rows$max_abs_diff > tolerance, ]
if (nrow(rows_over_tol) > 0) {
  print(rows_over_tol)
  stop(glue(
    "fdaPDE library CIs differ from R quantiles above tolerance {tolerance}. ",
    "Max difference: {max(rows_over_tol$max_abs_diff, na.rm = TRUE)}"
  ))
}

cat(glue("\nfdaPDE library CIs match R quantiles within tolerance {tolerance}.\n"))
