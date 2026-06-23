#' Compute evaluation metrics for one fitted model against generated truth.
#'
#' @param model Fitted model object in the standard result-container format.
#' @param data Generated data and truth object.
#' @return The value produced by `evaluate_results`.
evaluate_results <- function(model, data) {

  ## Dimensions ----
  n_comp <- data$dimensions$n_comp
  n_groups <- data$dimensions$n_groups

  normalized_rmse <- function(estimate, truth) {
    if (is.null(estimate) || is.null(truth)) {
      return(NaN)
    }
    norm <- RMSE(truth)
    norm <- ifelse(norm == 0, 1, norm)
    RMSE(estimate - truth) / norm
  }

  diagnostic_vector <- function(model, name) {
    values <- model$diagnostics$component_diagnostics[[name]]
    if (is.null(values)) {
      values <- rep(NaN, n_comp)
    }
    values <- values[seq_len(min(length(values), n_comp))]
    if (length(values) < n_comp) {
      values <- c(values, rep(NaN, n_comp - length(values)))
    }
    values
  }

  bootstrap_resamples <- function(model) {
    out <- diagnostic_vector(model, "bootstrap_count")
    bootstrap <- model$model_selection$bootstrap
    if (!is.null(bootstrap)) {
      for (h in seq_len(min(length(bootstrap), n_comp))) {
        if (!is.null(bootstrap[[h]]$B)) {
          out[h] <- bootstrap[[h]]$B
        }
      }
    }
    out
  }

  selection_metrics <- function(model, data) {
    truth <- data$model_selection_truth
    lambda_weights <- model$model_selection$lambda_weights %||% rep(NaN, n_comp)
    lambda_weights <- lambda_weights[seq_len(min(length(lambda_weights), n_comp))]
    if (length(lambda_weights) < n_comp) {
      lambda_weights <- c(lambda_weights, rep(NaN, n_comp - length(lambda_weights)))
    }

    out <- list(
      lambda_weights = lambda_weights,
      active_blocks_accuracy = rep(NaN, n_comp),
      active_blocks_false_positive = rep(NaN, n_comp),
      active_blocks_false_negative = rep(NaN, n_comp),
      active_blocks_selected = rep(NaN, n_comp),
      active_connections_accuracy = rep(NaN, n_comp),
      active_connections_false_positive = rep(NaN, n_comp),
      active_connections_false_negative = rep(NaN, n_comp),
      active_connections_selected = rep(NaN, n_comp)
    )

    if (is.null(truth)) {
      return(out)
    }

    selected_blocks <- model$model_selection$active_blocks
    selected_connections <- model$model_selection$C
    for (h in seq_len(n_comp)) {
      truth_blocks <- truth$active_blocks[[h]]
      est_blocks <- selected_blocks[[h]]
      if (!is.null(est_blocks) && length(est_blocks) == length(truth_blocks)) {
        est_blocks <- as.logical(est_blocks)
        truth_blocks <- as.logical(truth_blocks)
        out$active_blocks_accuracy[h] <- mean(est_blocks == truth_blocks)
        out$active_blocks_false_positive[h] <- sum(est_blocks & !truth_blocks)
        out$active_blocks_false_negative[h] <- sum(!est_blocks & truth_blocks)
        out$active_blocks_selected[h] <- sum(est_blocks)
      }

      truth_connections <- truth$active_connections[[h]]
      est_connections <- selected_connections[[h]]
      if (!is.null(est_connections) &&
          all(dim(as.matrix(est_connections)) == dim(truth_connections))) {
        est_connections <- as.matrix(est_connections) != 0
        truth_connections <- as.matrix(truth_connections) != 0
        idx <- upper.tri(truth_connections)
        out$active_connections_accuracy[h] <- mean(est_connections[idx] == truth_connections[idx])
        out$active_connections_false_positive[h] <- sum(est_connections[idx] & !truth_connections[idx])
        out$active_connections_false_negative[h] <- sum(!est_connections[idx] & truth_connections[idx])
        out$active_connections_selected[h] <- sum(est_connections[idx])
      }
    }

    out
  }

  ## Room for results ----
  rmse <- list()
  irmse <- list()
  lambdas <- list()

  ## Execution time ----
  execution_time <- model$diagnostics$execution_time

  ## Lambdas ----
  # lambda <- model$results$lambda

  ## RMSE at locations ----

  ## Reconstruction
  # rmse$X_locs <- c()
  # for(g in 1:n_groups) {
  #   norm <- RMSE(data$X_locs[[g]])
  #   norm <- ifelse(norm == 0, 1, norm)
  #   rmse$X[g] <- RMSE(model$results$X_hat_locs[[g]] - data$X_locs[[g]]) / norm
  # }

  ## Components
  for(g in 1:n_groups) {
    rmse[[paste0("H", g)]] <- c()
    for(h in 1:n_comp) {
      norm <- RMSE(data$H[[g]][, h])
      norm <- ifelse(norm == 0, 1, norm)
      rmse[[paste0("H", g)]][h] <- RMSE(model$results$H_hat[[g]][, h] - data$H[[g]][, h]) / norm
    }
    rmse[[paste0("H", g, "_all")]] <- normalized_rmse(
      model$results$H_hat[[g]],
      data$H[[g]]
    )
  }

  ## Loadings
  for(g in 1:n_groups) {
    rmse[[paste0("A", g, "_locs")]] <- c()
    for(h in 1:n_comp) {
      norm <- RMSE(data$A_locs[[g]][, h])
      norm <- ifelse(norm == 0, 1, norm)
      rmse[[paste0("A", g, "_locs")]][h] <- RMSE(model$results$A_hat_locs[[g]][, h] - data$A_locs[[g]][, h]) / norm
    }
    rmse[[paste0("A", g, "_locs_all")]] <- normalized_rmse(
      model$results$A_hat_locs[[g]],
      data$A_locs[[g]]
    )
  }

  ## Loadings star
  for(g in 1:n_groups) {
    rmse[[paste0("A_star", g, "_locs")]] <- c()
    for(h in 1:n_comp) {
      norm <- RMSE(data$A_locs[[g]][, h])
      norm <- ifelse(norm == 0, 1, norm)
      rmse[[paste0("A_star", g, "_locs")]][h] <- RMSE(model$results$A_star_hat_locs[[g]][, h] - data$A_locs[[g]][, h]) / norm
    }
    rmse[[paste0("A_star", g, "_locs_all")]] <- normalized_rmse(
      model$results$A_star_hat_locs[[g]],
      data$A_locs[[g]]
    )
  }

  ### RMSE at grid (if possible) ----
  if (isTRUE(model$model_traits$has_interpolator)) {
    # norm <- ifelse(RMSE(data$...) == 0, 1, RMSE(data$...))
    # rmse$... <- RMSE(model$results$... - data$...) / norm
  }

  ## IRMSE (if possible) ----
  if (isTRUE(model$model_traits$is_functional)) {
    # norm <- IRMSE(data$..., model$R0())
    # norm <- ifelse(norm == 0, 1, norm)
    # irmse$... <- IRMSE(model$results$... - data$..., model$R0()) / norm
  }

  return(list(
    execution_time = execution_time,
    lambdas = model$model_selection$lambda_weights %||% c(),
    model_selection = selection_metrics(model, data),
    diagnostics = list(
      iterations = diagnostic_vector(model, "iters"),
      bootstrap_resamples = bootstrap_resamples(model)
    ),
    objective = model$diagnostics$component_diagnostics$objective %||% c(),
    rmse = rmse,
    irmse = irmse
  ))
}
