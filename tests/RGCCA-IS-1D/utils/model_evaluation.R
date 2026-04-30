# = ========================================================================== =
# - Script: mnodel_evaluation.R
# - Desc: Utilities evaluating performance via RMSE/IRMSE, ezecution times, ....
# = ========================================================================== =


# - Function: evaluate_results
# - Args:
#   * model: fitted model object with $results and $model_traits fields
#   * data: list from data generator with true quantities
# - Desc:
#   Computes RMSE/IRMSE metrics and saves them in a sctuctured format
evaluate_results <- function(model, data) {
  
  ## Dimensions ----
  n_comp <- data$dimensions$n_comp
  n_groups <- data$dimensions$n_groups
  
  ## Room for results ----
  rmse <- list()
  irmse <- list()
  lambdas <- list()
  
  ## Execution time ----
  execution_time <- model$results$execution_time
  
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
  }
  
  ## Loadings
  for(g in 1:n_groups) {
    rmse[[paste0("A", g, "_locs")]] <- c()
    for(h in 1:n_comp) {
      norm <- RMSE(data$A_locs[[g]][, h])
      norm <- ifelse(norm == 0, 1, norm)
      rmse[[paste0("A", g, "_locs")]][h] <- RMSE(model$results$A_hat_locs[[g]][, h] - data$A_locs[[g]][, h]) / norm
    }
  }
  
  ### RMSE at grid (if possible) ----
  if (model$model_traits$has_interpolator) {
    # norm <- ifelse(RMSE(data$...) == 0, 1, RMSE(data$...))
    # rmse$... <- RMSE(model$results$... - data$...) / norm
  }
  
  ## IRMSE (if possible) ----
  if (model$model_traits$is_functional) {
    # norm <- IRMSE(data$..., model$R0())
    # norm <- ifelse(norm == 0, 1, norm)
    # irmse$... <- IRMSE(model$results$... - data$..., model$R0()) / norm
  }
  
  return(list(
    execution_time = execution_time,
    lambdas = model$lambdas,
    objective = model$objective,
    rmse = rmse,
    irmse = irmse
  ))
}