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
  
  ## Room for results ----
  rmse <- list()
  irmse <- list()
  lambda <- NULL
  
  ## Execution time ----
  execution_time <- model$results$execution_time
  
  ## Lambdas ----
  lambda <- model$results$lambda
  
  ## RMSE againts fda ----
  if (model$model_traits$discretization == "splines") {
    rmse$f_coeffs <- max(abs(model$results$f_coeffs - data$f_coeffs))
    rmse$gcv_scores <- max(abs(model$results$gcv_scores - data$gcv_scores))
  }
  
  ## RMSE at locations ----
  norm <- ifelse(RMSE(data$f_locs) == 0, 1, RMSE(data$f_locs))
  rmse$f_locs <- RMSE(model$results$f_locs - data$f_locs) / norm
  
  ### RMSE at grid (if possible) ----
  if (model$model_traits$has_interpolator) {
    norm <- ifelse(RMSE(data$f_grid) == 0, 1, RMSE(data$f_grid))
    rmse$f_grid <- RMSE(model$results$f_grid - data$f_grid) / norm
  }
  
  ## IRMSE (if possible) ----
  if (model$model_traits$is_functional) {
    # norm <- IRMSE(data$..., model$R0())
    # norm <- ifelse(norm == 0, 1, norm)
    # irmse$... <- IRMSE(model$results$... - data$..., model$R0()) / norm
  }
  
  return(list(
    execution_time = execution_time,
    lambda = lambda,
    rmse = rmse,
    irmse = irmse
  ))
}