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
  
  ## RMSE at locations ----
  
  # norm <- ifelse(RMSE(data$...) == 0, 1, RMSE(data$...))
  # rmse$... <- RMSE(model$results$... - data$...) / norm
  
  
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
    lambdas = lambdas,
    rmse = rmse,
    irmse = irmse
  ))
}