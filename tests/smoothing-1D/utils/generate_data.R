# = ========================================================================== =
# - Script: generate_data.R
# - Desc: Generates synthetic data on a given domain and set of locations
# = ========================================================================== =


## Function: generate_data
# - Args:
#   * domain: list containing at least $fdapde_mesh (knots) and $femr_mesh
#   * locations: matrix/data.frame of evaluation points (n_locs x 1)
#   * test_options: nested list with fields:
#     -    
#   * seed: integer, RNG seed for reproducibility
# - Desc:
#   .... 
generate_data <- function(domain, locations, grid, test_options, seed = 0) {
  
  ## Nodes and locations ----
  knots <- domain$knots
  locs <- locations
  grid <- grid
  
  ## Dimensions ----
  n_knots <- length(knots)
  n_locs <- length(locs)
  
  ## femR Objects To Compute Functional Norm ----
  # Vh <- FunctionSpace(domain$femr_mesh, fe_order = 1)
  # u <- Function(Vh)
  # Lu <- -laplace(u)
  # force <- function(points) {
  #   return(0 * points[, 1])
  # }
  # pde <- Pde(Lu, force)
  
  ## Data ----
  f_grid <- sin(2*pi*grid)
  f_locs <- sin(2*pi*locs)
  
  ## fda splines ---

  ## Noise ----
  ## Compute noise sd from signal variance and NSR
  NSR <- test_options$noise$NSR
  sigma_noise <- sqrt(NSR * var(f_locs))
  
  ## Compute noise matrix (zero-mean)
  noise <- rnorm(n = n_locs, sd = sigma_noise)
  noise <- scale(noise, scale = FALSE) ## enforce zero-mean noise
  
  ## Add noise to data observed at locations
  z <- f_locs + noise
  
  data <- list(
    ## Dimensions
    dimensions = list(
      n_knots = n_knots,
      n_locs = n_locs
    ),
    ## Domain & locations
    domain = domain,
    locations = locs,
    grid = grid,
    ## Data
    z = z,
    ## Expected results
    f_grid = f_grid,
    f_locs = f_locs,
    ## Computed quantities
    sigma_noise = sigma_noise,
    NSR = NSR
  )
  
  model_fda <- fit_model("fda_splines", data, test_options, path_list)
  data$f_coeffs <- model_fda$results$f_coeffs
  data$gcv_scores <- model_fda$results$gcv_scores
  data$lambda <- model_fda$results$lambda
  
  return(data)
}
