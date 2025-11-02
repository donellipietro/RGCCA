# = ========================================================================== =
# - Script: generate_data.R
# - Desc: Generates synthetic data on a given domain and set of locations
# = ========================================================================== =


## Function: generate_2D_fpca_data
# - Args:
#   * domain: list containing at least $fdapde_mesh (nodes) and $femr_mesh
#   * locations: matrix/data.frame of evaluation points (n_locs x 2)
#   * test_options: nested list with fields:
#     -    
#   * seed: integer, RNG seed for reproducibility
# - Desc:
#   .... 
generate_2D_fpca_data <- function(domain, locations,
                                  test_options, seed = 0) {
  
  ## Nodes and locations ----
  nodes <- domain$fdapde_mesh$nodes
  locs <- locations
  
  ## Dimensions ----
  n_stat_units <- test_options$dimensions$n_stat_units
  n_nodes <- nrow(nodes)
  n_locs <- nrow(locs)
  
  ## femR Objects To Compute Functional Norm ----
  # Vh <- FunctionSpace(domain$femr_mesh, fe_order = 1)
  # u <- Function(Vh)
  # Lu <- -laplace(u)
  # force <- function(points) {
  #   return(0 * points[, 1])
  # }
  # pde <- Pde(Lu, force)
  
  ## Data ----
  
  ## Noise ----
  ## Compute noise sd from signal variance and NSR
  NSR <- test_options$noise$NSR
  # sigma_noise <- sqrt(NSR * sum(sigma_s^2))
  
  ## Compute noise matrix (zero-mean)
  # noise <- rnorm(n = ..., sd = sigma_noise)
  # noise <- matrix(EE, nrow = n_stat_units)
  # noise <- scale(EE, scale = FALSE) ## enforce zero-mean noise
  
  ## Add noise to data observed at locations
  # ... <- ... + noise
  
  return(list(
    ## Dimensions
    dimensions = list(
      n_stat_units = n_stat_units,
      n_nodes = n_nodes,
      n_locs = n_locs
    ),
    ## Domain & locations
    domain = domain,
    locations = locs,
    ## Data
    # ...
    ## Expected results
    # ...
    ## Computed quantities
    sigma_noise = sigma_noise,
    NSR = NSR,
  ))
}
