# = ========================================================================== =
# - Script: generate_data.R
# - Desc: Generates synthetic 2D FPCA-style data on a given domain and set of
#         locations using user-provided (or default) loading and mean generators.
# = ========================================================================== =


## Function: generate_2D_fpca_data
# - Args:
#   * domain: list containing at least $fdapde_mesh (nodes) and $femr_mesh
#   * locations: matrix/data.frame of evaluation points (n_locs x 2)
#   * test_options: nested list with fields:
#       - $dimensions$n_stat_units
#       - $model_options$n_comp
#       - $noise$NSR
#   * loadings_generator: function(points, i) -> loading values for component i
#   * mean_generator: function(points) -> mean field values
#   * seed: integer, RNG seed for reproducibility
# - Desc:
#   Builds orthonormalized spatial loadings, samples scores, constructs mean and
#   noise, and returns a full set of true and observed quantities for testing.
generate_2D_fpca_data <- function(domain, locations,
                                  test_options,
                                  loadings_generator = NULL,
                                  mean_generator = NULL,
                                  seed = 0) {
  ## Generators ----
  if (is.null(loadings_generator)) {
    loadings_generator <- cube_eigenfunction
  }
  if (is.null(mean_generator)) {
    mean_generator <- log_mean_generator
  }

  ## Nodes and locations ----
  nodes <- domain$fdapde_mesh$nodes
  locs <- locations

  ## Dimensions ----
  n_stat_units <- test_options$dimensions$n_stat_units
  n_comp <- test_options$model_options$n_comp
  n_nodes <- nrow(nodes)
  n_locs <- nrow(locs)

  ## femR Objects To Compute Functional Norm ----
  Vh <- FunctionSpace(domain$femr_mesh, fe_order = 1)
  u <- Function(Vh)
  Lu <- -laplace(u)
  force <- function(points) {
    return(0 * points[, 1])
  }
  pde <- Pde(Lu, force)

  ## fPCs ----
  loadings_true <- matrix(0, nrow = n_nodes, ncol = n_comp)
  loadings_true_locs <- matrix(0, nrow = n_locs, ncol = n_comp)
  for (i in 1:n_comp) {
    ## Generate loading on nodes
    loadings_true[, i] <- loadings_generator(nodes, i)
    ## Compute L2 norm and normalize
    norm <- norm_A(loadings_true[, i], pde$mass())
    loadings_true[, i] <- loadings_true[, i] / norm
    ## Evaluate loading at locations and normalize consistently
    loadings_true_locs[, i] <- loadings_generator(locs, i) / norm
  }

  ## Scores ----
  ## Compute Scores Standard Deviations
  data_range <- max(loadings_true) - min(loadings_true)
  sd_s <- test_options$data$var_pct
  sigma_s <- sqrt(sd_s) * data_range

  ## Set seed
  set.seed(seed)

  ## Sample scores
  scores_true <- MASS::mvrnorm(n = n_stat_units, mu = rep(0, n_comp), Sigma = diag(sigma_s^2), empirical = T)

  ## Convention: loadings normalised with respect to the l2 norm
  loadings_norms <- numeric(n_comp)
  for (i in 1:n_comp) {
    loadings_norms[i] <- norm_l2(loadings_true_locs[, i])
    ## Loadings
    loadings_true[, i] <- loadings_true[, i] / loadings_norms[i]
    loadings_true_locs[, i] <- loadings_true_locs[, i] / loadings_norms[i]
    ## Scores
    scores_true[, i] <- scores_true[, i] * loadings_norms[i]
  }
  ## Data ----
  X_c_true <- scores_true %*% t(loadings_true)
  X_c_true_locs <- scores_true %*% t(loadings_true_locs)

  ## Mean ----
  ## Compute mean using the generator
  X_mean_true <- mean_generator(nodes)
  X_mean_true_locs <- mean_generator(locs)

  ## Add mean to the data
  X_true <- X_c_true + rep(1, n_stat_units) %*% t(X_mean_true)
  X_true_locs <- X_c_true_locs + rep(1, n_stat_units) %*% t(X_mean_true_locs)

  ## Noise ----
  ## Compute noise sd from signal variance and NSR
  NSR <- test_options$noise$NSR
  sigma_noise_x <- sqrt(NSR * sum(sigma_s^2))
  NSR_X <- NSR

  ## Compute noise matrix (zero-mean)
  EE <- rnorm(n = n_stat_units * n_locs, sd = sigma_noise_x)
  EE <- matrix(EE, nrow = n_stat_units)
  EE <- scale(EE, scale = FALSE) ## enforce zero-mean noise

  ## Add noise to data observed at locations
  X_locs <- X_true_locs + EE

  return(list(
    ## Dimensions
    dimensions = list(
      n_stat_units = n_stat_units,
      n_comp = n_comp,
      n_nodes = n_nodes,
      n_locs = n_locs
    ),
    ## Domain & locations
    domain = domain,
    locations = locs,
    ## Data
    X = X_locs,
    ## Expected results: reconstruction
    X_mean_true = X_mean_true,
    X_mean_true_locs = X_mean_true_locs,
    X_c_true = X_c_true,
    X_c_true_locs = X_c_true_locs,
    X_true = X_true,
    X_true_locs = X_true_locs,
    ## Expected results: decomposition
    loadings_norms = loadings_norms,
    loadings_true = loadings_true,
    loadings_true_locs = loadings_true_locs,
    scores_true = scores_true,
    ## Computed quantities
    sd_s = sd_s,
    sigma_s = sigma_s,
    sigma_noise_x = sigma_noise_x,
    NSR_X = NSR_X,
    ## Generators
    loadings_generator = loadings_generator,
    mean_generator = mean_generator
  ))
}
