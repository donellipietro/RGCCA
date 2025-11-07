# = ========================================================================== =
# - Script: generate_data.R
# - Desc: Generates synthetic data on a given domain and set of locations
# = ========================================================================== =


generate_data <- function(test_options, seed = 0) {
  
  ## Weights functions ----
  a_gen <- function(x, id) {
    if (id == 0) return(0*x)
    if (id == 1) return(exp(-400*(x-1/4)^2))
    if (id == 2) return(exp(-400*(x-3/4)^2))
    if (id == 3) return(exp(-400*(x-1/2)^2))
  }
  e_gen <- function(t, w){
    if (w == 0) return(0*t)
    else return(cos(w*pi*t))
  }
  
  ## Connectivity matrix ----
  C <- matrix(
    c(0, 1, 1, 0,
      1, 0, 0, 1,
      1, 0, 0, 0,
      0, 1, 0, 0),
    ncol = 4, nrow = 4, byrow = TRUE
  )
  
  ## Dimensions ----
  n_groups <- test_options$dimensions$n_groups
  n_comp <- test_options$model_options$n_comp
  n_nodes_D <- test_options$dimensions$n_nodes_D
  n_nodes_T <- test_options$dimensions$n_nodes_T
  n_locs_D <- test_options$dimensions$n_locs_D
  n_locs_T <- test_options$dimensions$n_locs_T
  n_nodes_HR_grid_D <- test_options$dimensions$n_nodes_HR_grid_D
  n_nodes_HR_grid_T <- test_options$dimensions$n_nodes_HR_grid_T
  
  ## Nodes and locations in space ----
  domain_D <- generate_domain(
    test_options$domain_and_locations$name_mesh,
    n_nodes_D
  )
  locs_D <- generate_locations(
    domain_D,
    test_options$domain_and_locations$locs_eq_nodes,
    n_locs_D
  )
  grid_D <- generate_locations(
    domain_D,
    FALSE,
    n_nodes_HR_grid_D
  )
  
  ## Nodes and locations in time ----
  domain_T <- generate_domain(
    test_options$domain_and_locations$name_mesh,
    n_nodes_T
  )
  locs_T <- generate_locations(
    domain_T,
    test_options$domain_and_locations$locs_eq_nodes,
    n_locs_T
  )
  grid_T <- generate_locations(
    domain_T,
    FALSE,
    n_nodes_HR_grid_T
  )
  
  
  ## Data ----
  
  ## Update dimensions
  n_locs_D <- length(locs_D)
  n_locs_T <- length(locs_T)
  
  ## Room for loadings and components
  A_locs <- list()
  A_grid <- list()
  E_locs <- list()
  E_grid <- list()
  
  ## Group 1
  A_locs[[1]] <- cbind(    a_gen(locs_D, 1),     a_gen(locs_D, 2),     a_gen(locs_D, 3))
  E_locs[[1]] <- cbind(1.0*e_gen(locs_T, 2), 0.7*e_gen(locs_T, 3), 0.5*e_gen(locs_T, 17))
  A_grid[[1]] <- cbind(    a_gen(grid_D, 1),     a_gen(grid_D, 2),     a_gen(grid_D, 3))
  E_grid[[1]] <- cbind(1.0*e_gen(grid_T, 2), 0.7*e_gen(grid_T, 3), 0.5*e_gen(grid_T, 17))
  
  ## Group 2
  A_locs[[2]] <- cbind(    a_gen(locs_D, 2),     a_gen(locs_D, 1),     a_gen(locs_D, 0))
  E_locs[[2]] <- cbind(1.0*e_gen(locs_T, 4), 0.7*e_gen(locs_T, 3), 0.5*e_gen(locs_T, 0))
  A_grid[[2]] <- cbind(    a_gen(grid_D, 2),     a_gen(grid_D, 1),     a_gen(grid_D, 0))
  E_grid[[2]] <- cbind(1.0*e_gen(grid_T, 4), 0.7*e_gen(grid_T, 3), 0.5*e_gen(grid_T, 0))
  
  ## Group 3
  A_locs[[3]] <- cbind(    a_gen(locs_D, 1),     a_gen(locs_D, 0),     a_gen(locs_D, 0))
  E_locs[[3]] <- cbind(1.0*e_gen(locs_T, 2), 0.7*e_gen(locs_T, 0), 0.5*e_gen(locs_T, 0))
  A_grid[[3]] <- cbind(    a_gen(grid_D, 1),     a_gen(grid_D, 0),     a_gen(grid_D, 0))
  E_grid[[3]] <- cbind(1.0*e_gen(grid_T, 2), 0.7*e_gen(grid_T, 0), 0.5*e_gen(grid_T, 0))
  
  ## Group 4
  A_locs[[4]] <- cbind(    a_gen(locs_D, 1),     a_gen(locs_D, 0),     a_gen(locs_D, 2))
  E_locs[[4]] <- cbind(1.0*e_gen(locs_T, 4), 0.7*e_gen(locs_T, 0), 0.5*e_gen(locs_T, 5))
  A_grid[[4]] <- cbind(    a_gen(grid_D, 1),     a_gen(grid_D, 0),     a_gen(grid_D, 2))
  E_grid[[4]] <- cbind(1.0*e_gen(grid_T, 4), 0.7*e_gen(grid_T, 0), 0.5*e_gen(grid_T, 5))
  
  ## Normalize
  for(g in 1:n_groups) {
    for(h in 1:n_comp) {
      norm <- sqrt(var(E_locs[[g]][, h]))
      norm <- ifelse(norm == 0, 1, norm)
      E_locs[[g]][, h] <- E_locs[[g]][, h] / norm
      E_grid[[g]][, h] <- E_grid[[g]][, h] / norm
      A_locs[[g]][, h] <- A_locs[[g]][, h] * norm
      A_grid[[g]][, h] <- A_grid[[g]][, h] * norm
    }
  }
  
  ## Room for data
  X_locs <- list()
  X_grid <- list()
  
  ## Assemble data from components and loadings
  for(g in 1:n_groups) {
    X_locs[[g]] <- E_locs[[g]] %*% t(A_locs[[g]])
    X_grid[[g]] <- E_locs[[g]] %*% t(A_grid[[g]])
  }
  
  ## Noise ----
  
  ## Get the desired noise sd
  sigma_noise <- test_options$noise$sigma_noise
  
  
  ## Compute noise matrix (zero-mean)
  set.seed(seed)
  noise <- list()
  for(g in 1:n_groups) {
    EE <- rnorm(n = n_locs_D*n_locs_T, sd = sigma_noise)
    EE <- matrix(EE, nrow = n_locs_T)
    noise[[g]] <- scale(EE, scale = FALSE) ## enforce zero-mean noise
  }
  
  ## Add noise to data observed at locations
  X <- list()
  for(g in 1:n_groups) {
    X[[g]] <- X_locs[[g]] + noise[[g]]
    rownames(X[[g]]) <- paste0("t", 1:n_locs_T)
    colnames(X[[g]]) <- paste0("g", g, "p", 1:n_locs_D)
  }
  names(X) <- paste0(X, 1:n_groups)

  
  return(list(
    ## Dimensions
    dimensions = list(
      n_comp = n_comp,
      n_groups = n_groups,
      n_nodes_D = n_nodes_D,
      n_nodes_T = n_nodes_T,
      n_locs_D = n_locs_D,
      n_locs_T = n_locs_T,
      n_nodes_HR_grid_D = n_nodes_HR_grid_D,
      n_nodes_HR_grid_T = n_nodes_HR_grid_T
    ),
    ## Domain, locations and grid (space and time)
    domain_D = domain_D,
    locations_D = locs_D,
    grid_D = grid_D,
    domain_T = domain_T,
    locations_T = locs_T,
    grid_T = grid_T,
    ## Data
    X = X,
    C = C,
    ## Expected results
    X_locs = X_locs,
    X_grid = X_grid,
    ## Expected decomposition
    A_locs = A_locs,
    E_locs = E_locs,
    A_grid = A_grid,
    E_grid = E_grid,
    ## Computed quantities
    sigma_noise = sigma_noise
  ))
}
