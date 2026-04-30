generate_data <- function(test_options, seed = 0) {
  
  ## Helpers ----
  a_gen <- function(x, id) {
    if (id == 0) return(0 * x)
    if (id == 1) return(exp(-80 * (x - 1/4)^2))
    if (id == 2) return(exp(-80 * (x - 3/4)^2))
    if (id == 3) return(exp(-80 * (x - 1/2)^2))
    stop("a_gen: unknown id")
  }
  
  e_gen_hz <- function(t, f_hz) {
    if (f_hz == 0) return(0 * t)
    sin(2 * pi * f_hz * t)
  }
  
  ## Define the design matrix ----
  C <- matrix(c(
    0,1,1,1,
    1,0,0,1,
    1,0,0,1,
    1,1,1,0
  ), ncol = 4, byrow = TRUE)
  
  ## Locs resolution -----
  T_sec <- test_options$domain_and_locations$T_sec
  
  delta <- test_options$data$delta
  if(is.null(delta)) {
    n_locs <- test_options$dimensions$n_locs
    if(is.null(n_locs)) error("Both delta and n_locs are NULL")
    else delta <- 1/(n_locs-1)
  }
  
  TR <- test_options$data$TR
  if(is.null(TR)) {
    n_times <- test_options$dimensions$n_times
    if(is.null(n_times)) error("Both TR and n_times are NULL")
    else TR <- T_sec/(n_times-1)
  }
  
  ## Get dimensions -----
  n_groups             <- test_options$dimensions$n_groups
  n_comp               <- test_options$model_options$n_comp
  n_nodes_D            <- test_options$dimensions$n_nodes_D
  n_nodes_T            <- test_options$dimensions$n_nodes_T
  n_nodes_HR_grid_D    <- test_options$dimensions$n_nodes_HR_grid_D
  n_nodes_HR_grid_T    <- test_options$dimensions$n_nodes_HR_grid_T
  
  ## Domains and locations ----
  
  ## Space domain and locations
  domain_D <- generate_domain(test_options$domain_and_locations$name_mesh, n_nodes_D)
  locs_D <- seq(0, 1, by = delta)
  grid_D <- generate_locations(domain_D, FALSE, n_nodes_HR_grid_D)
  
  ## Time domain [0, T_sec], and time points sampled every TR seconds
  domain_T <- generate_domain("interval", n_nodes_T, T_sec = T_sec) 
  locs_T <- seq(0, T_sec, by = TR)
  grid_T <- generate_locations(domain_T, FALSE, n_nodes_HR_grid_T)
  
  ## Update effective dims
  n_locs_D <- length(locs_D)
  n_locs_T <- length(locs_T)
  
  ## Choose sine frequencies (Hz) -----
  f_set1 <- c(2, 3, 6) / T_sec # c(2.1, 2.9, 6.5) / T_sec
  f_set2 <- c(4, 3, 0) / T_sec  # c(4.3, 2.9, 0) / T_sec 
  f_set3 <- c(2, 5, 0) / T_sec  # c(2.1, 5.2, 0) / T_sec
  f_set4 <- c(4, 5, 6) / T_sec # c(4.3, 5.2, 6.5) / T_sec

  ## ----- allocate -----
  A_locs <- A_grid <- E_locs <- E_grid <- vector("list", n_groups)
  
  ## ----- groups -----
  sigma_1 <- 4
  sigma_2 <- 3
  sigma_3 <- 2.5
  
  ## Group 1
  A_locs[[1]] <- cbind(a_gen(locs_D, 1), a_gen(locs_D, 2), a_gen(locs_D, 3))
  E_locs[[1]] <- cbind(sigma_1*e_gen_hz(locs_T, f_set1[1]),
                       sigma_2*e_gen_hz(locs_T, f_set1[2]),
                       sigma_3*e_gen_hz(locs_T, f_set1[3]))
  A_grid[[1]] <- cbind(a_gen(grid_D, 1), a_gen(grid_D, 2), a_gen(grid_D, 3))
  E_grid[[1]] <- cbind(sigma_1*e_gen_hz(grid_T, f_set1[1]),
                       sigma_2*e_gen_hz(grid_T, f_set1[2]),
                       sigma_3*e_gen_hz(grid_T, f_set1[3]))
  
  ## Group 2
  A_locs[[2]] <- cbind(a_gen(locs_D, 2), a_gen(locs_D, 1), a_gen(locs_D, 0))
  E_locs[[2]] <- cbind(-sigma_1*e_gen_hz(locs_T, f_set2[1]),
                       -sigma_2*e_gen_hz(locs_T, f_set2[2]),
                       sigma_3*e_gen_hz(locs_T, f_set2[3]))
  A_grid[[2]] <- cbind(a_gen(grid_D, 2), a_gen(grid_D, 1), a_gen(grid_D, 0))
  E_grid[[2]] <- cbind(-sigma_1*e_gen_hz(grid_T, f_set2[1]),
                       -sigma_2*e_gen_hz(grid_T, f_set2[2]),
                       sigma_3*e_gen_hz(grid_T, f_set2[3]))
  
  ## Group 3
  A_locs[[3]] <- cbind(a_gen(locs_D, 1), a_gen(locs_D, 3), a_gen(locs_D, 0))
  E_locs[[3]] <- cbind(sigma_1*e_gen_hz(locs_T, f_set3[1]),
                       sigma_2*e_gen_hz(locs_T, f_set3[2]),
                       sigma_3*e_gen_hz(locs_T, f_set3[3]))
  A_grid[[3]] <- cbind(a_gen(grid_D, 1), a_gen(grid_D, 3), a_gen(grid_D, 0))
  E_grid[[3]] <- cbind(sigma_1*e_gen_hz(grid_T, f_set3[1]),
                       sigma_2*e_gen_hz(grid_T, f_set3[2]),
                       sigma_3*e_gen_hz(grid_T, f_set3[3]))
  
  ## Group 4
  A_locs[[4]] <- cbind(a_gen(locs_D, 1), a_gen(locs_D, 3), a_gen(locs_D, 2))
  E_locs[[4]] <- cbind(sigma_1*e_gen_hz(locs_T, f_set4[1]),
                       sigma_2*e_gen_hz(locs_T, f_set4[2]),
                       sigma_3*e_gen_hz(locs_T, f_set4[3]))
  A_grid[[4]] <- cbind(a_gen(grid_D, 1), a_gen(grid_D, 3), a_gen(grid_D, 2))
  E_grid[[4]] <- cbind(sigma_1*e_gen_hz(grid_T, f_set4[1]),
                       sigma_2*e_gen_hz(grid_T, f_set4[2]),
                       sigma_3*e_gen_hz(grid_T, f_set4[3]))
  
  # HH <- cbind(E_locs[[1]][,1], E_locs[[2]][,1], E_locs[[3]][,1], E_locs[[4]][,1],
  #             E_locs[[1]][,2], E_locs[[2]][,2], E_locs[[3]][,2], E_locs[[4]][,2],
  #             E_locs[[1]][,3], E_locs[[2]][,3], E_locs[[3]][,3], E_locs[[4]][,3])
  # abs(t(HH) %*% HH / nrow(HH)) > 1e-9
  # pairs(HH, xlim = c(-4,4), ylim = c(-4,4))
  
  ## ----- assemble data and normalize components / balance amplitudes -----
  X_locs <- X_grid <- vector("list", n_groups)
  for (g in 1:n_groups) {
    X_locs[[g]] <- E_locs[[g]] %*% t(A_locs[[g]])
    X_grid[[g]] <- E_locs[[g]] %*% t(A_grid[[g]])
    for (h in 1:n_comp) {
      norm_E <- var(E_locs[[g]][, h])
      norm_E <- if (is.na(norm_E) || norm_E == 0) 1 else sqrt(norm_E)
      norm_A <- norm_l2(A_locs[[g]][, h])
      norm_A <- if (is.na(norm_A) || norm_A == 0) 1 else norm_A
      E_locs[[g]][, h] <- E_locs[[g]][, h] / norm_E
      E_grid[[g]][, h] <- E_grid[[g]][, h] / norm_E
      A_locs[[g]][, h] <- A_locs[[g]][, h] / norm_A
      A_grid[[g]][, h] <- A_grid[[g]][, h] / norm_A
    }
  }

  
  ## ----- add zero-mean noise -----
  sigma_noise <- test_options$noise$sigma_noise
  set.seed(seed)
  noise <- vector("list", n_groups)
  for (g in 1:n_groups) {
    EE <- rnorm(n_locs_D * n_locs_T, sd = sigma_noise)
    EE <- matrix(EE, nrow = n_locs_T)
    noise[[g]] <- scale(EE, scale = FALSE) # enforce zero-mean
  }
  
  X <- vector("list", n_groups)
  for (g in 1:n_groups) {
    X[[g]] <- X_locs[[g]] + noise[[g]]
    rownames(X[[g]]) <- paste0("t", locs_T, "s")
    colnames(X[[g]]) <- paste0("g", g, "p", seq_len(n_locs_D))
  }
  names(X) <- paste0("X", seq_len(n_groups))  # fix bug
  
  ## ----- return -----
  list(
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
    domain_D = domain_D,
    locations_D = locs_D,
    grid_D = grid_D,
    domain_T = domain_T,
    locations_T = locs_T,
    grid_T = grid_T,
    X = X,
    C = C,
    X_locs = X_locs,
    X_grid = X_grid,
    A_locs = A_locs,
    E_locs = E_locs,
    A_grid = A_grid,
    E_grid = E_grid,
    sigma_noise = sigma_noise,
    TR = TR,
    T_sec = T_sec,
    f_groups_hz = list(f_set1 = f_set1, f_set2 = f_set2, f_set3 = f_set3, f_set4 = f_set4)
  )
}