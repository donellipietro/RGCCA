generate_data <- function(test_options, seed = 0) {
  
  ## ----- helpers -----
  a_gen <- function(x, id) {
    if (id == 0) return(0 * x)
    if (id == 1) return(exp(-400 * (x - 1/4)^2))
    if (id == 2) return(exp(-400 * (x - 3/4)^2))
    if (id == 3) return(exp(-400 * (x - 1/2)^2))
    stop("a_gen: unknown id")
  }
  
  e_gen_hz <- function(t, f_hz) {
    if (f_hz == 0) return(0 * t)
    cos(2 * pi * f_hz * t)
  }
  
  nyquist_ok <- function(freqs_hz, TR) {
    fN <- 1 / (2 * TR)
    if (any(freqs_hz > fN)) {
      bad <- freqs_hz[freqs_hz > fN]
      stop(sprintf("Frequencies above Nyquist for TR=%.3f s (f_N=%.3f Hz): %s",
                   TR, fN, paste(bad, collapse = ", ")))
    }
  }
  
  ## ----- design / connectivity -----
  C <- matrix(c(
    0,1,1,0,
    1,0,0,1,
    1,0,0,0,
    0,1,0,0
  ), ncol = 4, byrow = TRUE)
  
  ## ----- locs resolution -----
  T_sec <- test_options$domain_and_locations$T_sec
  delta <- test_options$data$delta
  TR <- test_options$data$TR
  
  ## ----- dimensions (space copied from your options) -----
  n_groups             <- test_options$dimensions$n_groups
  n_comp               <- test_options$model_options$n_comp
  n_nodes_D            <- test_options$dimensions$n_nodes_D
  n_nodes_T            <- test_options$dimensions$n_nodes_T
  n_locs_D             <- test_options$dimensions$n_locs_D
  n_locs_T_requested   <- test_options$dimensions$n_locs_T
  n_nodes_HR_grid_D    <- test_options$dimensions$n_nodes_HR_grid_D
  n_nodes_HR_grid_T    <- test_options$dimensions$n_nodes_HR_grid_T
  
  ## ----- space (unchanged) -----
  domain_D <- generate_domain(test_options$domain_and_locations$name_mesh, n_nodes_D)
  locs_D <- seq(0, 1, by = delta)
  grid_D <- generate_locations(domain_D, FALSE, n_nodes_HR_grid_D)
  
  ## ----- time over [0, T_sec], sampled every TR seconds -----
  domain_T <- generate_domain("interval", n_nodes_T, T_sec = T_sec) 
  locs_T <- seq(0, T_sec, by = TR)
  grid_T <- generate_locations(domain_T, FALSE, n_nodes_HR_grid_T)
  
  ## update effective dims
  n_locs_D <- length(locs_D)
  n_locs_T <- length(locs_T)
  
  ## ----- choose cosine frequencies (Hz) safely below Nyquist -----
  fN <- 1 / (2 * TR)  # Nyquist
  ## You can tweak these; all ≤ 0.25 Hz for TR = 2 s
  f_set1 <- c(2, 3, 7) / T_sec
  f_set2 <- c(4, 3, 0) / T_sec 
  f_set3 <- c(2, 0, 0) / T_sec
  f_set4 <- c(4, 0, 5) / T_sec
  
  nyquist_ok(c(f_set1, f_set2, f_set3, f_set4), TR)
  
  ## ----- allocate -----
  A_locs <- A_grid <- E_locs <- E_grid <- vector("list", n_groups)
  
  ## ----- groups -----
  ## Group 1
  A_locs[[1]] <- cbind(a_gen(locs_D, 1), a_gen(locs_D, 2), a_gen(locs_D, 3))
  E_locs[[1]] <- cbind(1.0*e_gen_hz(locs_T, f_set1[1]),
                       0.7*e_gen_hz(locs_T, f_set1[2]),
                       0.5*e_gen_hz(locs_T, f_set1[3]))
  A_grid[[1]] <- cbind(a_gen(grid_D, 1), a_gen(grid_D, 2), a_gen(grid_D, 3))
  E_grid[[1]] <- cbind(1.0*e_gen_hz(grid_T, f_set1[1]),
                       0.7*e_gen_hz(grid_T, f_set1[2]),
                       0.5*e_gen_hz(grid_T, f_set1[3]))
  
  ## Group 2
  A_locs[[2]] <- cbind(a_gen(locs_D, 2), a_gen(locs_D, 1), a_gen(locs_D, 0))
  E_locs[[2]] <- cbind(1.0*e_gen_hz(locs_T, f_set2[1]),
                       0.7*e_gen_hz(locs_T, f_set2[2]),
                       0.5*e_gen_hz(locs_T, f_set2[3]))
  A_grid[[2]] <- cbind(a_gen(grid_D, 2), a_gen(grid_D, 1), a_gen(grid_D, 0))
  E_grid[[2]] <- cbind(1.0*e_gen_hz(grid_T, f_set2[1]),
                       0.7*e_gen_hz(grid_T, f_set2[2]),
                       0.5*e_gen_hz(grid_T, f_set2[3]))
  
  ## Group 3
  A_locs[[3]] <- cbind(a_gen(locs_D, 1), a_gen(locs_D, 0), a_gen(locs_D, 0))
  E_locs[[3]] <- cbind(1.0*e_gen_hz(locs_T, f_set3[1]),
                       0.7*e_gen_hz(locs_T, f_set3[2]),
                       0.5*e_gen_hz(locs_T, f_set3[3]))
  A_grid[[3]] <- cbind(a_gen(grid_D, 1), a_gen(grid_D, 0), a_gen(grid_D, 0))
  E_grid[[3]] <- cbind(1.0*e_gen_hz(grid_T, f_set3[1]),
                       0.7*e_gen_hz(grid_T, f_set3[2]),
                       0.5*e_gen_hz(grid_T, f_set3[3]))
  
  ## Group 4
  A_locs[[4]] <- cbind(a_gen(locs_D, 1), a_gen(locs_D, 0), a_gen(locs_D, 2))
  E_locs[[4]] <- cbind(1.0*e_gen_hz(locs_T, f_set4[1]),
                       0.7*e_gen_hz(locs_T, f_set4[2]),
                       0.5*e_gen_hz(locs_T, f_set4[3]))
  A_grid[[4]] <- cbind(a_gen(grid_D, 1), a_gen(grid_D, 0), a_gen(grid_D, 2))
  E_grid[[4]] <- cbind(1.0*e_gen_hz(grid_T, f_set4[1]),
                       0.7*e_gen_hz(grid_T, f_set4[2]),
                       0.5*e_gen_hz(grid_T, f_set4[3]))
  
  ## ----- assemble data and normalize components / balance amplitudes -----
  X_locs <- X_grid <- vector("list", n_groups)
  for (g in 1:n_groups) {
    # print(t(E_grid[[g]]) %*% E_grid[[g]] / n_nodes_HR_grid_T)
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
    f_groups_hz = list(f_set1 = f_set1, f_set2 = f_set2, f_set3 = f_set3, f_set4 = f_set4),
    f_Nyquist_hz = fN
  )
}