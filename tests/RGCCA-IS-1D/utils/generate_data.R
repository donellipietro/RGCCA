generate_data <- function(test_options, seed = 0) {
  ## Helpers ----
  a_gen <- function(x, id) {
    if (id == 0) {
      return(0 * x)
    }
    if (id == 1) {
      return(exp(-80 * (x - 1 / 4)^2))
    }
    if (id == 2) {
      return(exp(-80 * (x - 3 / 4)^2))
    }
    if (id == 3) {
      return(exp(-80 * (x - 1 / 2)^2))
    }
    stop("a_gen: unknown id")
  }

  ## Define the design matrix ----
  C <- matrix(c(
    0, 1, 1, 1,
    1, 0, 0, 1,
    1, 0, 0, 1,
    1, 1, 1, 0
  ), ncol = 4, byrow = TRUE)

  ## Get number of samples ----
  n <- test_options$dimensions$n

  ## Get space resolution ----
  delta <- test_options$data$delta
  if (is.null(delta)) {
    n_locs <- test_options$dimensions$n_locs
    if (is.null(n_locs)) {
      error("Both delta and n_locs are NULL")
    } else {
      delta <- 1 / (n_locs - 1)
    }
  }

  ## Get dimensions ----
  n_groups <- test_options$dimensions$n_groups
  n_comp <- test_options$model_options$n_comp
  n_nodes_D <- test_options$dimensions$n_nodes_D
  n_locs_D <- test_options$dimensions$n_locs_D
  n_nodes_HR_grid_D <- test_options$dimensions$n_nodes_HR_grid_D

  ## Domain & locations -----
  domain_D <- generate_domain(test_options$domain_and_locations$name_mesh, n_nodes_D)
  grid_D <- generate_locations(domain_D, FALSE, n_nodes_HR_grid_D)
  locs_D <- seq(0, 1, by = delta)
  n_locs_D <- length(locs_D)

  ## Generate data ----

  ## Room for loadings and components
  A_locs <- A_grid <- H <- vector("list", n_groups)

  ## Canonical components
  rho <- 0.9
  set.seed(seed)
  Sca <- diag(1, 16, 16)
  Cor <- diag(1, 16, 16)
  Sca[1:4, 1:4] <- 1.5 * Sca[1:4, 1:4]
  Sca[5:8, 5:8] <- 1.2 * Sca[5:8, 5:8]
  Sca[9:12, 9:12] <- 1.0 * Sca[9:12, 9:12]
  Sca[13:16, 13:16] <- 0 * Sca[13:16, 13:16]
  Cor[1, 3] <- Cor[3, 1] <- rho
  Cor[2, 4] <- Cor[4, 2] <- -rho
  Cor[4 + 1, 4 + 2] <- Cor[4 + 2, 4 + 1] <- -rho * 0.9
  Cor[4 + 3, 4 + 4] <- Cor[4 + 4, 4 + 3] <- rho * 0.9
  Cor[8 + 1, 8 + 4] <- Cor[8 + 4, 8 + 1] <- rho * 0.8
  HH <- mvrnorm(n = n, mu = rep(0, 16), Sigma = Sca %*% Cor %*% Sca, empirical = TRUE)
  HH[, 8 + 2] <- 0 * HH[, 8 + 2]
  HH[, 8 + 3] <- 0 * HH[, 8 + 3]
  HH[, 13:16] <- 0 * HH[, 13:16]
  colnames(HH) <- c(paste0("H", 1, "g", 1:4), paste0("H", 2, "g", 1:4), paste0("H", 3, "g", 1:4), paste0("H", 4, "g", 1:4))
  # pairs(HH, xlim = c(-4, 4), ylim = c(-4, 4))
  H1 <- HH[, 1:4]
  H2 <- HH[, 5:8]
  H3 <- HH[, 9:12]
  H4 <- HH[, 13:16]

  ## Group 1
  A_locs[[1]] <- cbind(cbind(a_gen(locs_D, 1), a_gen(locs_D, 2), a_gen(locs_D, 3), a_gen(locs_D, 0))[, 1:n_comp])
  A_grid[[1]] <- cbind(cbind(a_gen(grid_D, 1), a_gen(grid_D, 2), a_gen(grid_D, 3), a_gen(grid_D, 0))[, 1:n_comp])
  H[[1]] <- cbind(cbind(H1[, 1], H2[, 1], H3[, 1], H4[, 1])[, 1:n_comp])

  ## Group 2
  A_locs[[2]] <- cbind(cbind(a_gen(locs_D, 2), a_gen(locs_D, 1), a_gen(locs_D, 0), a_gen(locs_D, 0))[, 1:n_comp])
  A_grid[[2]] <- cbind(cbind(a_gen(grid_D, 2), a_gen(grid_D, 1), a_gen(grid_D, 0), a_gen(grid_D, 0))[, 1:n_comp])
  H[[2]] <- cbind(cbind(H1[, 2], H2[, 2], H3[, 2], H4[, 2])[, 1:n_comp])

  ## Group 3
  A_locs[[3]] <- cbind(cbind(a_gen(locs_D, 1), a_gen(locs_D, 3), a_gen(locs_D, 0), a_gen(locs_D, 0))[, 1:n_comp])
  A_grid[[3]] <- cbind(cbind(a_gen(grid_D, 1), a_gen(grid_D, 3), a_gen(grid_D, 0), a_gen(grid_D, 0))[, 1:n_comp])
  H[[3]] <- cbind(cbind(H1[, 3], H2[, 3], H3[, 3], H4[, 3])[, 1:n_comp])

  ## Group 4
  A_locs[[4]] <- cbind(cbind(a_gen(locs_D, 1), a_gen(locs_D, 3), a_gen(locs_D, 2), a_gen(locs_D, 0))[, 1:n_comp])
  A_grid[[4]] <- cbind(cbind(a_gen(grid_D, 1), a_gen(grid_D, 3), a_gen(grid_D, 2), a_gen(grid_D, 0))[, 1:n_comp])
  H[[4]] <- cbind(cbind(H1[, 4], H2[, 4], H3[, 4], H4[, 4])[, 1:n_comp])


  ## Assemble data and normalize loadings and canonical components
  X_locs <- X_grid <- vector("list", n_groups)
  for (g in 1:n_groups) {
    X_locs[[g]] <- H[[g]] %*% t(A_locs[[g]])
    X_grid[[g]] <- H[[g]] %*% t(A_grid[[g]])
    for (h in 1:n_comp) {
      norm_H <- var(H[[g]][, h])
      norm_H <- if (is.na(norm_H) || norm_H == 0) 1 else sqrt(norm_H)
      norm_A <- norm_l2(A_locs[[g]][, h])
      norm_A <- if (is.na(norm_A) || norm_A == 0) 1 else norm_A
      H[[g]][, h] <- H[[g]][, h] / norm_H
      A_locs[[g]][, h] <- A_locs[[g]][, h] / norm_A
      A_grid[[g]][, h] <- A_grid[[g]][, h] / norm_A
    }
  }

  ## Add zero-mean noise
  sigma_noise <- test_options$noise$sigma_noise

  noise <- vector("list", n_groups)
  set.seed(seed + 1000)
  for (g in 1:n_groups) {
    EE <- rnorm(n_locs_D * n, sd = sigma_noise)
    EE <- matrix(EE, nrow = n)
    noise[[g]] <- scale(EE, scale = FALSE) # enforce zero-mean
  }

  X <- vector("list", n_groups)
  for (g in 1:n_groups) {
    X[[g]] <- X_locs[[g]] + noise[[g]]
    rownames(X[[g]]) <- paste0("n", 1:n)
    colnames(X[[g]]) <- paste0("g", g, "p", seq_len(n_locs_D))
  }
  names(X) <- paste0("X", seq_len(n_groups))

  ## ----- return -----
  list(
    dimensions = list(
      n = n,
      n_comp = n_comp,
      n_groups = n_groups,
      n_nodes_D = n_nodes_D,
      n_locs_D = n_locs_D,
      n_nodes_HR_grid_D = n_nodes_HR_grid_D
    ),
    domain_D = domain_D,
    locations_D = locs_D,
    grid_D = grid_D,
    X = X,
    C = C,
    X_locs = X_locs,
    X_grid = X_grid,
    A_locs = A_locs,
    A_grid = A_grid,
    H = H,
    sigma_noise = sigma_noise
  )
}
