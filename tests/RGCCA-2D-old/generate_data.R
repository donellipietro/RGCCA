load_region_object <- function(region_name) {
  # region_name like "region_1"
  readRDS(file.path("data/mesh/regions", paste0(region_name, ".rds")))
}

eval_region_loadings <- function(region_obj, locs_mat) {
  
  # rebuild femR mesh from nodes + triangles
  mesh_list <- list(
    nodes     = region_obj$nodes,
    triangles = region_obj$triangles,
    nodesmarkers = region_obj$nodesmarkers
  )
  
  femr_mesh <- femR::Mesh(fdapde2femR_mesh(mesh_list))
  
  # basis
  Vh <- FunctionSpace(femr_mesh, fe_order = 1)
  basis <- Vh$basis()
  
  # interpolation matrix
  Phi <- basis$eval(locs_mat)
  
  # combine with coefficients
  Phi %*% region_obj$coef_mat
}


generate_data <- function(test_options, seed = 0) {
  
  ## Helpers ----
  e_gen_hz <- function(t, f_hz) {
    if (f_hz == 0) return(0 * t)
    sin(2 * pi * f_hz * t)
  }
  
  ## ----- design / connectivity (unchanged) -----
  C <- matrix(c(
    0,1,1,1,
    1,0,0,1,
    1,0,0,1,
    1,1,1,0
  ), ncol = 4, byrow = TRUE)
  
  ## ----- locs resolution / global dimensions ----- 
  T_sec <- test_options$domain_and_locations$T_sec
  delta <- test_options$data$delta
  TR    <- test_options$data$TR
  
  n_groups           <- test_options$dimensions$n_groups
  n_comp             <- test_options$model_options$n_comp
  n_nodes_T          <- test_options$dimensions$n_nodes_T
  n_nodes_HR_grid_T  <- test_options$dimensions$n_nodes_HR_grid_T
  
  # which spatial region each group uses
  region_names <- test_options$domain_and_locations$name_mesh
  stopifnot(length(region_names) == n_groups)
  
  ## ----- time over [0, T_sec], sampled every TR seconds ----- 
  domain_T <- generate_domain("interval", n_nodes_T, T_sec = T_sec)
  locs_T   <- seq(0, T_sec, by = TR)
  grid_T   <- generate_locations(domain_T, FALSE, n_nodes_HR_grid_T)
  
  n_locs_T <- length(locs_T)
  
  ## Choose sine frequencies (Hz) -----
  f_set1 <- c(2.1, 2.9, 6.5) / T_sec
  f_set2 <- c(-4.3, -2.9, 0) / T_sec 
  f_set3 <- c(2.1, 5.2, 0) / T_sec
  f_set4 <- c(4.3, 5.2, 6.5) / T_sec
  
  ## ----- allocate containers (now lists per group) ----- 
  A_locs <- A_grid <- E_locs <- E_grid <- vector("list", n_groups)
  X_locs <- X_grid <- vector("list", n_groups)
  
  # spatial objects per group
  domain_D   <- vector("list", n_groups)  # here: region objects
  locations_D <- grid_D <- vector("list", n_groups)
  n_locs_D_vec <- integer(n_groups)
  
  ## ----- build space + time loadings per group ----- 
  for (g in 1:n_groups) { # g = 1
    region_name <- region_names[g]       # "region_1", ...
    region_obj  <- load_region_object(region_name)
    
    # treat region_obj as "domain" for this group
    domain_D[[g]] <- region_obj
    
    # sample spatial locations inside this region
    n_locs_D_target <- test_options$dimensions$n_locs_D[g]  # scalar target
    set.seed(0)
    points <- spsample(region_obj$boundary,
                       n = n_locs_D_target,
                       type = "stratified")
    locs_Dg <- points@coords
    locations_D[[g]] <- locs_Dg
    n_locs_D_vec[g]  <- nrow(locs_Dg)
    
    # for a spatial high–res grid we can re-use these, or make denser
    n_grid_D_target <- test_options$dimensions$n_nodes_HR_grid_D[g]  # scalar target
    points <- spsample(region_obj$boundary,
                       n = n_grid_D_target,
                       type = "regular")
    grid_Dg <- points@coords
    grid_D[[g]] <- grid_Dg
    
    # evaluate spatial loadings from FEM
    A_locs[[g]] <- eval_region_loadings(region_obj, locations_D[[g]])
    A_grid[[g]] <- eval_region_loadings(region_obj, grid_D[[g]])
    
    # time profiles: SAME as before, just indexed by group
    if (g == 1) {
      freqs <- f_set1
    } else if (g == 2) {
      freqs <- f_set2
    } else if (g == 3) {
      freqs <- f_set3
    } else if (g == 4) {
      freqs <- f_set4
    } else {
      stop("Need to define f_set for group > 4")
    }
    
    E_locs[[g]] <- cbind(
      4.0 * e_gen_hz(locs_T, freqs[1]),
      3.0 * e_gen_hz(locs_T, freqs[2]),
      2.5 * e_gen_hz(locs_T, freqs[3])
    )
    E_grid[[g]] <- cbind(
      4.0 * e_gen_hz(grid_T, freqs[1]),
      3.0 * e_gen_hz(grid_T, freqs[2]),
      2.5 * e_gen_hz(grid_T, freqs[3])
    )
    
    ## ----- normalize A and E component–wise as before ----- 
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
    
    ## ----- assemble noiseless data for group g ----- 
    X_locs[[g]] <- E_locs[[g]] %*% t(A_locs[[g]])   # n_T × n_locs_Dg
    X_grid[[g]] <- E_grid[[g]] %*% t(A_grid[[g]])
  }
  
  ## ----- add zero-mean noise (per group, per spatial size) ----- 
  sigma_noise <- test_options$noise$sigma_noise
  set.seed(seed)
  
  noise <- vector("list", n_groups)
  X     <- vector("list", n_groups)
  
  for (g in 1:n_groups) {
    n_locs_Dg <- n_locs_D_vec[g]
    EE <- rnorm(n_locs_Dg * n_locs_T, sd = sigma_noise)
    EE <- matrix(EE, nrow = n_locs_T)
    noise[[g]] <- scale(EE, scale = FALSE)
    
    X[[g]] <- X_locs[[g]] + noise[[g]]
    rownames(X[[g]]) <- paste0("t", locs_T, "s")
    colnames(X[[g]]) <- paste0("g", g, "p", seq_len(n_locs_Dg))
  }
  names(X) <- paste0("X", seq_len(n_groups))
  
  ## ----- return ----- 
  list(
    dimensions = list(
      n_comp   = n_comp,
      n_groups = n_groups,
      n_nodes_T = n_nodes_T,
      n_locs_T  = n_locs_T,
      n_nodes_HR_grid_T = n_nodes_HR_grid_T,
      n_locs_D_vec = n_locs_D_vec    # per–group spatial sizes
    ),
    domain_D   = domain_D,          # list of region objects
    locations_D = locations_D,      # list of locs per group
    grid_D      = grid_D,           # list of grids per group
    domain_T    = domain_T,
    locations_T = locs_T,
    grid_T      = grid_T,
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
    f_groups_hz = list(
      f_set1 = f_set1, f_set2 = f_set2, f_set3 = f_set3, f_set4 = f_set4
    )
  )
}