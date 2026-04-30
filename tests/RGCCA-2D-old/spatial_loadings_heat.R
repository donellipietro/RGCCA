library(Matrix)
library(ggplot2)
library(gridExtra)
library(sp)

# source("src/data-generation/function_generators_2D.R")
source("src/utils/plotting_utils.R")
source("src/utils/mesh_utils.R")
source("src/utils/domain_utils.R")
source("src/utils/svg_utils.R")

## ---------------------------------------------------------------------
## 1. Read regions, build polygons + meshes
## ---------------------------------------------------------------------

svg_file <- "data/paths/regions.svg"
res_list <- svg_to_sp_and_nodes_multi(svg_file, step = 0.45, curve_steps = 60)

# Gather rings and group into polygons (islands)
all_rings <- unlist(lapply(res_list, `[[`, "rings"), recursive = FALSE)
poly_groups <- rings_to_polygons_by_containment(all_rings)

# Global bbox (all regions together)
all_nodes <- do.call(
  rbind,
  lapply(poly_groups, function(g) g$outer[-nrow(g$outer), ])
)
xlim_all <- c(0, 55) # range(all_nodes[, 1])
ylim_all <- c(-30, 0) # range(all_nodes[, 2])

# One mesh per island
mesh_list <- meshes_from_poly_groups(poly_groups, maximum_area = 0.25) # 0.5
# plot_multi_mesh(mesh_list)  # optional check

plot(mesh_list[[1]]$mesh)
plot(mesh_list[[2]]$mesh)
plot(mesh_list[[3]]$mesh)
plot(mesh_list[[4]]$mesh)
dim(mesh_list[[1]]$mesh$nodes)
dim(mesh_list[[2]]$mesh$nodes)
dim(mesh_list[[3]]$mesh$nodes)
dim(mesh_list[[4]]$mesh$nodes)

## ---------------------------------------------------------------------
## 2. Helper: define RHS f(x,y) for each region and component id
## ---------------------------------------------------------------------

rhs_fun <- function(points, region, id) {
  x <- points[, 1]
  y <- points[, 2]
  
  if (region == 1) { # actual region: 1 - TL
    # if (id == 1) return(exp(-0.1 * ((x - 20)^2 + (y + 17)^2)))
    # if (id == 2) return(exp(-0.08 * ((x - 8)^2 + (y + 16)^2)))
    # if (id == 3) return(exp(-0.1 * ((x - 25)^2 + (y + 5)^2)))
    
    if (id == 1) return(exp(-0.05 * ((x - 25)^2 + (y + 5)^2))) # 0.1
    if (id == 2) return(exp(-0.05 * ((x - 8)^2 + (y + 16)^2)))   # 0.2
    if (id == 3) return(exp(-0.05 * ((x - 11)^2 + (y + 3)^2))) # 0.2
  }
  if (region == 2) { # actual region: 3 - TR
    if (id == 1) return(exp(-0.05 * ((x - 30)^2 + (y + 15)^2))) # 0.1
    if (id == 2) return(exp(-0.05 * ((x - 50)^2 + (y + 7)^2)) + exp(-0.05 * ((x - 50)^2 + (y + 12)^2)))
    if (id == 3) return(exp(-0.05 * ((x - 37)^2 + (y + 4)^2))) # 0.1
    
  }
  if (region == 3) { # actual region: 2 - BL
    if (id == 1) return(exp(-0.05 * ((x - 24)^2 + (y + 22)^2)))
    if (id == 2) return(exp(-0.05 * ((x - 10)^2 + (y + 24)^2)))
    if (id == 3) return(0 * x)
  }
  if (region == 4) { # actual region: 3 - BR
    if (id == 1) return(exp(-0.05 * ((x - 32)^2 + (y + 23)^2))) # 0.1
    if (id == 2) return(exp(-0.05 * ((x - 45)^2 + (y + 24)^2))) # 0.1
    if (id == 3) return(0 * x)
  }
  
  stop("Unknown combination of region / id")
}

## ---------------------------------------------------------------------
## 3. Generate fields: loop over regions and components
##    -> store all in a list
## ---------------------------------------------------------------------

generate_fields <- function(mesh_list, poly_groups,
                            region_ids = 1:4,
                            component_ids = 1:3,
                            n_locs = 1e4) {
  # Result structure:
  # fields[[region]][[id]] = list(
  #   region = region, id = id,
  #   locs = matrix(n_locs x 2),
  #   values = numeric(n_locs),
  #   boundary = SpatialPolygons
  # )
  fields <- vector("list", length(region_ids))
  names(fields) <- paste0("region_", region_ids)
  
  # PDE operator
  L <- function(u) { -laplace(u) }
  
  for (r in region_ids) {
    r_shift <- c(1, 3, 4, 2)[r]
    cat("Computing region", r, "...\n")
    
    mesh <- mesh_list[[r_shift]]$mesh
    femr_mesh <- femR::Mesh(fdapde2femR_mesh(mesh))
    boundary <- poly_groups[[r_shift]]$poly_sp
    
    # Points inside region (same for all components of same region)
    points <- spsample(boundary, n = n_locs, type = "regular")
    locs <- points@coords
    
    # Function space and unknown
    Vh <- FunctionSpace(femr_mesh, fe_order = 1)
    u <- Function(Vh)
    
    region_fields <- vector("list", length(component_ids))
    names(region_fields) <- paste0("comp_", component_ids)
    
    for (id in component_ids) {
      cat("  Component", id, "\n")
      
      f <- function(pts) rhs_fun(pts, region = r_shift, id = id)
      pde <- femR::Pde(L(u), f)
      pde$solve()
      
      # Evaluate solution at locations
      u_locs <- u$eval(locs)
      
      # Normalize (avoid division by 0)
      norm <- max(u_locs)
      if (norm == 0) norm <- 1
      u_locs <- u_locs / norm
      
      region_fields[[paste0("comp_", id)]] <- list(
        region   = r,
        id       = id,
        locs     = locs,
        values   = u_locs,
        boundary = boundary
      )
    }
    
    fields[[paste0("region_", r)]] <- region_fields
  }
  
  fields
}

fields <- generate_fields(mesh_list, poly_groups,
                          region_ids = 1:4,
                          component_ids = 1:3)

## ---------------------------------------------------------------------
## 4. Plot function: one component, all regions in ONE ggplot
## ---------------------------------------------------------------------

library(ggplot2)

plot_component_all_regions <- function(fields,
                                       component_id,
                                       poly_groups,
                                       xlim = NULL,
                                       ylim = NULL) {
  ## ---- 1. Data frame with all locations/values for this component ----
  df_list <- list()
  idx <- 1
  
  for (r_name in names(fields)) {
    region_fields <- fields[[r_name]]
    comp_name <- paste0("comp_", component_id)
    entry <- region_fields[[comp_name]]
    
    df_list[[idx]] <- data.frame(
      x      = entry$locs[, 1],
      y      = entry$locs[, 2],
      value  = entry$values,
      region = factor(entry$region)
    )
    idx <- idx + 1
  }
  
  df <- do.call(rbind, df_list)
  
  # Global color limits for this component
  val_limits <- range(df$value, finite = TRUE)
  
  ## ---- 2. Polygon boundaries with UNIQUE groups per region ----
  poly_df_list <- lapply(seq_along(poly_groups), function(r) {
    pg  <- poly_groups[[r]]$poly_sp
    tmp <- fortify(pg)
    tmp$region_id <- factor(r)
    # make group unique per region to avoid connecting polygons
    tmp$group_id  <- interaction(tmp$region_id, tmp$group, drop = TRUE)
    tmp
  })
  poly_df <- do.call(rbind, poly_df_list)
  
  ## ---- 3. Plot: loadings as points + region outlines ----
  ggplot() +
    # loadings field
    geom_point(
      data = df,
      aes(x = x, y = y, colour = value),
      size = 0.4
    ) +
    # region boundaries
    geom_path(
      data = poly_df,
      aes(x = long, y = lat, group = group_id),
      colour = "black",
      linewidth = 1
    ) +
    coord_equal(xlim = xlim, ylim = ylim, expand = FALSE) +
    scale_colour_viridis_c(limits = val_limits) +
    ggtitle(paste("Component", component_id)) +
    std_plot_settings_fields() +
    theme(
      legend.position      = "right",
      legend.text.position = "right"
    ) + guides(color = "none")
}

## ---------------------------------------------------------------------
## 5. Produce the three figures (one per component)
## ---------------------------------------------------------------------

p1 <- plot_component_all_regions(fields, component_id = 1,
                                 poly_groups = poly_groups,
                                 xlim = xlim_all, ylim = ylim_all)
p1

p2 <- plot_component_all_regions(fields, component_id = 2,
                                 poly_groups = poly_groups,
                                 xlim = xlim_all, ylim = ylim_all)
p2

p3 <- plot_component_all_regions(fields, component_id = 3,
                                 poly_groups = poly_groups,
                                 xlim = xlim_all, ylim = ylim_all)
p3


# --- RUN THIS ONCE TO CREATE data/mesh/regions/region_k.rds ---

dir.create("data/mesh/regions", recursive = TRUE, showWarnings = FALSE)

precompute_region_objects <- function(mesh_list, poly_groups,
                                      region_ids = 1:4,
                                      component_ids = 1:3) {
  
  for (r in region_ids) { # r = 1
    r_shift <- c(1, 3, 4, 2)[r]
    cat("Precomputing FEM coefficients for region", r, "...\n")
    
    mesh <- mesh_list[[r_shift]]$mesh
    
    # Extract pure mesh geometry
    nodes <- mesh$nodes             # N × 2
    triangles <- mesh$triangles     # T × 3
    nodesmarkers <- mesh$nodesmarkers
    
    cat(paste("- n nodes:", nrow(nodes), "\n"))
    
    # boundary polygon for sampling
    boundary <- poly_groups[[r_shift]]$poly_sp
    
    # Reconstruct femR mesh for solving
    femr_mesh <- femR::Mesh(fdapde2femR_mesh(mesh))
    Vh <- FunctionSpace(femr_mesh, fe_order = 1)
    u  <- Function(Vh)
    
    coef_list <- vector("list", length(component_ids))
    
    for (j in seq_along(component_ids)) {
      id <- component_ids[j]
      f  <- function(pts) rhs_fun(pts, region = r_shift, id = id)
      pde <- femR::Pde(-laplace(u), f)
      pde$solve()
      coef_list[[j]] <- u$coefficients()
    }
    
    coef_mat <- do.call(cbind, coef_list)
    
    # ----------- SAVE ONLY GEOMETRY + COEFS --------------
    region_obj <- list(
      nodes         = nodes,
      triangles     = triangles,
      nodesmarkers  = nodesmarkers,
      boundary      = boundary,
      coef_mat      = coef_mat,
      component_ids = component_ids
    )
    
    saveRDS(
      region_obj,
      file = file.path("data/mesh/regions", paste0("region_", r, ".rds"))
    )
  }
}

# call once, using your already built mesh_list / poly_groups:
precompute_region_objects(mesh_list, poly_groups)
