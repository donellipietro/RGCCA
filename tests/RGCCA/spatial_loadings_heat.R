library(Matrix)
source("src/data-generation/function_generators_2D.R")
source("src/utils/plotting_utils.R")
source("src/utils/mesh_utils.R")
source("src/utils/domain_utils.R")
source("src/utils/svg_utils.R")



####
svg_file <- "data/paths/regions.svg"
res_list <- svg_to_sp_and_nodes_multi(svg_file, step = 0.5, curve_steps = 60)

## Example: gather all rings from all paths (outer + interior shapes)
all_rings <- unlist(lapply(res_list, `[[`, "rings"), recursive = FALSE)
poly_groups <- rings_to_polygons_by_containment(all_rings)

## Plot them all with common limits (no flipping issues)
all_nodes <- do.call(rbind, lapply(poly_groups, function(g) g$outer[-nrow(g$outer), ]))
xlim <- range(all_nodes[,1]); ylim <- range(all_nodes[,2])
plot(poly_groups[[1]]$poly_sp, asp = 1, xlim = xlim, ylim = ylim, col = "grey85", border = "black")
if (length(poly_groups) > 1) {
  for (i in 2:length(poly_groups)) {
    plot(poly_groups[[i]]$poly_sp, asp = 1, xlim = xlim, ylim = ylim, add = TRUE, col = "grey85")
  }
}


## Build one mesh per island, refining by target area
mesh_list <- meshes_from_poly_groups(poly_groups, maximum_area = 0.05) # minimum_angle = 30

region <- 4
mesh <- mesh_list[[region]]$mesh
femr_mesh <- femR::Mesh(fdapde2femR_mesh(mesh))
boundary <- poly_groups[[region]]$poly_sp
# plot(femr_mesh)




# plot_multi_mesh(mesh_list)

region <- 1
plot_list <- list()


for (region in c(1, 2, 3, 4)) {
  
  mesh <- mesh_list[[region]]$mesh
  femr_mesh <- femR::Mesh(fdapde2femR_mesh(mesh))
  boundary <- poly_groups[[region]]$poly_sp
  plot(femr_mesh)
  
  ## Domain
  # domain <- generate_domain("unit_square", 100)
  # femr_mesh <- domain$femr_mesh
  # boundary <- domain$boundary
  
  ## Locs
  n_locs <- 200^2
  points <- spsample(boundary, n_locs, type = "regular")
  locs <- points@coords
  indexes <- c(1, 2, 3, 4, 5, 6)
  # plot(locs, asp = 1)
  
  
  id <- 1
  
  ## Define operator
  L <- function(u){ -laplace(u) }
  
  ## Define the PDE
  Vh <- FunctionSpace(femr_mesh, fe_order = 1)
  u <- Function(Vh)
  
  for(id in 1:3) {
    f <- function(points){
      if(region == 1) {
        if(id == 1) {
          return(exp(-0.05*((points[, 1] - 17)^2 + (points[, 2] + 15)^2)))
        }
        if(id == 2) {
          return(exp(-0.05*((points[, 1] - 32)^2 + (points[, 2] + 12)^2)))
        }
        if(id == 3) {
          return(0*points[, 1])
        }
      }
      if(region == 2) {
        if(id == 1) {
          return(
            0.75 * exp(-0.05*((points[, 1] - 43)^2 + (points[, 2] + 19)^2)) +
            1.00 * exp(-0.05*((points[, 1] - 47)^2 + (points[, 2] + 10)^2))
          )
        }
        if(id == 2) {
          return(exp(-0.05*((points[, 1] - 60)^2 + (points[, 2] + 12)^2)))
        }
        if(id == 3) {
          return(exp(-0.05*((points[, 1] - 57)^2 + (points[, 2] + 22)^2)))
        }
      }
      if(region == 3) {
        if(id == 1) {
          return(exp(-0.05*((points[, 1] - 18)^2 + (points[, 2] + 31)^2)))
        }
        if(id == 2) {
          return(exp(-0.05*((points[, 1] - 32)^2 + (points[, 2] + 26)^2)))
        }
        if(id == 3) {
          return(0*points[, 1])
        }
      }
      if(region == 4) {
        if(id == 1) {
          return(exp(-0.05*((points[, 1] - 40)^2 + (points[, 2] + 27)^2)))
        }
        if(id == 2) {
          return(exp(-0.05*((points[, 1] - 56)^2 + (points[, 2] + 29)^2)))
        }
        if(id == 3) {
          return(0*points[, 1])
        }
      }
    }
    
    
    pde <- femR::Pde(L(u), f)
    pde$solve()
    
    Psi <- Vh$basis()$eval(as.matrix(locs))
    
    u_locs <- u$eval(locs)
    norm <- max(u_locs)
    norm <- ifelse(norm == 0, 1, norm)
    u_locs <- (u_locs/norm)
    
    limits <- range(u_locs)
    print(limits)
    
    plot_list <- c(
      plot_list, 
      plot.field_tile(locs, u_locs, boundary,  limits = limits) + # ISOLINES = TRUE,
        std_plot_settings_fields() + 
        ggtitle(paste("id", id)))
  }
  
}

plot <- arrangeGrob(grobs = plot_list, ncol = 3)
grid.arrange(plot)

