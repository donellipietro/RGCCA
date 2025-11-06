library(Matrix)
source("src/data-generation/function_generators_2D.R")
source("src/utils/plotting_utils.R")
source("src/utils/mesh_utils.R")
source("src/utils/domain_utils.R")
source("src/utils/svg_utils.R")



####
svg_file <- "data/paths/regions.svg"
res_list <- svg_to_sp_and_nodes_multi(svg_file, step = 0.6, curve_steps = 60)

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
mesh_list <- meshes_from_poly_groups(poly_groups, maximum_area = 0.25) # minimum_angle = 30



plot_multi_mesh(mesh_list)


plot_list <- list()
for (id in 1:4) {
  
  mesh <- mesh_list[[id]]$mesh
  femr_mesh <- femR::Mesh(fdapde2femR_mesh(mesh))
  boundary <- poly_groups[[id]]$poly_sp
  # plot(femr_mesh)
  
  ## Domain
  # domain <- generate_domain("unit_square", 100)
  # femr_mesh <- domain$femr_mesh
  # boundary <- domain$boundary
  
  ## Locs
  n_locs <- 100^2
  points <- spsample(boundary, n_locs, type = "regular")
  locs <- points@coords
  indexes <- c(1, 2, 3, 4, 5, 6)
  # plot(locs, asp = 1)
  
  ## Define operator
  L <- function(u){ -laplace(u) }
  f <- function(points){
    return(matrix(0,nrow=nrow(points), ncol=1))
  }
  
  FF_locs <- eigenfunctions_elliptic_operator(locs, indexes, femr_mesh, L, f)
  
  for(i in indexes){
    plot_list <- c(plot_list, plot.field_tile(locs, FF_locs[, i], boundary) + std_plot_settings_fields() + ggtitle(paste("id", i)))
  }
  
}

plot <- arrangeGrob(grobs = plot_list, nrow = 4)
grid.arrange(plot)

