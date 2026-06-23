#' Load mesh component CSV files into a named list.
#'
#' @param path File or directory path.
#' @return The value produced by `import_mesh_data`.
import_mesh_data <- function(path) {
  mesh_data <- list(
    nodes    = as.matrix(read.csv(paste(path, "points.csv",   sep = ""))[, -1]),
    edges    = as.matrix(read.csv(paste(path, "edges.csv",    sep = ""))[, -1]),
    elements = as.matrix(read.csv(paste(path, "elements.csv", sep = ""))[, -1]),
    neigh    = as.matrix(read.csv(paste(path, "neigh.csv",    sep = ""))[, -1]),
    boundary = as.matrix(read.csv(paste(path, "boundary.csv", sep = ""))[, -1])
  )
  return(mesh_data)
}
#' Evaluate FEM node values at requested grid locations.
#'
#' @param grid Evaluation grid.
#' @param f_at_nodes Field values at mesh nodes.
#' @param mesh Mesh object or mesh-list structure.
#' @return The value produced by `evaluate_field`.
evaluate_field <- function(grid, f_at_nodes, mesh) {

  FEMbasis    <- create.FEM.basis(mesh)
  FEMfunction <- FEM(f_at_nodes, FEMbasis)
  f_at_grid   <- eval.FEM(FEMfunction, grid)

  return(f_at_grid)
}
#' Build a structured unit-square mesh.
#'
#' @param n_nodes Number of nodes or grid points to create.
#' @return The value produced by `unit_square`.
unit_square <- function(n_nodes) {
  x <- y <- seq(0, 1, length = sqrt(n_nodes))
  grid  <- meshgrid(x, y)
  nodes <- data.frame(as.vector(grid$X), as.vector(grid$Y))
  mesh  <- fdaPDE::create.mesh.2D(nodes)
  mesh_data <- list(
    nodes    = as.matrix(mesh$nodes),
    edges    = as.matrix(mesh$edges),
    elements = as.matrix(mesh$triangles),
    neigh    = as.matrix(mesh$neighbors),
    boundary = as.matrix(as.numeric(mesh$nodesmarkers))
  )
  return(mesh_data)
}
#' Build a uniform grid on the unit interval.
#'
#' @param n_nodes Number of nodes or grid points to create.
#' @return The value produced by `unit_interval`.
unit_interval <- function(n_nodes){
  x <- seq(0, 1, length = n_nodes)
  return(x)
}