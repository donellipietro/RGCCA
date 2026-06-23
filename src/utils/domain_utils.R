#' Create a supported spatial or temporal domain description.
#'
#' @param name_mesh Domain or mesh identifier.
#' @param n_nodes Number of nodes or grid points to create.
#' @param T_sec Optional interval length in seconds.
#' @return The value produced by `generate_domain`.
generate_domain <- function(name_mesh = "unit_square", n_nodes = 1600, T_sec = NULL) {
  switch(name_mesh,
         unit_interval = {

           ## Boundary
           boundary <- extent(0, 1, 0, 0)
           boundary <- as(boundary, "SpatialLines")

           ## Knots
           knots <- unit_interval(n_nodes)

           return(list(
             d = 1,
             boundary = boundary,
             knots = knots
           ))
         },
         interval = {

           if(is.null(T_sec)) T_sec = 1

           ## Boundary
           boundary <- extent(0, T_sec, 0, 0)
           boundary <- as(boundary, "SpatialLines")

           ## Knots
           knots <- unit_interval(n_nodes) * T_sec

           return(list(
             d = 1,
             boundary = boundary,
             knots = knots
           ))
         },
         unit_square = {

           ## Domain
           femr_mesh <- femR::Mesh(unit_square(n_nodes))

           ## Boundary
           boundary <- extent(0, 1, 0, 1)
           boundary <- as(boundary, "SpatialPolygons")

           ## Mesh fdaPDE1
           fdapde_mesh <- fdaPDE::create.mesh.2D(femr_mesh$nodes())

           return(list(
             d = 2,
             femr_mesh = femr_mesh,
             boundary = boundary,
             fdapde_mesh = fdapde_mesh
           ))
         },
         {
           stop("The selected domain is not available.")
         }
  )
}
#' Generate measurement locations for a domain.
#'
#' @param domain Domain object associated with generated data.
#' @param locs_eq_nodes Whether locations should equal mesh nodes.
#' @param n_locs Number of locations to generate.
#' @param type Sampling strategy passed to spatial sampling.
#' @return The value produced by `generate_locations`.
generate_locations <- function(domain, locs_eq_nodes, n_locs, type = "stratified") {
  if (locs_eq_nodes) {
    if(domain$d == 2) {
      locations <- domain$fdapde_mesh$nodes
    } else if(domain$d == 1) {
      locations <- domain$knots
    }
    # cat("\nLocations set to be equal to the nodes of the mesh!\n")
  } else {
    set.seed(-1)
    points <- spsample(domain$boundary, n_locs, type = type)
    if(domain$d == 2) {
      locations <- points@coords
    } else if(domain$d == 1) {
      locations <- points@coords[, 1]
    }
    # cat("\nCustom locations initialized!\n")
  }
  return(locations)
}