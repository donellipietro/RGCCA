# = ========================================================================== =
# - Script: domain_utils.R
# - Desc: Domain and location generation utilities. Provides helpers to
#         construct simple domains/meshes and to sample measurement locations.
# = ========================================================================== =


# - Function: generate_domain
# - Args:
#   * name_mesh: character, domain identifier ("unit_interval" or "unit_square")
#   * n_nodes: integer, number of nodes to use when constructing the domain/mesh
# - Desc:
#   Builds a domain object based on 'name_mesh'. For "unit_interval", returns
#   a line boundary and 1D knots. For "unit_square", returns a femR mesh, a
#   polygon boundary, and an fdaPDE mesh. The returned list fields depend on
#   the selected domain.
generate_domain <- function(name_mesh = "unit_square", n_nodes = 1600) {
  switch(name_mesh,
         unit_interval = {
           
           ## Boundary
           boundary <- extent(0, 1, 0, 0)
           boundary <- as(boundary, "SpatialLines")
           
           ## Knots
           knots <- unit_interval(n_nodes)
           
           return(list(
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


# - Function: generate_locations
# - Args:
#   * domain: list as returned by generate_domain()
#   * locs_eq_nodes: logical, if TRUE use mesh nodes as locations; otherwise
#                    sample locations within the domain boundary
#   * n_locs: integer, number of locations to generate
# - Desc:
#   Generates measurement locations. If 'locs_eq_nodes' is TRUE, returns the
#   mesh nodes from the provided domain. Otherwise, samples 'n_locs'
#   points (stratified) inside the domain boundary and returns their coordinates.
generate_locations <- function(domain, locs_eq_nodes, n_locs) {
  if (locs_eq_nodes) {
    locations <- domain$fdapde_mesh$nodes
    cat("\nLocations set to be equal to the nodes of the mesh!\n")
  } else {
    set.seed(-1)
    points <- spsample(domain$boundary, n_locs, type = "stratified")
    locations <- points@coords
    cat("\nCustom locations initialized!\n")
  }
  return(locations)
}