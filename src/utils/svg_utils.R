## Required packages
library(xml2)
library(sf)
library(sp)

invisible(suppressMessages(sapply(c(
  # discretization
  "fdaPDE", "femR",
  # algebraic utils
  "pracma",
  # data manipulation
  "MASS", "tidyr", "dplyr",
  # visualization
  "ggplot2", "viridis", "stringr", "RColorBrewer", "grid", "gridExtra",
  # json
  "jsonlite",
  # sampling
  "sf", "sp", "raster"
), require, character.only = TRUE)))


## -------------------- tokenizing / math helpers ---------------------------

#' Tokenize SVG path data into drawing commands and numeric values.
#'
#' @param d SVG path `d` attribute.
#' @return The value produced by `tokenize_svg_path`.
tokenize_svg_path <- function(d) {
  ## Tokenize an SVG path string into commands and numbers.
  ## Handles compact forms like "c-.68.27-1.52" and scientific notation.
  pattern <- "([MmLlCcZzHhVvQqTtSsAa])|([+-]?(?:\\d*\\.\\d+|\\d+\\.?)(?:[eE][+-]?\\d+)?)"
  tokens  <- regmatches(d, gregexpr(pattern, d, perl = TRUE))[[1]]
  tokens[nzchar(tokens)]
}

#' Evaluate a cubic Bezier curve at one parameter value.
#'
#' @param p0 Bezier start point.
#' @param p1 First Bezier control point.
#' @param p2 Second Bezier control point.
#' @param p3 Bezier end point.
#' @param t Bezier parameter in `[0, 1]`.
#' @return The value produced by `bez3`.
bez3 <- function(p0, p1, p2, p3, t) {
  ## Evaluate a cubic Bezier at parameter t in [0,1].
  u <- 1 - t
  (u^3)*p0 + 3*u^2*t*p1 + 3*u*t^2*p2 + (t^3)*p3
}

#' Flip SVG coordinates into the Cartesian orientation used by mesh tools.
#'
#' @param coords Coordinate matrix.
#' @param viewBox Optional SVG viewBox.
#' @return The value produced by `flip_y_svg_to_cart`.
flip_y_svg_to_cart <- function(coords, viewBox = NULL) {
  coords[, 2] <- -coords[, 2]
  coords
}

#' Resample a closed polygon ring to a fixed vertex count.
#'
#' @param coords Coordinate matrix.
#' @param n_vertices Number of vertices to sample.
#' @return The value produced by `resample_ring`.
resample_ring <- function(coords, n_vertices) {
  ## Resample a closed polyline to exactly 'n_vertices' vertices, uniform by arclength.
  ## Returns a (n_vertices + 1) x 2 matrix with last row = first row (closed ring).
  if (!all(coords[1, ] == coords[nrow(coords), ])) coords <- rbind(coords, coords[1, ])
  ls  <- st_sfc(st_linestring(coords))
  pts <- st_line_sample(ls, sample = seq(0, 1, length.out = n_vertices + 1), type = "regular")[[1]]
  ring <- st_coordinates(pts)[, 1:2, drop = FALSE]
  if (all(ring[1, ] == ring[nrow(ring), ])) ring <- ring[-nrow(ring), , drop = FALSE]
  rbind(ring, ring[1, , drop = FALSE])
}

## --- Perimeter and step-based resampling ---------------------------------

#' Compute the perimeter of a closed or open polygon ring.
#'
#' @param coords Coordinate matrix.
#' @return The value produced by `ring_perimeter`.
ring_perimeter <- function(coords) {
  ## Return total length of a (possibly open) polyline, closing if needed.
  if (!all(coords[1, ] == coords[nrow(coords), ])) {
    coords <- rbind(coords, coords[1, ])
  }
  dx <- diff(coords[, 1]); dy <- diff(coords[, 2])
  sum(sqrt(dx*dx + dy*dy))
}

#' Resample a closed polygon ring to an approximate arc-length step.
#'
#' @param coords Coordinate matrix.
#' @param step Approximate arc-length step.
#' @param min_vertices Minimum number of sampled vertices.
#' @return The value produced by `resample_ring_by_step`.
resample_ring_by_step <- function(coords, step, min_vertices = 8L) {
  ## Resample a closed ring with (approximately) constant arc-length spacing = step.
  ## Returns a (K+1) x 2 matrix (closed; last row repeats first).
  stopifnot(step > 0)
  if (!all(coords[1, ] == coords[nrow(coords), ])) coords <- rbind(coords, coords[1, ])

  ## choose K from perimeter/step (at least min_vertices)
  perim <- ring_perimeter(coords)
  K <- max(min_vertices, ceiling(perim / step))

  ## sample K points uniformly along arclength
  ls  <- sf::st_sfc(sf::st_linestring(coords))
  pts <- sf::st_line_sample(ls, sample = seq(0, 1, length.out = K + 1), type = "regular")[[1]]
  ring <- sf::st_coordinates(pts)[, 1:2, drop = FALSE]

  ## drop duplicated last row (if present), then close explicitly
  if (all(ring[1, ] == ring[nrow(ring), ])) ring <- ring[-nrow(ring), , drop = FALSE]
  rbind(ring, ring[1, , drop = FALSE])
}

## -------------------- parsing a single 'd' into subpaths ------------------

#' Flatten supported SVG path commands into closed polygon subpaths.
#'
#' @param d SVG path `d` attribute.
#' @param curve_steps Number of subdivisions per curve command.
#' @return The value produced by `flatten_svg_path_into_subpaths`.
flatten_svg_path_into_subpaths <- function(d, curve_steps = 60L) {
  ## Parse one 'd' attribute, supporting M/m, L/l, C/c, Z/z.
  ## Returns a list of subpaths; each subpath is an (x,y) matrix (closed).
  toks <- tokenize_svg_path(d)
  i <- 1L
  subpaths <- list()

  #' Describe the `is_cmd` helper used by this repository.
  #'
  #' @return The value produced by `is_cmd`.
  is_cmd   <- function(tk) grepl("^[A-Za-z]$", tk)
  #' Describe the `rn` helper used by this repository.
  #'
  #' @return The value produced by `rn`.
  rn <- function() {
    if (i > length(toks)) stop("Unexpected end of path while reading number.")
    val <- as.numeric(toks[[i]]); i <<- i + 1L; val
  }

  current <- NULL
  cur <- c(0, 0)
  start_pt <- c(NA_real_, NA_real_)

  while (i <= length(toks)) {
    if (!is_cmd(toks[[i]])) stop(sprintf("Expected command at token %d ('%s')", i, toks[[i]]))
    cmd <- toks[[i]]; i <- i + 1L

    if (cmd %in% c("M","m")) {
      ## Start a new subpath
      if (!is.null(current) && nrow(current) > 0) {
        ## Close previous if not closed
        if (!all(current[1, ] == current[nrow(current), ]))
          current <- rbind(current, current[1, ])
        subpaths[[length(subpaths) + 1L]] <- current
      }
      rel <- cmd == "m"
      x <- rn(); y <- rn()
      pt <- if (rel) cur + c(x, y) else c(x, y)
      cur <- pt; start_pt <- pt
      current <- matrix(pt, ncol = 2)
      ## Subsequent pairs are implicit L
      while (i <= length(toks) && !is_cmd(toks[[i]])) {
        x <- rn(); y <- rn()
        pt <- if (rel) cur + c(x, y) else c(x, y)
        current <- rbind(current, pt)
        cur <- pt
      }

    } else if (cmd %in% c("L","l")) {
      rel <- cmd == "l"
      while (i <= length(toks) && !is_cmd(toks[[i]])) {
        x <- rn(); y <- rn()
        pt <- if (rel) cur + c(x, y) else c(x, y)
        current <- rbind(current, pt)
        cur <- pt
      }

    } else if (cmd %in% c("C","c")) {
      rel <- cmd == "c"
      while (i <= length(toks) && !is_cmd(toks[[i]])) {
        if (i + 5L > length(toks)) stop("Cubic Bezier needs 6 numbers.")
        x1 <- rn(); y1 <- rn()
        x2 <- rn(); y2 <- rn()
        x3 <- rn(); y3 <- rn()
        p0 <- cur
        p1 <- if (rel) cur + c(x1, y1) else c(x1, y1)
        p2 <- if (rel) cur + c(x2, y2) else c(x2, y2)
        p3 <- if (rel) cur + c(x3, y3) else c(x3, y3)
        ts <- seq(0, 1, length.out = curve_steps + 1L)[-1]
        seg <- rbind(p0, t(sapply(ts, function(t) bez3(p0, p1, p2, p3, t))))
        current <- rbind(current, seg[-1, , drop = FALSE])
        cur <- p3
      }

    } else if (cmd %in% c("Z","z")) {
      ## Close subpath
      if (!is.null(current) && nrow(current) > 0) {
        if (!all(current[1, ] == current[nrow(current), ]))
          current <- rbind(current, current[1, ])
        subpaths[[length(subpaths) + 1L]] <- current
        current <- NULL
        cur <- start_pt
      }

    } else {
      stop(sprintf("Command '%s' not implemented (add if needed).", cmd))
    }
  }

  ## Flush last open subpath
  if (!is.null(current) && nrow(current) > 0) {
    if (!all(current[1, ] == current[nrow(current), ]))
      current <- rbind(current, current[1, ])
    subpaths[[length(subpaths) + 1L]] <- current
  }

  ## Name and return
  lapply(subpaths, function(m) { colnames(m) <- c("x","y"); m })
}

## -------------------- building sp objects per path ------------------------

#' Convert closed rings into an `sp` polygon object.
#'
#' @param rings_list List of closed polygon rings.
#' @param id Polygon identifier.
#' @return The value produced by `rings_to_sp_polygon`.
rings_to_sp_polygon <- function(rings_list, id = "1") {
  ## Build an sp::SpatialPolygons from a list of closed rings (outer + holes).
  ## Uses sf for robust validity/orientation, then converts to sp.
  poly_sfc <- st_sfc(st_polygon(rings_list))
  poly_sf  <- st_make_valid(st_sf(geometry = poly_sfc))
  methods::as(poly_sf, "Spatial")
}

## -------------------- top-level: multi-path SVG --------------------------

#' Convert SVG paths into polygon and node data for meshing.
#'
#' @param svg_file Path to the SVG file.
#' @param step Approximate arc-length step.
#' @param n_vertices Number of vertices to sample.
#' @param curve_steps Number of subdivisions per curve command.
#' @return The value produced by `svg_to_sp_and_nodes_multi`.
svg_to_sp_and_nodes_multi <- function(svg_file, step = NULL, n_vertices = NULL, curve_steps = 60L) {
  ## If both 'step' and 'n_vertices' are given, 'step' takes precedence.
  if (is.null(step) && is.null(n_vertices)) {
    stop("Provide either 'step' (preferred) or 'n_vertices'.")
  }

  doc  <- xml2::read_xml(svg_file)
  doc2 <- xml2::xml_ns_strip(doc)

  svg_node <- xml2::xml_find_first(doc2, ".//svg")
  viewBox  <- if (!is.na(xml2::xml_attr(svg_node, "viewBox"))) xml2::xml_attr(svg_node, "viewBox") else NULL

  paths <- xml2::xml_find_all(doc2, ".//path")
  if (length(paths) == 0) stop("No <path> elements found in the SVG.")

  out <- vector("list", length(paths))

  for (k in seq_along(paths)) {
    d <- xml2::xml_attr(paths[[k]], "d")
    if (is.na(d) || !nzchar(d)) stop(sprintf("Path %d has empty 'd' attribute.", k))

    ## Flatten to subpaths and flip Y
    subs <- flatten_svg_path_into_subpaths(d, curve_steps = curve_steps)
    subs <- lapply(subs, flip_y_svg_to_cart, viewBox = viewBox)

    ## Resample each ring: by step if provided, else by n_vertices
    rings <- if (!is.null(step)) {
      lapply(subs, resample_ring_by_step, step = step)
    } else {
      lapply(subs, resample_ring, n_vertices = n_vertices)
    }

    nodes_list <- lapply(rings, function(rg) rg[-nrow(rg), , drop = FALSE])
    sp_poly <- rings_to_sp_polygon(rings, id = as.character(k))

    out[[k]] <- list(
      poly_sp     = sp_poly,
      nodes       = nodes_list[[1]],
      holes_nodes = if (length(nodes_list) > 1) nodes_list[-1] else list(),
      rings       = rings
    )
  }

  out
}
#' Plot all polygons extracted from an SVG file.
#'
#' @param res_list Result list returned by SVG conversion.
#' @param col Fill color vector.
#' @param border Border color.
#' @param ... Named options or values passed through to the helper.
#' @return The value produced by `plot_svg_polygons`.
plot_svg_polygons <- function(res_list, col = NULL, border = NULL, ...) {
  if (length(res_list) == 0) stop("Empty result list - nothing to plot.")

  ## Compute global bounding box from all polygons
  all_polys <- lapply(res_list, \(x) x$poly_sp)
  all_bbox <- do.call(rbind, lapply(all_polys, \(p) bbox(p)))
  ## all_bbox is block-stacked: rbind of 2x2 matrices

  ## Extract min/max across all
  xlim <- range(all_bbox[seq(1, nrow(all_bbox), by = 2), ])
  ylim <- range(all_bbox[seq(2, nrow(all_bbox), by = 2), ])

  ## Colors (auto-recycled if needed)
  if (is.null(col)) col <- rep("grey80", length(res_list))
  if (length(col) < length(res_list)) col <- rep(col, length.out = length(res_list))

  ## Plot first polygon with global limits
  plot(all_polys[[1]], asp = 1, xlim = xlim, ylim = ylim,
       col = col[1], border = border, ...)

  ## Add others
  if (length(all_polys) > 1) {
    for (i in 2:length(all_polys)) {
      plot(all_polys[[i]], asp = 1, xlim = xlim, ylim = ylim,
           add = TRUE, col = col[i], border = border, ...)
    }
  }
}
#' Return an explicit no-CRS object compatible with the installed `sf` version.
#'
#' @return The value produced by `.crs_none`.
.crs_none <- function() {
  if (exists("NA_crs_", asNamespace("sf"))) get("NA_crs_", asNamespace("sf"))
  else sf::st_crs(NA)
}
#' Orient a closed polygon ring counter-clockwise.
#'
#' @param ring Closed polygon ring.
#' @return The value produced by `ensure_ccw`.
ensure_ccw <- function(ring) {
  ring <- as.matrix(ring); storage.mode(ring) <- "double"
  if (!all(ring[1,] == ring[nrow(ring),])) ring <- rbind(ring, ring[1,])
  area2 <- sum(ring[-nrow(ring),1]*ring[-1,2] - ring[-1,1]*ring[-nrow(ring),2])
  if (area2 < 0) ring <- ring[nrow(ring):1, , drop = FALSE]
  ring
}
#' Orient a closed polygon ring clockwise.
#'
#' @param ring Closed polygon ring.
#' @return The value produced by `ensure_cw`.
ensure_cw <- function(ring) {
  ring <- as.matrix(ring); storage.mode(ring) <- "double"
  if (!all(ring[1,] == ring[nrow(ring),])) ring <- rbind(ring, ring[1,])
  area2 <- sum(ring[-nrow(ring),1]*ring[-1,2] - ring[-1,1]*ring[-nrow(ring),2])
  if (area2 > 0) ring <- ring[nrow(ring):1, , drop = FALSE]
  ring
}
#' Classify rings by containment and assemble polygons with holes.
#'
#' @param rings List of closed polygon rings.
#' @return The value produced by `rings_to_polygons_by_containment`.
rings_to_polygons_by_containment <- function(rings) {
  if (length(rings) == 0) return(list())

  ## sanitize & close all rings
  rings <- lapply(rings, function(rg) {
    rg <- as.matrix(rg); storage.mode(rg) <- "double"
    if (ncol(rg) != 2) stop("Each ring must be a 2-column matrix.")
    if (!all(rg[1,] == rg[nrow(rg),])) rg <- rbind(rg, rg[1,])
    rg
  })

  crs_none <- .crs_none()

  ## single-ring polygons (sf, no CRS)
  sfc_single <- st_sfc(lapply(rings, function(rg) st_polygon(list(rg))),
                       crs = crs_none)
  sfc_single <- st_make_valid(sfc_single)

  ## strict containment matrix
  Cmat  <- st_contains_properly(sfc_single, sfc_single, sparse = FALSE)
  depth <- colSums(Cmat)
  areas <- as.numeric(st_area(sfc_single))

  ## parent: smallest container of j (if any)
  parent <- rep(NA_integer_, length(rings))
  for (j in seq_along(rings)) {
    cs <- which(Cmat[, j])
    if (length(cs)) parent[j] <- cs[which.min(areas[cs])]
  }

  ## orient by parity: even=outer(CCW), odd=hole(CW)
  rings_oriented <- lapply(seq_along(rings), function(i) {
    if (depth[i] %% 2 == 0) ensure_ccw(rings[[i]]) else ensure_cw(rings[[i]])
  })

  ## build polygons: each outer collects its immediate children as holes
  outers <- which(depth %% 2 == 0)
  res <- vector("list", length(outers))
  k <- 0
  for (i in outers) {
    k <- k + 1
    holes_idx   <- which(parent == i & depth == depth[i] + 1L)
    holes_rings <- if (length(holes_idx)) rings_oriented[holes_idx] else list()

    poly_sfc <- st_sfc(st_polygon(c(list(rings_oriented[[i]]), holes_rings)),
                       crs = crs_none)
    poly_sf  <- st_make_valid(st_sf(geometry = poly_sfc))
    poly_sp  <- methods::as(poly_sf, "Spatial")

    res[[k]] <- list(
      outer      = rings_oriented[[i]],
      holes      = holes_rings,
      poly_sf    = poly_sf$geometry[[1]],
      poly_sp    = poly_sp
    )
  }

  res
}



####

library(fdaPDE)
#' Build cyclic boundary segments from ordered ring nodes.
#'
#' @param nodes Node coordinate matrix.
#' @return The value produced by `build_segments`.
build_segments <- function(nodes) {
  ## Returns an Sx2 integer matrix of edges (1-2, 2-3, …, n-1, n-1)
  if (all(nodes[1, ] == nodes[nrow(nodes), ])) {
    nodes <- nodes[-nrow(nodes), , drop = FALSE]
  }
  n <- nrow(nodes)
  seg <- cbind(1:n, c(2:n, 1))
  storage.mode(seg) <- "integer"
  seg
}
#' Remove the duplicated closing row from a closed ring.
#'
#' @param ring Closed polygon ring.
#' @return The value produced by `ring_to_nodes`.
ring_to_nodes <- function(ring) {
  ring[-nrow(ring), , drop = FALSE]
}
#' Find an interior point for a closed hole ring.
#'
#' @param ring Closed polygon ring.
#' @return The value produced by `hole_point_inside`.
hole_point_inside <- function(ring) {
  ## Uses sf::st_point_on_surface to guarantee the point is inside
  sfc <- sf::st_sfc(sf::st_polygon(list(ring)), crs = sf::st_crs(NA))
  pt  <- sf::st_point_on_surface(sfc)
  as.numeric(sf::st_coordinates(pt))
}

#' Merge duplicate boundary nodes and update segment indices.
#'
#' @param nodes Node coordinate matrix.
#' @param segments Boundary segment index matrix.
#' @param tol Duplicate-node tolerance.
#' @return The value produced by `remove_duplicate_nodes`.
remove_duplicate_nodes <- function(nodes, segments, tol = 1e-10) {
  ## nodes: Nx2
  ## segments: Sx2 (indices)
  ## Returns: list(nodes = new_nodes, segments = new_segments)

  # Step 1: identify unique rows within tolerance
  # Round is extremely safe and stable for geometry
  key <- paste0(round(nodes[,1] / tol), "_", round(nodes[,2] / tol))
  uniq <- !duplicated(key)

  # Map old -> new indices
  new_index <- cumsum(uniq)
  segments_new <- matrix(new_index[segments], ncol = 2)

  # Step 2: Remove degenerate segments (i -> i)
  deg <- segments_new[,1] == segments_new[,2]
  if (any(deg)) segments_new <- segments_new[!deg, , drop = FALSE]

  list(
    nodes = nodes[uniq, , drop = FALSE],
    segments = segments_new
  )
}
#' Build fdaPDE boundary inputs for one polygon with optional holes.
#'
#' @param poly_group Polygon group with outer ring and holes.
#' @return The value produced by `assemble_boundary_for_fdaPDE`.
assemble_boundary_for_fdaPDE <- function(poly_group) {
  ## poly_group$outer : CLOSED ring (n+1x2, last row equals first)
  ## poly_group$holes : list of CLOSED rings (possibly empty / NULL)

  # outer
  nodes    <- ring_to_nodes(poly_group$outer)
  segments <- build_segments(nodes)

  # holes (optional)
  holes_pts <- NULL
  if (!is.null(poly_group$holes) && length(poly_group$holes)) {
    for (h in poly_group$holes) {
      h_nodes <- ring_to_nodes(h)
      off     <- nrow(nodes)

      nodes     <- rbind(nodes, h_nodes)
      segments  <- rbind(segments, build_segments(h_nodes) + off)
      holes_pts <- rbind(holes_pts, hole_point_inside(h))
    }
    colnames(holes_pts) <- c("x","y")
  }

  storage.mode(nodes) <- "double"
  storage.mode(segments) <- "integer"

  ## ---------------------------------------------------------
  ## SURGICAL FIX: remove duplicate boundary nodes
  ## ---------------------------------------------------------
  dedup <- remove_duplicate_nodes(nodes, segments, tol = 1e-10)
  nodes    <- dedup$nodes
  segments <- dedup$segments
  ## ---------------------------------------------------------

  list(
    nodes = nodes,
    segments = segments,
    holes_pts = holes_pts
  )
}
#' Create and optionally refine an fdaPDE mesh from one polygon group.
#'
#' @param poly_group Polygon group with outer ring and holes.
#' @param ... Named options or values passed through to the helper.
#' @return The value produced by `mesh_from_poly_group`.
mesh_from_poly_group <- function(poly_group, ...) {
  bdry <- assemble_boundary_for_fdaPDE(poly_group)

  if (is.null(bdry$holes_pts)) {
    mesh <- fdaPDE::create.mesh.2D(
      nodes    = bdry$nodes,
      segments = bdry$segments)
  } else {
    mesh <- fdaPDE::create.mesh.2D(
      nodes    = bdry$nodes,
      segments = bdry$segments,
      holes    = bdry$holes_pts)
  }

  ## Refinement
  mesh <- fdaPDE::refine.mesh.2D(mesh, ...)

  list(
    mesh      = mesh,
    nodes     = bdry$nodes,
    segments  = bdry$segments,
    holes_pts = bdry$holes_pts
  )
}
#' Create fdaPDE meshes for multiple polygon groups.
#'
#' @param poly_groups List of polygon groups.
#' @param ... Named options or values passed through to the helper.
#' @return The value produced by `meshes_from_poly_groups`.
meshes_from_poly_groups <- function(poly_groups, ...) {
  lapply(poly_groups, function(pg) {
    mesh_from_poly_group(pg,...)
  })
}

#' Convert an fdaPDE mesh list to the structure expected by femR.
#'
#' @param mesh Mesh object or mesh-list structure.
#' @return The value produced by `fdapde2femR_mesh`.
fdapde2femR_mesh <- function(mesh) {
  mesh_data <- list(
    nodes    = mesh$nodes,
    elements = mesh$triangles,
    boundary = mesh$nodesmarkers
  )
  return(mesh_data)
}

#' Plot multiple meshes together with plotly.
#'
#' @param mesh_list List of mesh objects to plot.
#' @return The value produced by `plot_multi_mesh`.
plot_multi_mesh <- function(mesh_list) {
  p <- plot_ly()  # Start an empty plot

  for(i in seq_along(mesh_list)) {
    mesh <- mesh_list[[i]]$mesh
    femr_mesh <- Mesh(fdapde2femR_mesh(mesh))

    # Extract geometry for overlaying
    coords <- femr_mesh$nodes()
    tris <- femr_mesh$elements()

    # Plot each mesh as a triangular mesh trace
    p <- p %>%
      add_trace(
        x = coords[,1],
        y = coords[,2],
        i = tris[,1] - 1,
        j = tris[,2] - 1,
        k = tris[,3] - 1,
        type = "mesh3d",
        opacity = 0.4
      )
  }

  # Enforce equal aspect ratio
  p <- p %>%
    layout(
      scene = list(
        aspectmode = "data",
        xaxis = list(visible = TRUE),
        yaxis = list(visible = TRUE),
        zaxis = list(visible = FALSE) # 2D so hide Z axis
      )
    )

  p
}

