# = ========================================================================== =
# - Script: options.R
# - Desc: Utility functions for expanding nested option lists and writing JSONs
# = ========================================================================== =


## Function: find_name_path
# - Args:
#   * x: nested list to be searched
#   * target: name of the field to find
#   * path: internal recursion argument (do not modify)
# - Desc:
#   Recursively searches for a field within a nested list and returns
#   the unique path to that field as a character vector.
#   If not found, returns list(found = FALSE).
find_name_path <- function(x, target, path = character()) {
  if (!is.list(x)) return(list(found = FALSE))
  nms <- names(x)
  for (i in seq_along(x)) {
    nm <- nms[i]
    newpath <- c(path, nm)
    if (identical(nm, target)) return(list(found = TRUE, path = newpath))
    res <- find_name_path(x[[i]], target, newpath)
    if (isTRUE(res$found)) return(res)
  }
  list(found = FALSE)
}


## Function: get_by_path
# - Args:
#   * x: nested list
#   * path: character vector of keys (e.g. c("dimensions", "n_nodes"))
# - Desc:
#   Returns the value of the nested field specified by 'path' inside list 'x'.
get_by_path <- function(x, path) {
  Reduce(function(acc, key) acc[[key]], path, init = x)
}


## Function: set_by_path
# - Args:
#   * x: nested list
#   * path: character vector specifying where to assign the value
#   * value: object to assign
# - Desc:
#   Sets a nested field in list 'x' at the position defined by 'path'
#   and returns the modified list.
set_by_path <- function(x, path, value) {
  if (length(path) == 1L) {
    x[[path]] <- value
  } else {
    x[[path[1L]]] <- set_by_path(x[[path[1L]]], path[-1L], value)
  }
  x
}


## Function: explode_options
# - Args:
#   * options: nested list representing the full JSON structure
#   * by: character vector of field names to combine (Cartesian product)
#   * formatters: optional named list of formatting functions
#                 (applied to specific 'by' values for naming)
#   * name_fun: optional function(opts_i, comb_row) returning a string
#               to set opts_i$name_test
# - Desc:
#   Expands the nested 'options' list by generating one configuration
#   for each combination of fields listed in 'by'. Returns a list of
#   expanded options, each ready to be written to JSON.
explode_options <- function(options, by, formatters = NULL, name_fun = NULL) {
  
  ## Locate paths for each 'by' field and get their vectors
  paths <- lapply(by, function(nm) {
    res <- find_name_path(options, nm)
    if (!isTRUE(res$found)) stop(sprintf("Field '%s' not found in options.", nm))
    res$path
  })
  names(paths) <- by
  
  ## Extract values for each field in 'by'
  grids <- lapply(by, function(nm) {
    val <- get_by_path(options, paths[[nm]])
    if (is.null(val)) stop(sprintf("Field '%s' is NULL.", nm))
    if (!is.atomic(val)) stop(sprintf("Field '%s' must be an atomic vector for 'by'.", nm))
    val
  })
  names(grids) <- by
  
  ## Build the Cartesian product
  comb <- do.call(expand.grid, c(grids, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE))
  
  ## Create one option list per combination
  out <- vector("list", nrow(comb))
  for (i in seq_len(nrow(comb))) {
    opt_i <- options
    
    ## Substitute scalars for the 'by' fields
    for (nm in by) {
      v <- comb[[nm]][i]
      if (is.list(v)) v <- v[[1L]]  # safeguard
      opt_i <- set_by_path(opt_i, paths[[nm]], v)
    }
    
    ## Build or refresh name_test
    if (!is.null(name_fun)) {
      opt_i$name_test <- name_fun(opt_i, comb[i, , drop = FALSE])
    } else if (is.null(opt_i$name_test)) {
      ## Default name: only 'by' fields with optional formatting
      parts <- mapply(function(nm, val) {
        vv <- val
        if (!is.null(formatters) && !is.null(formatters[[nm]]))
          vv <- formatters[[nm]](vv)
        paste0(nm, "_", as.character(vv))
      }, nm = by, val = as.list(comb[i, by, drop = FALSE]))
      opt_i$name_test <- paste(unlist(parts), collapse = "_")
    }
    
    out[[i]] <- opt_i
  }
  
  attr(out, "combinations") <- comb
  out
}


## Function: write_options_json
# - Args:
#   * options_list: list of expanded option objects
#   * dir: output directory for the JSON files
#   * name_field: name of the field containing the filename stem (default: "name_test")
#   * pretty: logical, whether to pretty-print JSON (default: TRUE)
#   * auto_unbox: logical, whether to simplify scalars (default: TRUE)
# - Desc:
#   Writes each options object in 'options_list' to a JSON file using the
#   specified naming convention. Creates the output directory if needed.
write_options_json <- function(options_list, dir, name_field = "name_test",
                               pretty = TRUE, auto_unbox = TRUE) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  for (opt in options_list) {
    nm <- opt[[name_field]]
    if (is.null(nm)) stop("Missing 'name_field' in one options element.")
    write_json(
      opt,
      file.path(dir, paste0(nm, ".json")),
      pretty = pretty,
      auto_unbox = auto_unbox,
      digits = NA
    )
  }
}