#' Convert an R time difference to seconds.
#'
#' @param t Time duration object or vector of time points.
#' @return The value produced by `format_time`.
format_time <- function(t) {
  if (attr(t, "units") == "mins") {
    return(as.numeric(t) * 60)
  } else if (attr(t, "units") == "hours") {
    return(as.numeric(t) * 60 * 60)
  } else {
    return(as.numeric(t))
  }
}
#' Append a model-result batch to a tabular result accumulator.
#'
#' @param data Existing result data frame.
#' @param new New result values to append.
#' @param groups_names Optional labels for result groups.
#' @return The value produced by `add_results`.
add_results <- function(data, new, groups_names = NULL) {

  ## Get column names from data
  names_columns <- colnames(data)

  ## Compute the maximum length among new entries
  n <- 0
  for (i in 1:length(new)) {
    n <- max(c(n, length(new[[i]])))
  }

  ## Initialize the new data data.frame
  if (is.null(groups_names)) {
    new_data <- data.frame(Group = paste(1:n))
  } else {
    new_data <- data.frame(Group = groups_names)
  }

  ## Add columns
  for (subgroup in names_columns[-1]) {
    new_data <- cbind(new_data, new[[subgroup]])
  }

  ## Rename columns
  colnames(new_data) <- names_columns

  ## Append to the existing data
  data <- as.data.frame(rbind(data, new_data))
  colnames(data) <- names_columns

  return(data)
}


#' Check whether a result entry is a named nested metric.
#'
#' @param value Value to process.
#' @return The value produced by `is_nested_result_metric`.
is_nested_result_metric <- function(value) {
  is.list(value) && !is.null(names(value)) && length(names(value)) > 0
}
#' Extract one metric from all models in a batch evaluation result.
#'
#' @param results_evaluation Per-model evaluation result list.
#' @param names_models Model names to extract.
#' @param name_result Metric name or nested metric path.
#' @return The value produced by `extract_new_results`.
extract_new_results <- function(results_evaluation, names_models, name_result) {

  ## Room for new results
  new_results <- list()
  for (name_model in names_models) {
    ## Router for reading the data
    if (length(name_result) == 1) {
      new_results[[name_model]] <- results_evaluation[[name_model]][[name_result]]
    } else if (length(name_result) == 2) {
      new_results[[name_model]] <- results_evaluation[[name_model]][[name_result[1]]][[name_result[2]]]
    } else {
      stop()
    }
    ## Convert time units if necessary
    if (length(new_results[[name_model]]) == 1 &&
        "units" %in% names(attributes(new_results[[name_model]]))) {
      new_results[[name_model]] <- format_time(new_results[[name_model]])
    }
    ## Non-tabular list outputs cannot be safely represented in this loader.
    if (is.list(new_results[[name_model]])) {
      new_results[[name_model]] <- c(NaN)
    }
    ## Replace NULL with NaN
    if (is.null(new_results[[name_model]])) {
      new_results[[name_model]] <- c(NaN)
    }
  }
  return(new_results)
}
#' Load and aggregate quantitative results for one option across batches.
#'
#' @param test_options Nested option object loaded from JSON.
#' @param path_list Named list of repository, output, queue, and temporary paths.
#' @return The value produced by `load_quantitative_results`.
load_quantitative_results <- function(test_options, path_list) {
  cat(paste0("\nLoading quantitative results for ", test_options$name_test, " ...\n"))

  ## Get model names, labels, and colors
  model_names  <- test_options$model_names
  model_labels <- test_options$model_labels
  model_colors <- test_options$model_colors

  ## Initialize an empty data.frame template
  names_columns <- c("Group", model_names)
  empty_df <- data.frame(matrix(NaN, nrow = 0, ncol = length(names_columns)))
  colnames(empty_df) <- names_columns

  ## Load the first batch defensively
  batch_index <- 1
  ok <- tryCatch({
    path_batch <- file.path(path_list$results, paste0("batch_", batch_index))
    load(file.path(path_batch, paste0("batch_", batch_index, "_results_evaluation.RData")))
    TRUE
  }, error = function(e) {
    cat(sprintf("Error in test %s - batch %d: %s\n", test_options$name_test, batch_index, conditionMessage(e)))
    FALSE
  })
  if (!ok) stop("The first batch is not present!")

  ## Create containers for each entry in results_evaluation
  res <- list()
  for (entry in names(results_evaluation[[1]])) {
    if (!is_nested_result_metric(results_evaluation[[1]][[entry]])) {
      res[[entry]] <- empty_df
    } else {
      res[[entry]] <- list()
      for (sub_entry in names(results_evaluation[[1]][[entry]])) {
        res[[entry]][[sub_entry]] <- empty_df
      }
    }
  }

  ## Load all batches sequentially
  n_reps <- test_options$test_options$n_reps
  for (batch_index in seq_len(n_reps)) {

    ## Safely load batch file
    ok <- tryCatch({
      path_batch <- file.path(path_list$results, paste0("batch_", batch_index))
      load(file.path(path_batch, paste0("batch_", batch_index, "_results_evaluation.RData")))
      TRUE
    }, error = function(e) {
      cat(sprintf("Error in test %s - batch %d: %s\n", test_options$name_test, batch_index, conditionMessage(e)))
      FALSE
    })
    if (!ok) next

    ## Check that results_evaluation exists
    if (!exists("results_evaluation", inherits = FALSE)) {
      cat(sprintf("Warning: no `results_evaluation` found in batch %d file; skipping.\n", batch_index))
      next
    }

    ## Append results for each entry and sub-entry
    for (entry in names(res)) {
      if (is.data.frame(res[[entry]])) {
        res[[entry]] <- add_results(
          res[[entry]],
          extract_new_results(results_evaluation, model_names, entry)
        )
      } else {
        for (sub_entry in names(res[[entry]])) {
          res[[entry]][[sub_entry]] <- add_results(
            res[[entry]][[sub_entry]],
            extract_new_results(results_evaluation, model_names, c(entry, sub_entry))
          )
        }
      }
    }

    cat(sprintf("- Batch %d loaded\n", batch_index))
  }

  cat("\n")

  ## Attach metadata
  res$model_names  <- model_names
  res$model_labels <- model_labels
  res$model_colors <- model_colors
  res$varying_options <- test_options$test_options$varying_options

  return(res)
}
#' Read a nested-list value by an explicit path.
#'
#' @param x Nested list to traverse.
#' @param path Character vector of names.
#' @return The nested value when the path exists; otherwise `NULL`.
get_nested_value <- function(x, path) {
  value <- x
  for (name in path) {
    if (!is.list(value) || is.null(value[[name]])) {
      return(NULL)
    }
    value <- value[[name]]
  }
  value
}


#' Find all paths to a named field inside a nested option object.
#'
#' @param x Object to search.
#' @param target Field name to locate.
#' @param path Current recursion path.
#' @return A list of character-vector paths.
find_option_paths <- function(x, target, path = character()) {
  if (!is.list(x)) {
    return(list())
  }

  names_x <- names(x)
  if (is.null(names_x)) {
    return(list())
  }

  paths <- list()
  for (i in seq_along(x)) {
    name <- names_x[i]
    if (is.na(name) || !nzchar(name)) {
      next
    }

    new_path <- c(path, name)
    if (identical(name, target)) {
      paths[[length(paths) + 1L]] <- new_path
    }

    child_paths <- find_option_paths(x[[i]], target, new_path)
    if (length(child_paths) > 0) {
      paths <- c(paths, child_paths)
    }
  }

  paths
}


#' Resolve a varying-option value from a nested option object.
#'
#' @param opt_name Option name or dotted option path.
#' @param test_options Nested option object loaded from JSON.
#' @return The value produced by `resolve_option_value`.
resolve_option_value <- function(opt_name, test_options) {
  if (grepl("\\.", opt_name)) {
    parts <- strsplit(opt_name, "\\.")[[1]]
    value <- get_nested_value(test_options, parts)
    if (is.null(value)) {
      return(NA)
    }
    return(value)
  }

  paths <- find_option_paths(test_options, opt_name)
  if (length(paths) == 0) {
    return(NA)
  }

  if (length(paths) > 1) {
    stop(
      paste0(
        "Ambiguous varying option '", opt_name, "' found at: ",
        paste(vapply(paths, paste, character(1), collapse = "."), collapse = ", "),
        ". Use a dotted path in varying_options to disambiguate."
      ),
      call. = FALSE
    )
  }

  get_nested_value(test_options, paths[[1]])
}
#' Add varying-option columns to a result data frame.
#'
#' @param df Data frame to augment.
#' @param varying_vals Named list of varying option values.
#' @return The value produced by `inject_varying_columns`.
inject_varying_columns <- function(df, varying_vals) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    ## Still add columns so the schema is correct
    for (vn in names(varying_vals)) {
      if (is.null(df[[vn]])) df[[vn]] <- numeric(0)
    }
    ## Try to place after Group when columns exist later
    return(df)
  }
  insert_after <- match("Group", colnames(df))
  for (vn in names(varying_vals)) {
    val <- varying_vals[[vn]]
    col_vec <- rep(val, nrow(df))
    if (vn %in% colnames(df)) next
    if (!is.na(insert_after)) {
      ## Insert after "Group"
      left_cols  <- colnames(df)[seq_len(insert_after)]
      right_cols <- colnames(df)[-seq_len(insert_after)]
      df <- cbind(
        df[left_cols],
        setNames(list(col_vec), vn),
        df[right_cols],
        stringsAsFactors = FALSE
      )
    } else {
      df[[vn]] <- col_vec
    }
  }
  df
}
#' Recursively add varying-option columns to every data frame in a result structure.
#'
#' @param x Input object.
#' @param varying_vals Named list of varying option values.
#' @return The value produced by `add_varying_to_all_dfs`.
add_varying_to_all_dfs <- function(x, varying_vals) {
  if (is.data.frame(x)) {
    return(inject_varying_columns(x, varying_vals))
  } else if (is.list(x)) {
    for (nm in names(x)) x[[nm]] <- add_varying_to_all_dfs(x[[nm]], varying_vals)
    return(x)
  }
  x
}
#' Recursively append one result structure into another.
#'
#' @param dst Destination result structure.
#' @param src Source result structure to append.
#' @return The value produced by `accumulate_results_struct`.
accumulate_results_struct <- function(dst, src) {
  if (is.data.frame(dst) && is.data.frame(src)) {
    cols <- colnames(dst)
    if (!all(colnames(src) %in% cols)) stop("Column mismatch in accumulation (src).")
    return(rbind(dst, src))
  } else if (is.list(dst) && is.list(src)) {
    for (nm in names(src)) {
      dst[[nm]] <- accumulate_results_struct(dst[[nm]], src[[nm]])
    }
    return(dst)
  }
  return(dst)
}
#' Load quantitative results for every option JSON in a test queue.
#'
#' @param path_list Named list of repository, output, queue, and temporary paths.
#' @param name_main_test Main test name or test-group name.
#' @return The value produced by `load_all_quantitative_results`.
load_all_quantitative_results <- function(path_list, name_main_test) {

  ## Discover available options
  file_options_list <- sort(list.files(path_list$queue), decreasing = FALSE)
  if (length(file_options_list) == 0) {
    stop("No option files found in the queue for this test.")
  }

  cat.script_title(paste("Results Loader —", TEST_SUITE))
  cat.section_title("Target")
  cat(paste0("- Test: ", test_suite, "/", name_main_test, "\n"))
  cat(paste0("- Options found: ", length(file_options_list), "\n\n"))

  ## Container for all loaded results
  all_results <- NULL

  ## Iterate options and load quantitative results
  for (file_options in file_options_list) {  # file_options <- file_options_list[1]
    ## Load option JSON
    test_options <- jsonlite::fromJSON(paste0(path_list$queue, file_options))

    ## Update paths for this specific option (so load_quantitative_results finds batches)
    path_list_i <- update_paths(path_list, name_main_test, test_options)

    ## Load quantitative results for this combination (all batches)
    loaded_results <- load_quantitative_results(test_options, path_list_i)

    ## Get varying options
    varying_options <- test_options$test_options$varying_options
    if (is.null(varying_options)) varying_options <- character(0)

    ## Resolve their values inside test_options
    varying_vals <- setNames(vector("list", length(varying_options)), varying_options)
    for (vn in varying_options) varying_vals[[vn]] <- resolve_option_value(vn, test_options)

    ## Add one column per varying option to each data.frame in the results
    loaded_results_with_vars <- add_varying_to_all_dfs(loaded_results, varying_vals)

    ## Accumulate results
    if (is.null(all_results)) {
      all_results <- loaded_results_with_vars
    } else {
      all_results <- accumulate_results_struct(all_results, loaded_results_with_vars)
    }

    ## Optional: keep the queue clean, mirroring previous workflow
    file.remove(paste0(path_list$queue, file_options))
  }

  return(all_results)
}
