# = ========================================================================== =
# - Test: RGCCA - 2D - Aggregate results
# - Desc: Loads quantitative results for all options of a selected test and
#         aggregates them in plots.
# - Args:
#     [1] name_main_test : name of the main test to scan.
# = ========================================================================== =

rm(list = ls())
graphics.off()
options(warn = -1)

width <- 13
height <- 15

# README ----
# Assumes all batches for each option have already been run and saved.
# For each option JSON in the queue/<test>/, this script loads the corresponding
# quantitative results and collects them.

# Configuration ----

## Load libraries ----
invisible(suppressMessages(sapply(c(
  # discretization
  "femR",
  # algebraic utils
  "pracma",
  # data manipulation
  "MASS", "tidyr", "dplyr",
  # plotting and utilities
  "ggplot2", "viridis", "stringr", "RColorBrewer", "grid", "gridExtra",
  # json
  "jsonlite",
  # sampling
  "sf", "sp", "raster"
), require, character.only = TRUE)))

## Load functions ----
source("src/utils/cat.R")
source("src/utils/directories.R")
source("src/utils/test_groups.R")
source("src/utils/options.R")
source("src/utils/load_results_utils.R")
source("src/utils/plotting_utils.R")

## Load configuration file
path_this <- get_script_path()
source(paste0(path_this, "config.R"))

## Load test-specific helpers
source(paste0("tests/", test_suite, "/utils/generate_options.R"))

## Create suite directories ----
path_list <- create_paths(test_suite)

# Loader ----

## Generate options ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  INTERACTIVE <- TRUE
  requested_main_tests <- name_main_test_default
} else {
  INTERACTIVE <- FALSE
  requested_main_tests <- args
}

name_main_tests <- resolve_test_names(test_suite, requested_main_tests)
name_output_test <- if (length(requested_main_tests) == 1) {
  requested_main_tests[1]
} else {
  paste(requested_main_tests, collapse = "_")
}

loaded_results <- NULL
expected_varying_options <- NULL

for (name_main_test in name_main_tests) {
  path_list_i <- path_list

  ## Prepare queue/log dirs for this test
  path_list_i$queue <- paste0(path_list_i$queue, name_main_test, "/")
  path_list_i$logs <- paste0(path_list_i$logs, name_main_test, "/")
  mkdir(c(path_list_i$queue, path_list_i$logs))

  ## Generate all option files for this test
  generate_options(test_suite, name_main_test, path_list_i$queue)

  ## Load results (all option combinations, all batches)
  loaded_results_i <- load_all_quantitative_results(path_list_i, name_main_test)

  if (is.null(expected_varying_options)) {
    expected_varying_options <- loaded_results_i$varying_options
  } else if (!identical(expected_varying_options, loaded_results_i$varying_options)) {
    stop(
      paste0(
        "Cannot aggregate tests with different varying_options: ",
        paste(name_main_tests, collapse = ", ")
      )
    )
  }

  loaded_results <- if (is.null(loaded_results)) {
    loaded_results_i
  } else {
    accumulate_results_struct(loaded_results, loaded_results_i)
  }
}

# Plot aggregated results ----

## Create the path for images
path_list$images <- paste0(path_list$images, name_output_test, "/")
mkdir(path_list$images)

## Define an order for the varying options
if (is.null(order) || length(order) != length(loaded_results$varying_options)) {
  order <- 1:length(loaded_results$varying_options)
}

finite_values <- function(data_plot) {
  if (is.null(data_plot)) {
    return(numeric(0))
  }
  values <- suppressWarnings(as.numeric(unlist(data_plot[loaded_results$model_names])))
  values[is.finite(values)]
}

metric_limits <- function(data_plot, lower = 0) {
  values <- finite_values(data_plot)
  if (length(values) == 0) {
    return(NULL)
  }
  c(lower, max(values, na.rm = TRUE))
}

plot_metric <- function(data_plot, title_prefix, values_name, limits = NULL,
                        plots_catalog = NULL) {
  if (length(finite_values(data_plot)) == 0) {
    return(invisible(FALSE))
  }
  plot.aggregated_data(
    loaded_results, data_plot, title_prefix, values_name,
    order = order, limits = limits, plots_catalog = plots_catalog
  )
  invisible(TRUE)
}

metric_long <- function(data_plot, metric_name, positive = FALSE) {
  if (length(finite_values(data_plot)) == 0) {
    return(NULL)
  }

  id_cols <- intersect(c("Group", loaded_results$varying_options), names(data_plot))
  out <- do.call(rbind, lapply(loaded_results$model_names, function(model_name) {
    if (!model_name %in% names(data_plot)) {
      return(NULL)
    }
    data.frame(
      metric = metric_name,
      data_plot[id_cols],
      model_name = model_name,
      model = loaded_results$model_labels[match(model_name, loaded_results$model_names)],
      value = suppressWarnings(as.numeric(data_plot[[model_name]])),
      check.names = FALSE
    )
  }))
  if (is.null(out)) {
    return(NULL)
  }
  out <- out[is.finite(out$value), , drop = FALSE]
  if (positive) {
    out <- out[out$value > 0, , drop = FALSE]
  }
  if (nrow(out) == 0) {
    return(NULL)
  }
  names(out)[names(out) == "Group"] <- "component"
  out
}

format_fixed <- function(x) sprintf("%.2f", x)
format_lambda <- function(x) sprintf("%.1e", x)

format_summary_cell <- function(x, formatter = format_fixed) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return("")
  }
  q <- quantile(x, c(0.25, 0.5, 0.75), na.rm = TRUE)
  if (isTRUE(all.equal(q[[1]], q[[3]]))) {
    return(formatter(q[[2]]))
  }
  paste0(formatter(q[[2]]), " [", formatter(q[[1]]), ", ", formatter(q[[3]]), "]")
}

table_row_cols <- function(data) {
  intersect(loaded_results$varying_options, names(data))
}

keep_selection_rows <- function(data) {
  if (!is.null(data) && "lambda" %in% names(data)) {
    data <- data[data$lambda < 0 | is.na(data$lambda), , drop = FALSE]
  }
  data
}

table_row_label <- function(data) {
  row_cols <- table_row_cols(data)
  if (length(row_cols) == 0) {
    return(rep("all", nrow(data)))
  }
  apply(data[row_cols], 1, function(row) {
    paste(paste0(row_cols, "=", row), collapse = ", ")
  })
}

wide_tables_by_component <- function(summary) {
  if (is.null(summary) || nrow(summary) == 0) {
    return(list())
  }
  lapply(sort(unique(summary$component)), function(component) {
    table <- summary[summary$component == component, , drop = FALSE]
    table$row <- table_row_label(table)
    table <- table[, c("row", "model", "cell", "accuracy"), drop = FALSE]
    table <- tidyr::pivot_wider(table, names_from = model, values_from = c(cell, accuracy))
    cell_cols <- grep("^cell_", names(table), value = TRUE)
    accuracy_cols <- sub("^cell_", "accuracy_", cell_cols)
    out <- as.data.frame(table[, c("row", cell_cols), drop = FALSE], check.names = FALSE)
    names(out) <- sub("^cell_", "", names(out))
    attr(out, "accuracy") <- as.data.frame(table[, accuracy_cols, drop = FALSE], check.names = FALSE)
    names(attr(out, "accuracy")) <- sub("^accuracy_", "", names(attr(out, "accuracy")))
    attr(out, "component") <- component
    out
  })
}

highlight_perfect_accuracy <- function(grob, table) {
  accuracy <- attr(table, "accuracy")
  if (is.null(accuracy)) {
    return(grob)
  }
  for (j in seq_along(accuracy)) {
    perfect <- which(is.finite(accuracy[[j]]) & accuracy[[j]] >= 1)
    if (length(perfect) == 0) {
      next
    }
    for (i in perfect) {
      grob <- gtable::gtable_add_grob(
        grob,
        grid::rectGrob(gp = grid::gpar(fill = "#C7E9C0", col = NA)),
        t = i + 1,
        l = j + 1,
        b = i + 1,
        r = j + 1,
        z = 0
      )
    }
  }
  grob
}

component_table_grob <- function(table, title, theme) {
  component <- attr(table, "component")
  gridExtra::arrangeGrob(
    grid::textGrob(
      paste("Component", component),
      x = 0,
      hjust = 0,
      gp = grid::gpar(fontsize = 9, fontface = "bold")
    ),
    highlight_perfect_accuracy(
      gridExtra::tableGrob(table, rows = NULL, theme = theme),
      table
    ),
    ncol = 1,
    heights = grid::unit.c(grid::unit(0.25, "in"), grid::unit(1, "null"))
  )
}

write_tables_pdf <- function(tables, path, title, note = NULL) {
  tables <- tables[vapply(tables, nrow, integer(1)) > 0]
  if (length(tables) == 0) {
    return(invisible(FALSE))
  }

  pdf(path, width = 13, height = 8.5)
  on.exit(dev.off(), add = TRUE)
  theme <- gridExtra::ttheme_minimal(base_size = 7)
  title_grob <- grid::textGrob(
    title,
    x = 0.01,
    hjust = 0,
    gp = grid::gpar(fontsize = 12, fontface = "bold")
  )
  note_grob <- if (!is.null(note)) {
    grid::textGrob(
      note,
      x = 0.01,
      hjust = 0,
      gp = grid::gpar(fontsize = 8)
    )
  } else {
    NULL
  }
  grobs <- lapply(tables, component_table_grob, title = title, theme = theme)
  gridExtra::grid.arrange(
    grobs = grobs,
    ncol = 1,
    top = title_grob,
    bottom = note_grob
  )
  invisible(TRUE)
}

write_lambda_tables <- function(output_dir) {
  data <- keep_selection_rows(metric_long(
    loaded_results$model_selection$lambda_weights,
    "lambda_weights",
    positive = TRUE
  ))
  if (is.null(data) || nrow(data) == 0) {
    return(invisible(FALSE))
  }

  group_cols <- c("component", table_row_cols(data), "model")
  summary <- data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      n = n(),
      cell = format_summary_cell(value, format_lambda),
      median = median(value),
      q25 = quantile(value, 0.25),
      q75 = quantile(value, 0.75),
      accuracy = NA_real_,
      .groups = "drop"
    )

  write.csv(summary, file.path(output_dir, "lambda_selection_grouped.csv"), row.names = FALSE)
  write_tables_pdf(
    wide_tables_by_component(summary),
    file.path(output_dir, "lambda_selection_tables.pdf"),
    "Selected lambda",
    "cell = median [q25, q75] across batches"
  )
}

write_selection_tables <- function(output_dir, prefix, title) {
  accuracy <- keep_selection_rows(metric_long(
    loaded_results$model_selection[[paste0(prefix, "_accuracy")]],
    paste0(prefix, "_accuracy")
  ))
  fp <- keep_selection_rows(metric_long(
    loaded_results$model_selection[[paste0(prefix, "_false_positive")]],
    paste0(prefix, "_false_positive")
  ))
  fn <- keep_selection_rows(metric_long(
    loaded_results$model_selection[[paste0(prefix, "_false_negative")]],
    paste0(prefix, "_false_negative")
  ))
  selected <- keep_selection_rows(metric_long(
    loaded_results$model_selection[[paste0(prefix, "_selected")]],
    paste0(prefix, "_selected")
  ))
  if (is.null(accuracy)) {
    return(invisible(FALSE))
  }

  key_cols <- c("component", intersect(loaded_results$varying_options, names(accuracy)), "model_name", "model")
  values <- accuracy[, c(key_cols, "value"), drop = FALSE]
  names(values)[ncol(values)] <- "accuracy"
  optional_metrics <- list(
    false_positive = fp,
    false_negative = fn,
    selected = selected
  )
  for (name in names(optional_metrics)) {
    data <- optional_metrics[[name]]
    if (is.null(data)) {
      next
    }
    data <- data[, c(key_cols, "value"), drop = FALSE]
    names(data)[ncol(data)] <- name
    values <- dplyr::left_join(values, data, by = key_cols)
  }
  for (name in names(optional_metrics)) {
    if (!name %in% names(values)) {
      values[[name]] <- NA_real_
    }
  }

  group_cols <- c("component", table_row_cols(values), "model")
  summary <- values %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      n = n(),
      accuracy = mean(accuracy, na.rm = TRUE),
      false_positive = mean(false_positive, na.rm = TRUE),
      false_negative = mean(false_negative, na.rm = TRUE),
      selected = mean(selected, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      cell = sprintf("%.2f | %.1f | %.1f | %.1f", accuracy, false_positive, false_negative, selected)
    )

  write.csv(summary, file.path(output_dir, paste0(prefix, "_grouped.csv")), row.names = FALSE)
  write_tables_pdf(
    wide_tables_by_component(summary),
    file.path(output_dir, paste0(prefix, "_tables.pdf")),
    title,
    "cell = mean accuracy | mean FP | mean FN | mean selected; green = 100% accuracy"
  )
}

### Time complexity ----

data_plot <- loaded_results$execution_time
title_prefix <- "Execution times w.r.t the"
values_name <- "Time [seconds]"
limits <- metric_limits(data_plot)

plots_catalog <- list(
  boxplots = TRUE,
  lines = FALSE,
  logx = FALSE,
  loglog = FALSE,
  normalized = FALSE
)

pdf(paste0(path_list$images, "time_complexity.pdf"), width = width, height = height)
plot_metric(data_plot, title_prefix, values_name, limits, plots_catalog)
dev.off()

### Objective ----

pdf(paste0(path_list$images, "objective.pdf"), width = width, height = height)

data_plot <- loaded_results$objective
if (!is.null(data_plot)) {
  data_plot[loaded_results$model_names] <- lapply(
    data_plot[loaded_results$model_names],
    function(x) log10(x + 0.1)
  )
}
title_prefix <- "log10(Objective) w.r.t"
values_name <- "log10(Obj)"
values <- finite_values(data_plot)
limits <- if (length(values) == 0) NULL else range(values, na.rm = TRUE)
plot_metric(data_plot, title_prefix, values_name, limits)

dev.off()

### Diagnostics ----

if (!is.null(loaded_results$diagnostics)) {
  pdf(paste0(path_list$images, "diagnostics.pdf"), width = width, height = height)

  diagnostic_plots <- list(
    iterations = list(
      title = "Iterations w.r.t",
      values = "Iterations",
      plots_catalog = list(
        boxplots = TRUE,
        lines = FALSE,
        logx = FALSE,
        loglog = FALSE,
        normalized = FALSE,
        boxplot_adaptive_limits = TRUE
      )
    ),
    bootstrap_resamples = list(
      title = "Effective bootstrap resamples w.r.t",
      values = "Resamples"
    )
  )

  for (metric_name in names(diagnostic_plots)) {
    data_plot <- loaded_results$diagnostics[[metric_name]]
    limits <- metric_limits(data_plot)
    plot_metric(
      data_plot,
      diagnostic_plots[[metric_name]]$title,
      diagnostic_plots[[metric_name]]$values,
      limits,
      diagnostic_plots[[metric_name]]$plots_catalog
    )
  }

  dev.off()
}

### Model selection ----

if (!is.null(loaded_results$model_selection)) {
  old_model_selection_pdf <- file.path(path_list$images, "model_selection.pdf")
  if (file.exists(old_model_selection_pdf)) {
    file.remove(old_model_selection_pdf)
  }
  path_tables <- file.path(path_list$images, "model_selection_tables")
  mkdir(path_tables)

  write_lambda_tables(path_tables)
  write_selection_tables(path_tables, "active_blocks", "Active blocks")
  write_selection_tables(path_tables, "active_connections", "Active connections")
}

### RMSE ----

n_groups <- 4

for (g in 1:n_groups) {
  pdf(paste0(path_list$images, "rmse_g", g, ".pdf"), width = width, height = height)

  #### Cumulative RMSE[A] at locations ----
  data_plot <- loaded_results$rmse[[paste0("A", g, "_locs_all")]]
  plot_metric(
    data_plot,
    paste0("Cumulative RMSE[A", g, "] at locations w.r.t"),
    "RMSE",
    metric_limits(data_plot)
  )

  #### RMSE[A] at locations ----
  data_plot <- loaded_results$rmse[[paste0("A", g, "_locs")]]
  plot_metric(
    data_plot,
    paste0("RMSE[A", g, "] at locations w.r.t"),
    "RMSE",
    metric_limits(data_plot)
  )

  #### Cumulative RMSE[A_star] at locations ----
  data_plot <- loaded_results$rmse[[paste0("A_star", g, "_locs_all")]]
  plot_metric(
    data_plot,
    paste0("Cumulative RMSE[A_star", g, "] at locations w.r.t"),
    "RMSE",
    metric_limits(data_plot)
  )

  #### RMSE[A_star] at locations ----
  data_plot <- loaded_results$rmse[[paste0("A_star", g, "_locs")]]
  plot_metric(
    data_plot,
    paste0("RMSE[A_star", g, "] at locations w.r.t"),
    "RMSE",
    metric_limits(data_plot)
  )

  #### Cumulative RMSE[E] at locations ----
  data_plot <- loaded_results$rmse[[paste0("E", g, "_locs_all")]]
  plot_metric(
    data_plot,
    paste0("Cumulative RMSE[E", g, "] at locations w.r.t"),
    "RMSE",
    metric_limits(data_plot)
  )

  #### RMSE[E] at locations ----
  data_plot <- loaded_results$rmse[[paste0("E", g, "_locs")]]
  plot_metric(
    data_plot,
    paste0("RMSE[E", g, "] at locations w.r.t"),
    "RMSE",
    metric_limits(data_plot)
  )

  dev.off()
}

## Open the results directory
open(path_list$images)
