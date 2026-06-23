#' Return the standard ggplot theme for summary plots.
#'
#' @return The value produced by `std_plot_settings`.
std_plot_settings <- function() {

  ## Create standard theme
  standard_plot_settings <- theme_bw() +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(
        color = "black",
        face = "bold",
        size = 14,
        hjust = 0.5,
        vjust = 1
      ),
      legend.position = "top",
    )
}
#' Return the standard ggplot theme for spatial field plots.
#'
#' @return The value produced by `std_plot_settings_fields`.
std_plot_settings_fields <- function() {

  ## Create theme for field visualization
  standard_plot_settings_fields <- theme_minimal() +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(
        color = "black",
        face = "bold",
        size = 14,
        hjust = 0.5,
        vjust = 1
      ),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.text.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(angle = 45, hjust = 1)
    )
}
#' Return the standard ggplot theme for curve plots.
#'
#' @return The value produced by `std_plot_settings_curves`.
std_plot_settings_curves <- function() {

  ## Create theme for field visualization
  standard_plot_settings_fields <- theme_light() +
    theme(
      text = element_text(size = 12),
      plot.title = element_text(
        color = "black",
        face = "bold",
        size = 14,
        hjust = 0.5,
        vjust = 1
      ),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # axis.text.x = element_blank(),
      # axis.text.y = element_blank(),
      # axis.ticks.x = element_blank(),
      # axis.ticks.y = element_blank(),
      # panel.grid.major = element_blank(),
      # panel.grid.minor = element_blank(),
      legend.text.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(angle = 45, hjust = 1)
    )
}
#' Plot two-dimensional points with optional grouping and boundaries.
#'
#' @param locations Evaluation or plotting locations.
#' @param boundary Optional domain boundary object.
#' @param group Optional group labels.
#' @param group_name Label for groups.
#' @param group_colors Colors used for groups.
#' @param group_labels Labels used for groups.
#' @param size Point size.
#' @param LEGEND Whether to show a legend.
#' @return The value produced by `plot.points`.
plot.points <- function(locations, boundary = NULL, group = NULL,
                        group_name = "Groups", group_colors = NULL, group_labels = NULL,
                        size = 1, LEGEND = FALSE) {

  ## Assemble data
  if (is.null(group)) {
    group <- rep(0, nrow(locations))
  }
  data <- data.frame(locations, group)
  colnames(data) <- c("x", "y", "Group")

  ## Grouping
  group_levels <- unique(data$Group)
  if (is.null(group_labels)) {
    group_labels <- group_levels
  }

  ## Define association between group labels and colors
  if (is.null(group_colors)) {
    if (length(group_labels) > 1) {
      group_colors <- rainbow(length(group_labels))
    } else {
      group_colors <- c("#000000")
    }
  }
  names(group_colors) <- group_labels

  ## Refactor categorical variables
  data <- data %>%
    mutate(Group = factor(Group, levels = group_levels, labels = group_labels))

  ## Build plot
  plot <- ggplot() +
    geom_point(data = data, aes(x = x, y = y, color = Group), size = size) +
    coord_fixed() +
    scale_color_manual(
      name = group_name,
      values = group_colors,
      labels = group_labels
    )

  ## Add boundary if provided
  if (!is.null(boundary)) {
    plot <- plot +
      geom_polygon(
        data = fortify(boundary),
        aes(x = long, y = lat),
        fill = "transparent",
        color = "black",
        linewidth = 1
      )
  }

  ## Add or remove legend
  if (!LEGEND) {
    plot <- plot + guides(color = "none")
  }

  return(plot)
}
#' Coerce curve data into a matrix with one curve per column.
#'
#' @param locations Evaluation or plotting locations.
#' @param obj Curve values as a vector, matrix, or list of vectors.
#' @param prefix Column-name prefix used when names are missing.
#' @return The value produced by `as_curve_matrix`.
as_curve_matrix <- function(locations, obj, prefix = "curve") {
  if (is.null(obj)) return(NULL)

  if (is.list(obj)) {
    cols <- lapply(obj, function(z) {
      z <- as.numeric(z)
      if (length(z) != length(locations))
        stop("All list elements must have length equal to length(locations).")
      z
    })
    M <- do.call(cbind, cols)
    cn <- names(obj)
    if (is.null(cn)) cn <- paste0(prefix, seq_len(ncol(M)))
    colnames(M) <- cn
    return(M)
  }

  if (is.vector(obj) && !is.list(obj)) {
    if (length(obj) != length(locations))
      stop("'f' (vector) must have length equal to length(locations).")
    M <- matrix(as.numeric(obj), ncol = 1)
    colnames(M) <- prefix
    return(M)
  }

  if (is.matrix(obj)) {
    if (nrow(obj) != length(locations))
      stop("'f' (matrix) must have nrow equal to length(locations).")
    M <- obj
    if (is.null(colnames(M)))
      colnames(M) <- paste0(prefix, seq_len(ncol(M)))
    return(M)
  }

  stop("Unsupported type for curves. Use vector, matrix, or list of vectors.")
}
#' Convert a curve matrix to long format for ggplot.
#'
#' @param x Input object.
#' @param M Curve matrix with one curve per column.
#' @return The value produced by `to_long`.
to_long <- function(x, M) {
  k <- ncol(M)
  data.frame(
    x = rep(x, times = k),
    y = as.numeric(M),
    curve = rep(colnames(M), each = length(x)),
    stringsAsFactors = FALSE
  )
}

#' Plot one or more curves, optionally against true reference curves.
#'
#' @param locations Evaluation or plotting locations.
#' @param f Fitted or observed function values.
#' @param true Optional reference function values.
#' @param limits Optional plotting limits.
#' @param LEGEND Whether to show a legend.
#' @param colors Line color or color vector.
#' @return The value produced by `plot.curve`.
plot.curve <- function(locations, f, true = NULL, limits = NULL, LEGEND = FALSE, colors = "black") {

  ## Handle null input
  if (is.null(f)) {
    return(ggplot() + theme_void())
  }

  # --- Main plot ---
  M <- as_curve_matrix(locations, f, prefix = "curve")
  data_long <- to_long(locations, M)

  plot <- ggplot(data_long, aes(x = x, y = y, group = curve)) +
    geom_line(color = colors)

  # --- True curves (green + thicker + dotted) ---
  if (!is.null(true)) {
    Tm <- as_curve_matrix(locations, true, prefix = "true")
    data_true <- to_long(locations, Tm)

    if (ncol(M) > 1) {
      # multiple curves -> make true curve stand out
      plot <- plot + geom_line(
        data = data_true,
        aes(x = x, y = y),
        linetype = "dashed",
        color = "darkgreen",
        linewidth = 0.8
      )
    } else {
      # single curve -> normal black dotted
      plot <- plot + geom_line(
        data = data_true,
        aes(x = x, y = y),
        linetype = "dotted",
        color = "darkgreen",
        linewidth = 0.8
      )
    }
  }

  # --- Y limits ---
  if (!is.null(limits)) {
    plot <- plot + ylim(limits[1], limits[2])
  }

  # --- Legend control ---
  if (!LEGEND) {
    plot <- plot + theme(legend.position = "none")
  }

  return(plot)
}

#' Plot curve samples as connected points with an optional true curve.
#'
#' @param locations Evaluation or plotting locations.
#' @param f Fitted or observed function values.
#' @param true Optional reference function values.
#' @param size Point size.
#' @param limits Optional plotting limits.
#' @return The value produced by `plot.curve_points`.
plot.curve_points <- function(locations, f, true = NULL, size = 1, limits = NULL) {

  ## Handle null input
  if (is.null(f)) {
    return(ggplot() + theme_void())
  }

  # --- main curves ---
  M <- as_curve_matrix(locations, f, prefix = "curve")
  data_long <- to_long(locations, M)

  plot <- ggplot(data_long, aes(x = x, y = y, group = curve)) +
    geom_point(size = size, color = "black") +
    geom_line(color = "black", linewidth = 0.5)  # connect dots with segments

  # --- true curve ---
  if (!is.null(true)) {
    Tm <- as_curve_matrix(locations, true, prefix = "true")
    data_true <- to_long(locations, Tm)

    if (ncol(M) > 1) {
      # multiple curves -> green dashed
      plot <- plot + geom_line(
        data = data_true,
        aes(x = x, y = y),
        color = "darkgreen",
        linetype = "dashed",
        linewidth = 1
      )
    } else {
      # single curve -> black dotted
      plot <- plot + geom_line(
        data = data_true,
        aes(x = x, y = y),
        color = "darkgreen",
        linetype = "dotted",
        linewidth = 0.8
      )
    }
  }

  # --- y limits ---
  if (!is.null(limits)) {
    plot <- plot + ylim(limits[1], limits[2])
  }

  # --- remove legend ---
  plot <- plot + theme(legend.position = "none")

  return(plot)
}


#' Plot bootstrap curves with optional fit, selected curve, truth, and bands.
#'
#' @param locations Evaluation locations.
#' @param f Bootstrap curve values as a vector, matrix, or list of vectors.
#' @param true Optional reference curve values.
#' @param fit Optional fitted curve values.
#' @param w_min Optional selected or minimum-width bootstrap curve values.
#' @param conf_int Optional two-column confidence interval matrix or data frame.
#' @param limits Optional y-axis limits as `c(min, max)`.
#' @param LEGEND Whether to show a legend.
#' @param colors_boot Color used for bootstrap curves.
#' @return A ggplot object.
plot.curve_bootstrap <- function(
    locations, f,
    true = NULL,
    fit = NULL,
    w_min = NULL,
    conf_int = NULL,
    limits = NULL,
    LEGEND = FALSE,
    colors_boot = "grey70") {
  if (is.null(f)) {
    return(ggplot() +
      theme_void())
  }

  ## Draw bootstrap curves behind all reference curves.
  M <- as_curve_matrix(locations, f, prefix = "boot")
  data_long <- to_long(locations, M)

  plot <- ggplot() +
    geom_line(
      data = data_long,
      aes(x = x, y = y, group = curve),
      color = colors_boot,
      linewidth = 0.25,
      alpha = 0.75
    )

  ## Add an optional confidence band.
  if (!is.null(conf_int)) {
    CI <- as.data.frame(conf_int)
    colnames(CI) <- c("lower", "upper")
    CI$x <- locations

    plot <- plot +
      geom_ribbon(
        data = CI,
        aes(x = x, ymin = lower, ymax = upper),
        alpha = 0.18,
        fill = "blue",
        inherit.aes = FALSE
      )
  }

  ## Add the fitted curve.
  if (!is.null(fit)) {
    Fm <- as_curve_matrix(locations, fit, prefix = "fit")
    data_fit <- to_long(locations, Fm)

    plot <- plot +
      geom_line(
        data = data_fit,
        aes(x = x, y = y),
        color = "black",
        linewidth = 1.0
      )
  }

  ## Add the selected bootstrap curve.
  if (!is.null(w_min)) {
    Wm <- as_curve_matrix(locations, w_min, prefix = "w_min")
    data_wmin <- to_long(locations, Wm)

    plot <- plot +
      geom_line(
        data = data_wmin,
        aes(x = x, y = y),
        color = "red",
        linewidth = 0.9
      )
  }

  ## Add the true reference curve.
  if (!is.null(true)) {
    Tm <- as_curve_matrix(locations, true, prefix = "true")
    data_true <- to_long(locations, Tm)

    plot <- plot +
      geom_line(
        data = data_true,
        aes(x = x, y = y),
        linetype = "dashed",
        color = "darkgreen",
        linewidth = 0.9
      )
  }

  if (!is.null(limits)) {
    plot <- plot + ylim(limits[1], limits[2])
  }

  if (!LEGEND) {
    plot <- plot + theme(legend.position = "none")
  }

  plot
}
#' Plot scalar field values at scattered two-dimensional locations.
#'
#' @param locations Evaluation or plotting locations.
#' @param f Fitted or observed function values.
#' @param boundary Optional domain boundary object.
#' @param size Point size.
#' @param limits Optional plotting limits.
#' @param colormap Viridis color-map option.
#' @param discrete Whether to use a discrete color scale.
#' @param LEGEND Whether to show a legend.
#' @return The value produced by `plot.field_points`.
plot.field_points <- function(locations, f, boundary = NULL,
                              size = 1, limits = NULL, colormap = "D",
                              discrete = FALSE, LEGEND = FALSE) {

  ## Handle null input
  if (is.null(f)) {
    return(ggplot() + theme_void())
  }

  ## Assemble data
  data <- data.frame(locations, value = f)
  colnames(data) <- c("x", "y", "value")

  ## Build base plot
  plot <- ggplot() +
    geom_point(data = data, aes(x = x, y = y, color = value), size = size) +
    coord_fixed()

  ## Apply color scale
  if (!discrete) {
    if (is.null(limits)) {
      plot <- plot + scale_color_viridis(option = colormap)
    } else {
      plot <- plot + scale_color_viridis(option = colormap, limits = limits)
    }
  } else {
    plot <- plot + scale_color_viridis_d(option = colormap)
  }

  ## Add boundary if provided
  if (!is.null(boundary)) {
    plot <- plot +
      geom_polygon(data = fortify(boundary), aes(x = long, y = lat),
                   fill = "transparent", color = "black", linewidth = 1)
  }

  ## Add or remove legend
  if (!LEGEND) {
    plot <- plot + guides(color = "none")
  }

  return(plot)
}
#' Plot scalar field values on a regular two-dimensional grid.
#'
#' @param nodes Node coordinate matrix.
#' @param f Fitted or observed function values.
#' @param boundary Optional domain boundary object.
#' @param limits Optional plotting limits.
#' @param breaks Optional contour breaks.
#' @param colormap Viridis color-map option.
#' @param discrete Whether to use a discrete color scale.
#' @param ISOLINES Whether to draw contour isolines.
#' @param LEGEND Whether to show a legend.
#' @return The value produced by `plot.field_tile`.
plot.field_tile <- function(nodes, f, boundary = NULL,
                            limits = NULL, breaks = NULL, colormap = "D",
                            discrete = FALSE, ISOLINES = FALSE, LEGEND = FALSE) {

  ## Handle null input
  if (is.null(f)) {
    return(ggplot() + theme_void())
  }

  ## Assemble data
  data <- data.frame(nodes, value = f)
  colnames(data) <- c("x", "y", "value")
  data <- na.omit(data)

  ## Build base plot
  plot <- ggplot() +
    geom_tile(data = data, aes(x = x, y = y, fill = value)) +
    coord_fixed()

  ## Add contour isolines if requested
  if (!is.null(breaks) || ISOLINES) {
    color <- "black"
    limits_real <- range(data$value)
    if (is.null(breaks)) {
      breaks <- seq(limits_real[1], limits_real[2], length = 10)
    }
    breaks_initial <- breaks
    h <- breaks[2] - breaks[1]
    if (limits_real[1] < min(breaks)) {
      breaks <- c(sort(seq(min(breaks), limits_real[1] - h, by = -h)[-1]), breaks)
    }
    if (limits_real[2] > max(breaks)) {
      breaks <- c(breaks, seq(max(breaks), limits_real[2] + h, by = h)[-1])
    }
    if (length(breaks) > 2 * length(breaks_initial)) {
      breaks <- breaks_initial
      color <- "red"
    }
    plot <- plot +
      geom_contour(data = data, aes(x = x, y = y, z = value),
                   color = color, breaks = breaks)
  }

  ## Apply color scale
  if (!discrete) {
    if (!is.null(breaks)) {
      h <- breaks[2] - breaks[1]
      limits <- limits + c(-h, h)
    }
    if (is.null(limits)) {
      plot <- plot + scale_fill_viridis(option = colormap)
    } else {
      plot <- plot + scale_fill_viridis(option = colormap, limits = limits)
    }
  } else {
    plot <- plot + scale_fill_viridis_d(option = colormap)
  }

  ## Add boundary if provided
  if (!is.null(boundary)) {
    plot <- plot +
      geom_polygon(data = fortify(boundary), aes(x = long, y = lat),
                   fill = "transparent", color = "black", linewidth = 1)
  }

  ## Add or remove legend
  if (!LEGEND) {
    plot <- plot + guides(fill = "none")
  }

  return(plot)
}
#' Create grouped boxplots from a wide result table.
#'
#' @param data Generated data and truth object.
#' @param group_name Label for groups.
#' @param group_labels Labels used for groups.
#' @param subgroup_name Legend title for subgroups.
#' @param subgroup_labels Labels used for subgroups.
#' @param subgroup_colors Colors used for subgroups.
#' @param values_name Label for plotted values.
#' @param limits Optional plotting limits.
#' @param DIVIDERS Whether to draw group dividers.
#' @param LEGEND Whether to show a legend.
#' @param LOGY Whether to use a log10 y-axis.
#' @return The value produced by `plot.grouped_boxplots`.
plot.grouped_boxplots <- function(data,
                                  group_name = "Components", group_labels = NULL,
                                  subgroup_name = "Models", subgroup_labels = NULL, subgroup_colors = NULL,
                                  values_name = "Score", limits = NULL,
                                  DIVIDERS = TRUE, LEGEND = TRUE, LOGY = FALSE) {

  ## Data integrity check
  if (!("Group" %in% names(data))) stop("The dataframe must contain a column named 'Group'")

  ## Reshape data
  data <- data %>%
    pivot_longer(cols = -Group, names_to = "SubGroup", values_to = "Score")

  ## Extract labels
  groups_levels <- sort(unique(data$Group))
  subgroup_levels <- unique(data$SubGroup)
  if (is.null(group_labels)) group_labels <- groups_levels
  if (is.null(subgroup_labels)) subgroup_labels <- subgroup_levels

  ## Assign colors
  if (is.null(subgroup_colors)) subgroup_colors <- rainbow(length(subgroup_labels))
  names(subgroup_colors) <- subgroup_labels

  ## Refactor categorical variables
  data <- data %>%
    mutate(Group = factor(Group, levels = groups_levels, labels = group_labels)) %>%
    mutate(SubGroup = factor(SubGroup, levels = subgroup_levels, labels = subgroup_labels))

  ## Build plot
  plot <- ggplot(data, aes(x = Group, y = Score, fill = SubGroup, color = SubGroup)) +
    geom_boxplot(na.rm = TRUE) +
    labs(x = group_name, y = values_name) +
    scale_fill_manual(name = subgroup_name, values = subgroup_colors) +
    scale_color_manual(name = subgroup_name, values = subgroup_colors)

  ## Add y-axis scaling
  if (isTRUE(LOGY)) {
    plot <- plot + scale_y_log10(limits = limits)
  } else if (!is.null(limits)) {
    plot <- plot + scale_y_continuous(limits = limits)
  }

  ## Add dividers
  if (DIVIDERS && length(group_labels) > 1) {
    plot <- plot +
      geom_vline(xintercept = seq(1.5, length(unique(group_labels)) - 0.5, 1),
                 lwd = 0.2, colour = "grey")
  }

  ## Add or remove legend
  if (!LEGEND) {
    plot <- plot + guides(fill = "none", color = "none")
  }

  return(plot)
}
#' Create multi-model line plots from a wide result table.
#'
#' @param data Generated data and truth object.
#' @param x_name X-axis label.
#' @param x_breaks Optional x-axis breaks.
#' @param subgroup_name Legend title for subgroups.
#' @param subgroup_labels Labels used for subgroups.
#' @param subgroup_colors Colors used for subgroups.
#' @param values_name Label for plotted values.
#' @param limits Optional plotting limits.
#' @param LOGX Whether to use a log-scaled x axis.
#' @param LOGY Whether to use a log-scaled y axis.
#' @param LOGLOG Whether to use log scales on both axes.
#' @param NORMALIZED Whether to normalize lines before plotting.
#' @param LEGEND Whether to show a legend.
#' @return The value produced by `plot.multiple_lines`.
plot.multiple_lines <- function(data,
                                x_name = "Components",
                                x_breaks = NULL,
                                subgroup_name = "Models", subgroup_labels = NULL, subgroup_colors = NULL,
                                values_name = "Score", limits = NULL,
                                LOGX = FALSE, LOGY = FALSE, LOGLOG = FALSE,
                                NORMALIZED = FALSE, LEGEND = TRUE) {

  ## Rename first column
  columns_names <- colnames(data)
  columns_names[1] <- "x"
  colnames(data) <- columns_names

  ## Normalize if requested
  if (NORMALIZED) {
    for (name in columns_names[-1])
      data[, name] <- data[, name] / min(data[, name])
  }

  ## Log-log consistency
  if (LOGLOG) {
    LOGX <- TRUE
    LOGY <- TRUE
  }

  ## Auto-generate breaks if requested
  if (is.logical(x_breaks) && x_breaks) x_breaks <- unique(data$x)

  ## Reshape data
  data <- data %>%
    pivot_longer(cols = -x, names_to = "SubGroup", values_to = "Score")

  ## Extract labels
  subgroup_levels <- unique(data$SubGroup)
  if (is.null(subgroup_labels)) subgroup_labels <- subgroup_levels

  ## Assign colors
  if (is.null(subgroup_colors)) subgroup_colors <- rainbow(length(subgroup_labels))
  names(subgroup_colors) <- subgroup_labels

  ## Refactor categories
  data <- data %>%
    mutate(SubGroup = factor(SubGroup, levels = subgroup_levels, labels = subgroup_labels))

  ## Build plot
  plot <- ggplot(data, aes(x = x, y = Score, color = SubGroup)) +
    geom_line(linewidth = 1) +
    labs(x = x_name, y = values_name) +
    scale_color_manual(name = subgroup_name, values = subgroup_colors)

  ## Logarithmic reference lines for normalized log-log plots
  if (LOGLOG && NORMALIZED) {
    x <- seq(min(data$x), max(data$x), length = 10)
    plot <- plot +
      geom_line(data = data.frame(x = x, y = x / x[1]), aes(x = x, y = y),
                linetype = "dashed", color = "grey", linewidth = 0.3) +
      geom_line(data = data.frame(x = x, y = (x / x[1])^2), aes(x = x, y = y),
                linetype = "dashed", color = "grey", linewidth = 0.3) +
      geom_line(data = data.frame(x = x, y = (x / x[1])^3), aes(x = x, y = y),
                linetype = "dashed", color = "grey", linewidth = 0.3)
  }

  ## Axis scaling
  if (LOGX) {
    plot <- plot + scale_x_log10(breaks = x_breaks)
  } else {
    plot <- plot + scale_x_continuous(breaks = x_breaks)
  }
  if (LOGY) {
    plot <- plot + scale_y_log10(limits = limits)
  } else if (!is.null(limits)) {
    plot <- plot + scale_y_continuous(limits = limits)
  }

  ## Add or remove legend
  if (!LEGEND) {
    plot <- plot + guides(color = "none")
  }

  return(plot)
}
#' Add row labels, column labels, and a title to an arranged plot grid.
#'
#' @param plot Plot or grid object.
#' @param title Optional plot title.
#' @param labels_cols Optional column labels.
#' @param labels_rows Optional row labels.
#' @param height Panel height multiplier.
#' @param width Panel width multiplier.
#' @return The value produced by `labeled_plots_grid`.
labeled_plots_grid <- function(plot, title = NULL, labels_cols = NULL,
                              labels_rows = NULL, height = 4, width = 4) {

  ## Compute grid dimensions
  n_row <- max(plot$layout$t)
  n_col <- max(plot$layout$l)

  ## Add column labels
  if (!is.null(labels_cols)) {
    labels_grobs_cols <- lapply(labels_cols, function(lab)
      textGrob(lab, gp = gpar(fontsize = 12, fontface = "bold")))
    labels_grobs_cols <- arrangeGrob(grobs = labels_grobs_cols, nrow = 1)
  }

  ## Add row labels
  add <- 0
  if (!is.null(labels_rows)) {
    labels_grobs_rows <- list()
    if (!is.null(labels_cols)) {
      add <- 1
      labels_grobs_rows[[1]] <- textGrob(" ", gp = gpar(fontsize = 12, fontface = "bold"))
    }
    for (row in 1:length(labels_rows) + add) {
      label_row <- labels_rows[[row - add]]
      labels_grobs_rows[[row]] <- textGrob(label_row, gp = gpar(fontsize = 12, fontface = "bold"), rot = 90)
    }
    labels_grobs_rows <- arrangeGrob(grobs = labels_grobs_rows, ncol = 1, heights = c(1, rep(height, n_row)))
  }

  ## Add title
  if (!is.null(title)) {
    title_grob <- textGrob(title, gp = gpar(fontsize = 14, fontface = "bold"))
  }

  ## Combine all components
  if (!is.null(labels_cols)) plot <- arrangeGrob(labels_grobs_cols, plot, heights = c(1, height * n_row))
  if (!is.null(labels_rows)) plot <- arrangeGrob(labels_grobs_rows, plot, widths = c(1, width * n_col))
  if (!is.null(title)) plot <- arrangeGrob(title_grob, plot, heights = c(1, add + height * n_row))

  return(plot)
}
#' Create grouped violin plots from a wide result table.
#'
#' @param data Generated data and truth object.
#' @param group_name Label for groups.
#' @param group_labels Labels used for groups.
#' @param subgroup_name Legend title for subgroups.
#' @param subgroup_labels Labels used for subgroups.
#' @param subgroup_colors Colors used for subgroups.
#' @param values_name Label for plotted values.
#' @param limits Optional plotting limits.
#' @param DIVIDERS Whether to draw group dividers.
#' @param LEGEND Whether to show a legend.
#' @param show_boxplot Whether to overlay boxplots.
#' @return The value produced by `plot.grouped_violins`.
plot.grouped_violins <- function(data,
                                 group_name = "Components", group_labels = NULL,
                                 subgroup_name = "Models", subgroup_labels = NULL, subgroup_colors = NULL,
                                 values_name = "Score", limits = NULL,
                                 DIVIDERS = TRUE, LEGEND = TRUE, show_boxplot = TRUE) {

  ## Data integrity check
  if (!("Group" %in% names(data))) stop("The dataframe must contain a column named 'Group'")

  ## Reshape data
  data <- data %>%
    pivot_longer(cols = -Group, names_to = "SubGroup", values_to = "Score")

  ## Extract labels
  groups_levels <- unique(data$Group)
  subgroup_levels <- unique(data$SubGroup)
  if (is.null(group_labels)) group_labels <- groups_levels
  if (is.null(subgroup_labels)) subgroup_labels <- subgroup_levels

  ## Assign colors
  if (is.null(subgroup_colors)) subgroup_colors <- rainbow(length(subgroup_labels))
  names(subgroup_colors) <- subgroup_labels

  ## Refactor variables
  data <- data %>%
    mutate(Group = factor(Group, levels = groups_levels, labels = group_labels)) %>%
    mutate(SubGroup = factor(SubGroup, levels = subgroup_levels, labels = subgroup_labels))

  ## Build violin plot
  plot <- ggplot(data, aes(x = Group, y = Score, fill = SubGroup)) +
    geom_violin(trim = FALSE, width = 1.5, position = position_dodge(width = 0.8),
                na.rm = TRUE, alpha = 0.8) +
    labs(x = group_name, y = values_name) +
    scale_fill_manual(name = subgroup_name, values = subgroup_colors)

  ## Optionally overlay boxplots
  if (show_boxplot) {
    plot <- plot +
      geom_boxplot(width = 0.15, position = position_dodge(width = 0.8),
                   outlier.shape = NA, alpha = 0.6)
  }

  ## Apply y-limits
  if (!is.null(limits)) {
    plot <- plot + scale_y_continuous(
      limits = limits
    )
  }

  ## Add groups divider if required
  if (DIVIDERS) {
    if (length(group_labels) > 1) {
      plot <- plot +
        geom_vline(
          xintercept = seq(1.5, length(unique(group_labels)) - 0.5, 1),
          lwd = 0.2, colour = "grey"
        )
    }
  }

  ## Legend control
  if (!LEGEND) {
    plot <- plot + guides(fill = "none")
  }

  return(plot)
}
#' Create aggregate plots over one or more varying test options.
#'
#' @param loaded_results Loaded quantitative or qualitative result structure.
#' @param data_plot_orig Result data frame before filtering or aggregation.
#' @param title_prefix Prefix used for plot titles.
#' @param values_name Label for plotted values.
#' @param order Order used for varying options.
#' @param limits Optional plotting limits.
#' @param plots_catalog List of booleans selecting which plots to draw.
#' @return The value produced by `plot.aggregated_data`.
plot.aggregated_data <- function(loaded_results, data_plot_orig, title_prefix, values_name, order = NULL, limits = NULL, plots_catalog = NULL) {

  ## Get model-related properties
  model_names  <- loaded_results$model_names
  model_labels <- loaded_results$model_labels
  model_colors <- loaded_results$model_colors

  ## Get varying options and apply ordering
  varying_options <- loaded_results$varying_options
  if (is.null(order)) order <- seq_along(varying_options)
  varying_options <- varying_options[order]
  name_varying_options <- varying_options

  ## Build options grid (unique sorted values for each varying option)
  options_grid <- list()
  for (name_ao in varying_options) {
    options_grid[[name_ao]] <- unique(data_plot_orig[, name_ao])
    options_grid[[name_ao]] <- sort(options_grid[[name_ao]])
  }

  ## Plots catalog defaults
  if (is.null(plots_catalog)) {
    plots_catalog <- list(
      boxplots = TRUE,
      lines = FALSE,
      logx = FALSE,
      loglog = FALSE,
      normalized = FALSE
    )
  }

  ## Detect groups (values of the first varying option will become x-axis)
  groups <- sort(unique(data_plot_orig$Group))

  ## If there is more than one group, plot them sequentially
  for (group in groups) {  # group <- groups[1]

    ## Select the specific group
    data_plot <- data_plot_orig[data_plot_orig$Group == group, ]

    ## Generate title
    title <- paste(
      title_prefix,
      name_varying_options[1],
      ifelse(length(groups) > 1, paste0("- ", group), "")
    )

    ## Rooms for plots
    boxplot_list <- list()
    plot_list <- list()
    plot_logx_list <- list()
    plot_logy_list <- list()
    plot_loglog_list <- list()
    plot_loglog_normalized_list <- list()

    name_aggregation_option <- varying_options[1]
    group_name <- name_varying_options[1]

    options_grid_selected <- options_grid
    options_grid_selected[[name_aggregation_option]] <- NULL
    names_options_selected  <- names(options_grid_selected)
    labels_options_selected <- name_varying_options[-1]

    ## Handle 1D case (only one varying option)
    if (length(options_grid_selected) == 0) {
      combinations_options <- data.frame(dummy = 1)
      labels_rows <- ""
      labels_cols <- ""
    } else {
      mg <- do.call(expand.grid, options_grid_selected)
      combinations_options <- do.call(data.frame, lapply(mg, as.vector))
      colnames(combinations_options) <- names_options_selected

      labels_rows <- if (length(labels_options_selected) >= 1) paste(labels_options_selected[1], "=", options_grid_selected[[1]]) else ""
      labels_cols <- if (length(labels_options_selected) >= 2) paste(labels_options_selected[2], "=", options_grid_selected[[2]]) else ""
    }

    for (j in 1:nrow(combinations_options)) {  # j <- 1

      ## Data preparation
      if (length(names_options_selected) == 0) {
        ## 1D case: use all data
        data_plot_trimmed <- data_plot[, c(name_aggregation_option, model_names)]
      } else {
        ## ND case: filter by combination
        condition <- rep(TRUE, nrow(data_plot))
        for (k in seq_along(names_options_selected)) {
          condition <- condition & (data_plot[[names_options_selected[k]]] == combinations_options[j, k])
        }
        data_plot_trimmed <- data_plot[condition, c(name_aggregation_option, model_names)]
      }
      colnames(data_plot_trimmed)[1] <- "Group"

      ## Skip if data is empty
      if (nrow(data_plot_trimmed) == 0) next

      ## Remove models without numeric results in this panel.
      has_finite_result <- function(x) {
        any(is.finite(suppressWarnings(as.numeric(x))))
      }
      valid_models <- model_names[apply(
        data_plot_trimmed[, model_names, drop = FALSE],
        2,
        has_finite_result
      )]
      if (length(valid_models) == 0) next

      ## Aggregate repeated batches before drawing line-based summaries.
      valid_rows <- !is.na(data_plot_trimmed$Group) & apply(
        data_plot_trimmed[, valid_models, drop = FALSE],
        1,
        has_finite_result
      )
      if (!any(valid_rows)) next

      data_plot_trimmed <- data_plot_trimmed[valid_rows, c("Group", valid_models), drop = FALSE]
      data_plot_aggregated <- data.frame(
        Group = sort(unique(data_plot_trimmed$Group))
      )
      for (name_model in valid_models) {
        medians <- tapply(
          data_plot_trimmed[[name_model]],
          data_plot_trimmed$Group,
          median,
          na.rm = TRUE
        )
        data_plot_aggregated[[name_model]] <- as.numeric(
          medians[as.character(data_plot_aggregated$Group)]
        )
      }

      ## Boxplots
      if (isTRUE(plots_catalog$boxplots)) {
        boxplot_list[[j]] <- plot.grouped_boxplots(
          data_plot_trimmed[, c("Group", valid_models)],
          values_name = values_name,
          group_name = group_name,
          subgroup_name = "Approaches",
          subgroup_labels = model_labels[match(valid_models, model_names)],
          subgroup_colors = model_colors[match(valid_models, model_names)],
          limits = limits,
          LEGEND = FALSE,
          LOGY = isTRUE(plots_catalog$boxplot_logy)
        ) + std_plot_settings()
      }

      ## Lines (linear x)
      if (isTRUE(plots_catalog$lines)) {
        plot_list[[j]] <- plot.multiple_lines(
          data_plot_aggregated[, c("Group", valid_models)],
          values_name = values_name,
          x_name = group_name,
          x_breaks = TRUE,
          subgroup_name = "Approaches",
          subgroup_labels = model_labels[match(valid_models, model_names)],
          subgroup_colors = model_colors[match(valid_models, model_names)],
          LEGEND = FALSE,
          limits  = limits,
          NORMALIZED = FALSE,
          LOGX = FALSE
        ) + std_plot_settings()
      }

      ## Lines (log-x)
      if (isTRUE(plots_catalog$logx)) {
        plot_logx_list[[j]] <- plot.multiple_lines(
          data_plot_aggregated[, c("Group", valid_models)],
          values_name = values_name,
          x_name = group_name,
          x_breaks = TRUE,
          subgroup_name = "Approaches",
          subgroup_labels = model_labels[match(valid_models, model_names)],
          subgroup_colors = model_colors[match(valid_models, model_names)],
          LEGEND = FALSE,
          limits = limits,
          NORMALIZED = FALSE,
          LOGX = TRUE
        ) + std_plot_settings()
      }

      ## Lines (log-y)
      if (isTRUE(plots_catalog$logy)) {
        plot_logy_list[[j]] <- plot.multiple_lines(
          data_plot_aggregated[, c("Group", valid_models)],
          values_name = values_name,
          x_name = group_name,
          x_breaks = TRUE,
          subgroup_name = "Approaches",
          subgroup_labels = model_labels[match(valid_models, model_names)],
          subgroup_colors = model_colors[match(valid_models, model_names)],
          LEGEND = FALSE,
          limits = limits,
          NORMALIZED = FALSE,
          LOGY = TRUE
        ) + std_plot_settings()
      }

      ## Lines (log-log)
      if (isTRUE(plots_catalog$loglog)) {
        plot_loglog_list[[j]] <- plot.multiple_lines(
          data_plot_aggregated[, c("Group", valid_models)],
          values_name = values_name,
          x_name = group_name,
          x_breaks = TRUE,
          subgroup_name = "Approaches",
          subgroup_labels = model_labels[match(valid_models, model_names)],
          subgroup_colors = model_colors[match(valid_models, model_names)],
          LEGEND = FALSE,
          limits = NULL,
          NORMALIZED = FALSE,
          LOGLOG = TRUE
        ) + std_plot_settings()
      }

      ## Lines (log-log, normalized)
      if (isTRUE(plots_catalog$normalized)) {
        plot_loglog_normalized_list[[j]] <- plot.multiple_lines(
          data_plot_aggregated[, c("Group", valid_models)],
          values_name = values_name,
          x_name = group_name,
          x_breaks = TRUE,
          subgroup_name = "Approaches",
          subgroup_labels = model_labels[match(valid_models, model_names)],
          subgroup_colors = model_colors[match(valid_models, model_names)],
          LEGEND = FALSE,
          limits = NULL,
          NORMALIZED = TRUE,
          LOGLOG = TRUE
        ) + std_plot_settings()
      }
    }

    ## Handle layout safely if 1D: use 1 column
    ncols <- if (length(labels_cols) == 0) 1 else length(labels_cols)

    if (isTRUE(plots_catalog$boxplots) && length(boxplot_list) > 0) {
      boxplot <- arrangeGrob(grobs = boxplot_list, ncol = ncols, as.table = FALSE)
      boxplot <- labeled_plots_grid(boxplot, title, labels_cols, labels_rows, 9, 7)
      grid.arrange(boxplot)
    }

    if (isTRUE(plots_catalog$lines) && length(plot_list) > 0) {
      plot <- arrangeGrob(grobs = plot_list, ncol = ncols, as.table = FALSE)
      plot <- labeled_plots_grid(plot, title, labels_cols, labels_rows, 9, 7)
      grid.arrange(plot)
    }

    if (isTRUE(plots_catalog$logx) && length(plot_logx_list) > 0) {
      plot_logx <- arrangeGrob(grobs = plot_logx_list, ncol = ncols, as.table = FALSE)
      plot_logx <- labeled_plots_grid(plot_logx, title, labels_cols, labels_rows, 9, 7)
      grid.arrange(plot_logx)
    }

    if (isTRUE(plots_catalog$logy) && length(plot_logy_list) > 0) {
      plot_logy <- arrangeGrob(grobs = plot_logy_list, ncol = ncols, as.table = FALSE)
      plot_logy <- labeled_plots_grid(plot_logy, title, labels_cols, labels_rows, 9, 7)
      grid.arrange(plot_logy)
    }

    if (isTRUE(plots_catalog$loglog) && length(plot_loglog_list) > 0) {
      plot_loglog <- arrangeGrob(grobs = plot_loglog_list, ncol = ncols, as.table = FALSE)
      plot_loglog <- labeled_plots_grid(plot_loglog, title, labels_cols, labels_rows, 9, 7)
      grid.arrange(plot_loglog)
    }

    if (isTRUE(plots_catalog$normalized) && length(plot_loglog_normalized_list) > 0) {
      plot_loglog_normalized <- arrangeGrob(grobs = plot_loglog_normalized_list, ncol = ncols, as.table = FALSE)
      plot_loglog_normalized <- labeled_plots_grid(plot_loglog_normalized, title, labels_cols, labels_rows, 9, 7)
      grid.arrange(plot_loglog_normalized)
    }
  }
}
