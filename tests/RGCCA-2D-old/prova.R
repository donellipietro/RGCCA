#!/usr/bin/env Rscript

rm(list = ls())

library(Matrix)
library(ggplot2)
library(gridExtra)
library(sp)
library(femR)

source("src/utils/plotting_utils.R")
source("src/utils/mesh_utils.R")
source("src/utils/error_metrics.R")
source("src/utils/domain_utils.R")
source("src/utils/svg_utils.R")


## ---------------------------------------------------------------------
## 3. SOURCE THE NEW generate_data() (FEM-based version)
##    Put the updated function in src/data-generation/generate_data_regions.R
##    It must:
##      - use load_region_object() and eval_region_loadings()
##      - read test_options$domain_and_locations$name_mesh (vector)
## ---------------------------------------------------------------------

source("tests/RGCCA-2D/generate_data.R")
# after this, generate_data() is available

## ---------------------------------------------------------------------
## 4. BUILD test_options
## ---------------------------------------------------------------------

T_sec <- 200
TR    <- 2

test_options <- list(
  domain_and_locations = list(
    T_sec    = T_sec,
    # one region per group, 4 groups
    name_mesh = paste0("region_", 1:4)
  ),
  data = list(
    TR    = TR
  ),
  dimensions = list(
    n_groups          = 4L,
    n_nodes_D         = 0L,    # not used (we work on regions)
    n_nodes_T         = 401L,  # grid for solve in time domain
    n_locs_D = c(6*1e2, 4*1e2, 4*1e2, 6*1e2),  # target spatial locations per region
    n_locs_T          = NA,    # will be overwritten inside
    n_nodes_HR_grid_D = c(4*1e4, 2*1e4, 2*1e4, 4*1e4),    # not used
    n_nodes_HR_grid_T = 501L   # HR time grid
  ),
  model_options = list(
    n_comp = 3L,
    init = "svd",
    tau = "optimal",
    scheme = "factorial"
  ),
  noise = list(
    sigma_noise = 0.8
  )
)


## ---------------------------------------------------------------------
## 5. RUN DATA GENERATION
## ---------------------------------------------------------------------

set.seed(123)
data <- generate_data(test_options, seed = 123)

str(data$X, max.level = 1)
cat("Group 1 matrix dim: ", dim(data$X[[1]]), "\n")  # n_time × n_space_g1

## ---------------------------------------------------------------------
## 6. QUICK VISUAL CHECKS
## ---------------------------------------------------------------------

# time series of first point in group 1
id = 3
plot(data$locations_T, data$X[[id]][, 1], type = "l",
     xlab = "time (s)", ylab = "signal",
     main = "Group 1, voxel 1")

# spatial map of component 1 loadings for group 1
df_Ai <- data.frame(
  x = data$locations_D[[id]][, 1],
  y = data$locations_D[[id]][, 2],
  v = data$A_locs[[id]][, 1]
)

ggplot(df_Ai, aes(x = x, y = y, colour = v)) +
  geom_point(size = 1) +
  coord_equal() +
  scale_colour_viridis_c() +
  ggtitle("Group 1 – spatial loading, component 1")


plot_all_regions_at_time <- function(data, locs_name = "locations_D", field_name = "X", t_index = NULL, comp_id = NULL, x_lim, y_lim, size, V_LIM) {
  
  df_list <- list()
  v_lim <- c(0, 0)
  for (i in 1:data$dimensions$n_groups) {
    if(!is.null(t_index)) {
      value <- data[[field_name]][[i]][t_index, ]
    } else if(!is.null(comp_id)){
      value <- data[[field_name]][[i]][, comp_id]
    } else {
      stop()
    }
    df_list[[i]] <- data.frame(
      x      = data[[locs_name]][[i]][, 1],
      y      = data[[locs_name]][[i]][, 2],
      value  = value,
      region = factor(i)
    )
    v_lim <- range(c(v_lim, range(data[[field_name]][[i]])))
  }
  df <- do.call(rbind, df_list)
  
  ## ---- 2. Polygon boundaries with UNIQUE groups per region ----
  poly_df_list <- lapply(1:4, function(r) {
    pg  <- data$domain_D[[r]]$boundary
    tmp <- fortify(pg)
    tmp$region_id <- factor(r)
    # make group unique per region to avoid connecting polygons
    tmp$group_id  <- interaction(tmp$region_id, tmp$group, drop = TRUE)
    tmp
  })
  poly_df <- do.call(rbind, poly_df_list)
  
  ggplot() +
    # loadings field
    geom_point(
      data = df,
      aes(x = x, y = y, colour = value),
      size = size
    )  +
    # region boundaries
    geom_path(
      data = poly_df,
      aes(x = long, y = lat, group = group_id),
      colour = "black",
      linewidth = 1
    ) +
    scale_colour_viridis_c(limits = v_lim) +
    coord_equal(xlim = x_lim, ylim = y_lim, expand = FALSE) +
    theme(
      legend.position      = "right",
      legend.text.position = "right"
    ) + guides(color = "none") +
    ggtitle(paste("All Regions – Time =", round(data$locations_T[t_index], 2))) +
    std_plot_settings_fields()
}

id <- 4
plot(data$domain_D[[id]]$boundary)
points(data$domain_D[[id]]$nodes)
points(data$locations_D[[id]], col = "red")
# points(read.csv("prova/locs_D_3.csv")[,2:3], col = "green")
points(data$domain_D[[id]]$nodes[data$domain_D[[id]]$nodesmarkers == 1, ], col = "blue")
points(data$domain_D[[id]]$nodes[1:2, 1], data$domain_D[[id]]$nodes[1:2, 2] , col = "green")
prova <- data$domain_D[[id]]$triangles

xlim_all <- c(0, 55)
ylim_all <- c(-30, 0)


plot_all_regions_at_time(data, locs_name = "locations_D", field_name = "A_locs", comp_id = 1, x_lim = xlim_all, y_lim = ylim_all, size = 4)
plot_all_regions_at_time(data, locs_name = "locations_D", field_name = "A_locs", comp_id = 2, x_lim = xlim_all, y_lim = ylim_all, size = 4)  
plot_all_regions_at_time(data, locs_name = "locations_D", field_name = "A_locs", comp_id = 3, x_lim = xlim_all, y_lim = ylim_all, size = 4)
plot_all_regions_at_time(data, locs_name = "grid_D", field_name = "A_grid", comp_id = 1, x_lim = xlim_all, y_lim = ylim_all, size = 0.1)
plot_all_regions_at_time(data, locs_name = "grid_D", field_name = "A_grid", comp_id = 2, x_lim = xlim_all, y_lim = ylim_all, size = 0.1)
plot_all_regions_at_time(data, locs_name = "grid_D", field_name = "A_grid", comp_id = 3, x_lim = xlim_all, y_lim = ylim_all, size = 0.1)

plot.curve(data$grid_T, data$E_grid[[1]][,1]) + std_plot_settings_curves()
plot.curve(data$grid_T, data$E_grid[[1]][,2]) + std_plot_settings_curves()
plot.curve(data$grid_T, data$E_grid[[1]][,3]) + std_plot_settings_curves()

norm_l2(data$E_grid[[1]][,3])

library(animation)

tt <- data$locations_T
n_frames <- length(tt)

# for(i in 1:4) {
#   
#   saveGIF({
#     for (t in seq_len(n_frames)) {
#       p <- plot.field_tile(
#         data$locations_D[[i]],
#         data$X[[i]][t, ],
#         boundary = data$domain_D[[i]]$boundary,
#         # true = data$X_locs[[i]][t, ],
#         lim = range(data$X[[i]])
#       ) +
#         ggtitle(paste("Block", i, "- Time =", round(tt[t], digits = 2))) +
#         std_plot_settings_fields()
#       
#       print(p)  # important for ggplot inside saveGIF
#     }
#   },
#   movie.name = paste0("block", i ,".gif"),
#   # 1 second total → each frame lasts 1/n_frames seconds
#   interval = 10 / n_frames,
#   ani.width = 600,
#   ani.height = 400
#   )
#   
# }

# Compute global plot bounds over all regions
xlim_all <- c(0, 55)
ylim_all <- c(-30, 0)

# ---------- ALL REGIONS GIF ----------
saveGIF({
  for (t in seq_len(n_frames)) {
    p <- plot_all_regions_at_time(
      data      = data,
      t_index   = t,
      x_lim  = xlim_all,
      y_lim  = ylim_all,
      size = 4
    )
    print(p)
  }
},
movie.name = "all_regions.gif",
interval   = 10 / n_frames,
ani.width  = 900,
ani.height = 500
)

library(clue)
library(RGCCA)
source("src/utils/directories.R")
source("tests/RGCCA-2D/utils/wrappers.R")
path_list <- list(
  cpp_script = "cpp/RGCCA-2D/",
  batch = "prova/",
  tmp_data = "prova/",
  tmp_results = "prova/"
)
IGNORE_CPP_OUTPUT <- FALSE
test_options$batch <- 0
model_RGCCA <- fit_model("R_RGCCA", data, path_list, test_options)
# model_tfRGCCA <- fit_model("CPP_tfRGCCA", data, path_list, test_options)


data$results_RGCCA_A_locs <- model_RGCCA$results$A_hat_locs
plot_all_regions_at_time(data, locs_name = "locations_D", field_name = "results_RGCCA_A_locs", comp_id = 1, x_lim = xlim_all, y_lim = ylim_all, size = 4)
plot_all_regions_at_time(data, locs_name = "locations_D", field_name = "results_RGCCA_A_locs", comp_id = 2, x_lim = xlim_all, y_lim = ylim_all, size = 4)
plot_all_regions_at_time(data, locs_name = "locations_D", field_name = "results_RGCCA_A_locs", comp_id = 3, x_lim = xlim_all, y_lim = ylim_all, size = 4)

# data$results_tfRGCCA_A_grid <- model_tfRGCCA$results$A_hat_grid
# plot_all_regions_at_time(data, locs_name = "grid_D", field_name = "results_tfRGCCA_A_grid", comp_id = 1, x_lim = xlim_all, y_lim = ylim_all, size = 0.1)
# plot_all_regions_at_time(data, locs_name = "grid_D", field_name = "results_tfRGCCA_A_grid", comp_id = 2, x_lim = xlim_all, y_lim = ylim_all, size = 0.1)
# plot_all_regions_at_time(data, locs_name = "grid_D", field_name = "results_tfRGCCA_A_grid", comp_id = 3, x_lim = xlim_all, y_lim = ylim_all, size = 0.1)

# 
# plot.curve(data$locations_T, model_RGCCA$results$E_hat_locs[[1]][,1]) + std_plot_settings_curves()
# plot.curve(data$locations_T, model_RGCCA$results$E_hat_locs[[2]][,1]) + std_plot_settings_curves()
# plot.curve(data$locations_T, model_RGCCA$results$E_hat_locs[[3]][,1]) + std_plot_settings_curves()
# plot.curve(data$locations_T, model_RGCCA$results$E_hat_locs[[4]][,1]) + std_plot_settings_curves()
# 
# plot.curve(data$grid_T, model_tfRGCCA$results$E_hat_grid[[1]][,1]) + std_plot_settings_curves()
# plot.curve(data$grid_T, model_tfRGCCA$results$E_hat_grid[[2]][,1]) + std_plot_settings_curves()
# plot.curve(data$grid_T, model_tfRGCCA$results$E_hat_grid[[3]][,1]) + std_plot_settings_curves()
# plot.curve(data$grid_T, model_tfRGCCA$results$E_hat_grid[[4]][,1]) + std_plot_settings_curves()
