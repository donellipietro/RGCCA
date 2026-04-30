
dim(data$domain_D[[4]]$nodes)

id <- 1
plot.field_tile(
  data$grid_D[[id]],
  data$A_grid[[id]][,1]
)



id <- 1
t <- 1
limits <- range(data$X[[id]])

plot.field_tile(
  data$locations_D[[id]],
  data$X[[id]][t,],
  limits = limits,
)
t <- t+1

t <- 1
limits <- range(data$X_grid[[id]])

plot.field_tile(
  data$grid_D[[id]],
  data$X_grid[[id]][t,],
  limits = limits,
)
t <- t+1


colnames(data$X[[4]])
rownames(data$X[[1]])

batch_idx <- 0 
data <- generate_data(
  test_options = test_options,
  seed = 4 * batch_idx
)

model_name <- "R_RGCCA"
result <- R_RGCCA(model_name, data, test_options)
# model_name <- "CPP_RGCCA"
# result <- CPP_RGCCA(model_name, data, test_options, path_list)


model_name <- "CPP_tfGCCA_cov"
result <- CPP_RGCCA(model_name, data, test_options, path_list)

model_name <- "CPP_fGCCA_cov"
result <- CPP_RGCCA(model_name, data, test_options, path_list)


# points
id <- 1
plot_list <- list()
for(g in 1:4) {
  for(h in 1:3) { 
    plot_list[[(g-1)*3 + h]] <- plot.field_points(
      data$locations_D[[g]],
      result$results$A_hat_locs[[g]][,h],
      boundary = data$domain_D[[g]]$boundary,
      size = 2
    ) + std_plot_settings_fields()
  }
}
plot <- arrangeGrob(grobs = plot_list, ncol = 3)
grid.arrange(plot)

# tile
id <- 1
plot_list <- list()
for(g in 1:4) {
  for(h in 1:3) { 
    plot_list[[(g-1)*3 + h]] <- plot.field_tile(
      data$grid_D[[g]],
      result$results$A_hat_grid[[g]][,h],
      boundary = data$domain_D[[g]]$boundary
    ) + std_plot_settings_fields()
  }
}
plot <- arrangeGrob(grobs = plot_list, ncol = 3)
grid.arrange(plot)


plot_list <- list()
for(g in 1:4) {
  for(h in 1:3) { 
    plot_list[[(g-1)*3 + h]] <- plot.curve_points(
      data$locations_T,
      result$results$E_hat_locs[[g]][,h],
      true = data$E_locs[[g]][,h]
    ) + std_plot_settings_curves()
  }
}
plot <- arrangeGrob(grobs = plot_list, ncol = 3)
grid.arrange(plot)
