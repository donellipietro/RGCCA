data$locations_D

plot.curve(data$grid_D, data$A_grid[[1]][,1]) + std_plot_settings_curves()
plot.curve(data$grid_D, data$A_grid[[1]][,2]) + std_plot_settings_curves()
plot.curve(data$grid_D, data$A_grid[[1]][,3]) + std_plot_settings_curves()

plot.curve(data$grid_T, data$E_grid[[1]][,1]) + std_plot_settings_curves()
plot.curve(data$grid_T, data$E_grid[[1]][,2]) + std_plot_settings_curves()
plot.curve(data$grid_T, data$E_grid[[1]][,3]) + std_plot_settings_curves()

norm_l2(data$E_grid[[1]][,3])

library(animation)

tt <- data$locations_T
n_frames <- length(tt)

saveGIF({
  for (t in seq_len(n_frames)) {
    p <- plot.curve(
      data$locations_D,
      data$X[[1]][t, ],
      true = data$X_locs[[1]][t, ],
      lim = range(data$X[[1]])
    ) +
      ggtitle(paste("Block 1 - Time =", tt[t])) +
      std_plot_settings_curves()
    
    print(p)  # important for ggplot inside saveGIF
  }
},
movie.name = "block1.gif",
# 1 second total → each frame lasts 1/n_frames seconds
interval = 10 / n_frames,
ani.width = 600,
ani.height = 400
)
