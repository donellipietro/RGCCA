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

for(i in 1:4) {
  
  saveGIF({
    for (t in seq_len(n_frames)) {
      p <- plot.curve(
        data$locations_D,
        data$X[[i]][t, ],
        true = data$X_locs[[i]][t, ],
        lim = range(data$X[[i]])
      ) +
        ggtitle(paste("Block", i, "- Time =", round(tt[t], digits = 2))) +
        std_plot_settings_curves()
      
      print(p)  # important for ggplot inside saveGIF
    }
  },
  movie.name = paste0("block", i ,".gif"),
  # 1 second total → each frame lasts 1/n_frames seconds
  interval = 10 / n_frames,
  ani.width = 600,
  ani.height = 400
  )
  
}
