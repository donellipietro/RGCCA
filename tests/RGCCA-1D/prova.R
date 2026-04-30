

par(mfrow = c(1,4), mar = c(2,2,2,2))
matplot(data$locations_D, t(data$X[[1]]), type = "l", xlab = "", ylab = "", main = "Group 1")
matplot(data$locations_D, t(data$X[[2]]), type = "l", xlab = "", ylab = "", main = "Group 2")
matplot(data$locations_D, t(data$X[[3]]), type = "l", xlab = "", ylab = "", main = "Group 3")
matplot(data$locations_D, t(data$X[[4]]), type = "l", xlab = "", ylab = "", main = "Group 4")

par(mfrow = c(2,2), mar = c(2,2,2,2))
matplot(data$locations_D, t(data$X_locs[[1]]), type = "l", xlab = "", ylab = "", main = "Group 1")
matplot(data$locations_D, t(data$X_locs[[2]]), type = "l", xlab = "", ylab = "", main = "Group 2")
matplot(data$locations_D, t(data$X_locs[[3]]), type = "l", xlab = "", ylab = "", main = "Group 3")
matplot(data$locations_D, t(data$X_locs[[4]]), type = "l", xlab = "", ylab = "", main = "Group 4")



data$locations_D

plot.curve(data$grid_D, data$A_grid[[1]][,1]) + std_plot_settings_curves()
plot.curve(data$grid_D, data$A_grid[[1]][,2]) + std_plot_settings_curves()
plot.curve(data$grid_D, data$A_grid[[1]][,3]) + std_plot_settings_curves()

plot.curve(data$grid_T, data$E_grid[[1]][,1]) + std_plot_settings_curves()
plot.curve(data$grid_T, data$E_grid[[1]][,2]) + std_plot_settings_curves()
plot.curve(data$grid_T, data$E_grid[[1]][,3]) + std_plot_settings_curves()

plot.curve(data$locations_T, data$E_locs[[1]][,1]) + std_plot_settings_curves()
plot.curve(data$locations_T, data$E_locs[[1]][,2]) + std_plot_settings_curves()
plot.curve(data$locations_T, data$E_locs[[1]][,3]) + std_plot_settings_curves()

norm_l2(data$H_grid[[1]][,3])

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



IGNORE_CPP_OUTPUT <- FALSE
results <- CPP_RGCCA("CPP_tfRGCCA", data, test_options, path_list)





