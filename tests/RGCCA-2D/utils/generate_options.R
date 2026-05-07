# = ========================================================================== =
# - Script: generate_options.R
# - Desc: Generates JSON option files for test configurations.
# = ========================================================================== =

## Function: generate_options(test_suite, name_main_test, path_queue)
# - Args:
#   * test_suite: name of the calling test suite (used for directory structure)
#   * name_main_test: identifier of the specific test to generate options for
#   * path_queue: directory where JSON files will be written
# - Desc:
#   Defines model parameters, expands selected grid options, and writes the
#   resulting combinations to JSON files ready for execution.
generate_options <- function(test_suite, name_main_test, path_queue) {
  ## Create the directory (if it does not exist yet)
  mkdir(c(path_queue))

  ## Names of the models you want to compare
  # - model_names: used for indexing (no spaces, please)
  # - model_labels: used for plotting
  model_names <- c(
    "R_GCCA_cor", "R_RGCCA", "R_GCCA_cov",
    
    "CPP_GCCA_cor", "CPP_RGCCA", "CPP_GCCA_cov",
    "CPP_fGCCA_cor", "CPP_fRGCCA", "CPP_fGCCA_cov",
    
    "CPP_GCCA_NN_cor", "CPP_RGCCA_NN", "CPP_GCCA_NN_cov",
    "CPP_fGCCA_NN_cor", "CPP_fRGCCA_NN", "CPP_fGCCA_NN_cov"
  )
  model_labels <- c(
    "GCCA - R - cor", "RGCCA - R", "GCCA - R - cov", 
    
    "GCCA - C++ - cor", "RGCCA - C++", "GCCA - C++ - cov",
    "fGCCA - cor", "fRGCCA", "fGCCA - cov",
    
    "GCCA - C++ - NN - cor", "RGCCA - C++ - NN", "GCCA - C++ - NN - cov",
    "fGCCA - NN - cor", "fRGCCA - NN", "fGCCA - NN - cov"
  )
  
  ## Define the color palette
  model_colors <- c(
    brewer.pal(5, "Greys")[5:3], 
    
    brewer.pal(5, "Reds")[5:3],
    brewer.pal(5, "Blues")[5:3],
    
    brewer.pal(5, "Oranges")[5:3],
    brewer.pal(5, "Purples")[5:3])

  switch(name_main_test,
    testSensitivity = {
      ## Set the desired options
      options <- list(
        model_names = model_names[c(3,5)*3], # (1:5)*3
        model_labels = model_labels[c(3,5)*3],
        model_colors = model_colors[c(3,5)*3],
        cpp_script = "RGCCA-2D",
        test_options = list(
          n_reps = 30,
          varying_options = c("lambda", "sigma_noise")
        ),
        domain_and_locations = list(
          name_mesh = paste0("region_", 1:4),
          T_sec = 200
        ),
        dimensions = list(
          n_groups = 4,
          n_nodes_T = c(51),
          n_nodes_HR_grid_D = c(4*1e3, 2*1e3, 2*1e3, 4*1e3),
          n_nodes_HR_grid_T = 501,
          n_locs_D = c(400),
          n_locs_mult = c(3, 2, 2, 3),
          n_times = c(1200)
        ),
        model_options = list(          
          n_comp = 3,
          init = "svd",
          tau = "optimal",
          scheme = "factorial"
        ),
        noise = list(
          sigma_noise = c(0.01, 1, 2, 4)
        ),
        regularization = list(
          lambda = 10^(-6:1)
        )
      )

      ## File naming policy
      name_fun <- function(opts_i, comb_row) {
        paste(
          name_main_test,
          ## Include all the varying options!
          "l", sprintf("%.0e", comb_row$lambda),
          "sd", sprintf("%.3f", comb_row$sigma_noise),
          sep = "_"
        )
      }

      ## Expand ONLY the varying options
      options_list <- explode_options(
        options,
        by = options$test_options$varying_options,
        name_fun = name_fun
      )

      ## Write JSON files
      write_options_json(
        options_list,
        dir = path_queue,
        name_field = "name_test"
      )
    },
    {
      stop(paste("The test", name_main_test, "does not exist"))
    }
  )
}
