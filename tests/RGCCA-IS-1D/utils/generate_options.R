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
    
    "CPP_fGCCA_cor_FEM", "CPP_fRGCCA_FEM", "CPP_fGCCA_cov_FEM",
    "CPP_fGCCA_cor_SPLINES", "CPP_fRGCCA_SPLINES", "CPP_fGCCA_cov_SPLINES",
    
    "CPP_GCCA_NN_cor", "CPP_RGCCA_NN", "CPP_GCCA_NN_cov",
    
    "CPP_fGCCA_NN_cor_FEM", "CPP_fRGCCA_NN_FEM", "CPP_fGCCA_NN_cov_FEM",
    "CPP_fGCCA_NN_cor_SPLINES", "CPP_fRGCCA_NN_SPLINES", "CPP_fGCCA_NN_cov_SPLINES",
    
    "R_FGCCA_cor", "R_FGCCA_cov"
  )
  model_labels <- c(
    "GCCA - R - cor", "RGCCA - R", "GCCA - R - cov", 
    "GCCA - C++ - cor", "RGCCA - C++", "GCCA - C++ - cov",
    
    "fGCCA - cor - FEM", "fRGCCA - FEM", "fGCCA - cov - FEM",
    "fGCCA - cor - Splines", "fRGCCA - Splines", "fGCCA - cov - Splines",
    
    "GCCA - C++ - NN - cor", "RGCCA - C++ - NN", "GCCA - C++ - NN - cov",
    
    "fGCCA - NN - cor - FEM", "fRGCCA - NN - FEM", "fGCCA - NN - cov - FEM",
    "fGCCA - NN - cor - Splines", "fRGCCA - NN - Splines", "fGCCA - NN - cov - Splines",
    
    "FGCCA - R - cov", "FGCCA - R - cor"
  )
  
  ## Define the color palette
  model_colors <- c(
    brewer.pal(5, "Reds")[5:3],     # R Multivariate
    brewer.pal(5, "Reds")[5:3],     # CPP Multivariate
    
    brewer.pal(5, "Blues")[5:3],    # CPP Functional FEM
    brewer.pal(5, "Blues")[5:3],    # CPP Functional SPLINES
    
    brewer.pal(5, "Oranges")[5:3],  # CPP Multivariate NN
    brewer.pal(5, "Purples")[5:3],  # CPP Functional NN FEM
    brewer.pal(5, "Purples")[5:3],  # CPP Functional NN SPLINES
    
    "grey", "grey")
  
  switch(name_main_test,
         testSensitivity = {
           ## Set the desired options
           options <- list(
             model_names = model_names[c(6,9,12,15,18,21)],
             model_labels = model_labels[c(6,9,12,15,18,21)],
             model_colors = model_colors[c(6,9,12,15,18,21)],
             cpp_script = "RGCCA",
             test_options = list(
               n_reps = 30,
               varying_options = c("lambda", "sigma_noise")
             ),
             domain_and_locations = list(
               name_mesh = "unit_interval",
               locs_eq_nodes = FALSE
             ),
             dimensions = list(
               n_groups = 4,
               n_nodes_D = 101,
               n_nodes_HR_grid_D = 200,
               n = 1200,
               n_locs = 101
             ),
             model_options = list(          
               n_comp = 3,
               lambda_selection_weights = FALSE,
               n_bootstrap_samples = 100
             ),
             noise = list(
               sigma_noise = c(0.01, 1.0, 2.0, 3.0, 4.0, 5.0)
             ),
             regularization = list(
               lambda = c(-1, 0, 10^(-9:-2)),
               lambda_grid = list(
                 10^(-9:-2), 10^(-9:-2), 10^(-9:-2)
               )
             )
           )
           
           ## File naming policy
           name_fun <- function(opts_i, comb_row) {
             paste(
               name_main_test,
               ## Include all the varying options!
               "l",  sprintf("%.0e", comb_row$lambda),
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
         testBootstrap = {
           ## Set the desired options
           options <- list(
             model_names = model_names[c(9,12,18,21)],
             model_labels = model_labels[c(9,12,18,21)],
             model_colors = model_colors[c(9,12,18,21)],
             cpp_script = "RGCCA",
             test_options = list(
               n_reps = 30,
               varying_options = c("n_bootstrap_samples", "sigma_noise")
             ),
             domain_and_locations = list(
               name_mesh = "unit_interval",
               locs_eq_nodes = FALSE
             ),
             dimensions = list(
               n_groups = 4,
               n_nodes_D = 101,
               n_nodes_HR_grid_D = 200,
               n = 1200,
               n_locs = 101
             ),
             model_options = list(          
               n_comp = 3,
               lambda_selection_weights = FALSE,
               n_bootstrap_samples = c(10, 100, 500, 1000)
             ),
             noise = list(
               sigma_noise = c(0.01, 1.0, 2.0, 3.0, 4.0, 5.0)
             ),
             regularization = list(
               lambda = c(-1),
               lambda_grid = list(
                 10^(-9:-2), 10^(-9:-2), 10^(-9:-2)
               )
             )
           )
           
           ## File naming policy
           name_fun <- function(opts_i, comb_row) {
             paste(
               name_main_test,
               ## Include all the varying options!
               "nbs", sprintf("%.0f", comb_row$n_bootstrap_samples),
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
