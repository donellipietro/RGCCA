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
    brewer.pal(5, "Reds")[5:3], 
    brewer.pal(5, "Oranges")[5:3],
    
    brewer.pal(5, "Blues")[5:3],
    brewer.pal(5, "Blues")[5:3],
    
    brewer.pal(5, "Greens")[5:3],
    brewer.pal(5, "Purples")[5:3],
    brewer.pal(5, "Purples")[5:3],
    
    "grey", "grey")
  
  switch(name_main_test,
         testMV = {
           ## Set the desired options
           options <- list(
             model_names = model_names[c(1,4,7, 2,5,8, 3,6,9)][c(1,2, 4,5, 7,8)],
             model_labels = model_labels[c(1,4,7, 2,5,8, 3,6,9)][c(1,2, 4,5, 7,8)],
             model_colors = model_colors[c(1,4,7, 2,5,8, 3,6,9)][c(1,2, 4,5, 7,8)],
             cpp_script = "RGCCA",
             test_options = list(
               n_reps = 3,
               varying_options = c("n", "sigma_noise") # , "n_nodes_D"
             ),
             domain_and_locations = list(
               name_mesh = "unit_interval",
               locs_eq_nodes = FALSE
             ),
             dimensions = list(
               n_groups = 4,
               n_nodes_D = 21, # c(21, 51),
               n_nodes_HR_grid_D = 200,
               n = c(50, 100, 200, 400, 800, 1600, 3200, 6400)
             ),
             model_options = list(          
               n_comp = 3
             ),
             data = list(
               delta = 0.05
             ),
             noise = list(
               sigma_noise = c(0.01, 0.1, 0.5, 0.8)
             )
           )
           
           ## File naming policy
           name_fun <- function(opts_i, comb_row) {
             paste(
               name_main_test,
               ## Include all the varying options!
               "n", sprintf("%04d", comb_row$n),
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
         testMV_dual = {
           ## Set the desired options
           options <- list(
             model_names = model_names[c(1,4,7, 2,5,8, 3,6,9)][-c(3,6,9)],
             model_labels = model_labels[c(1,4,7, 2,5,8, 3,6,9)][-c(3,6,9)],
             model_colors = model_colors[c(1,4,7, 2,5,8, 3,6,9)][-c(3,6,9)],
             cpp_script = "RGCCA",
             test_options = list(
               n_reps = 3,
               varying_options = c("n_locs", "sigma_noise") # , "n_nodes_D"
             ),
             domain_and_locations = list(
               name_mesh = "unit_interval",
               locs_eq_nodes = FALSE
             ),
             dimensions = list(
               n_groups = 4,
               n_nodes_D = 21, #c(21, 51),
               n_nodes_HR_grid_D = 200,
               n = 300,
               n_locs = c(101, 201, 401, 801)
             ),
             model_options = list(          
               n_comp = 3
             ),
             data = list(
               # delta = c()
             ),
             noise = list(
               sigma_noise = c(0.01, 0.1, 0.5, 0.8)
             )
           )
           
           ## File naming policy
           name_fun <- function(opts_i, comb_row) {
             paste(
               name_main_test,
               ## Include all the varying options!
               "n_locs", sprintf("%04d", comb_row$n_locs),
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
         testSensitivity = {
           ## Set the desired options
           options <- list(
             model_names = model_names[c(4,7,10,13,16,19, 5,8,11,14,17,20, 6,9,12,15,18,21)],
             model_labels = model_labels[c(4,7,10,13,16,19, 5,8,11,14,17,20, 6,9,12,15,18,21)],
             model_colors = model_colors[c(4,7,10,13,16,19, 5,8,11,14,17,20, 6,9,12,15,18,21)],
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
               n_nodes_D = 21, #c(21, 51),
               n_nodes_HR_grid_D = 200,
               n = 200,
               n_locs = 101
             ),
             model_options = list(          
               n_comp = 3
             ),
             noise = list(
               sigma_noise = c(0.01, 0.1, 0.5, 0.8, 1.0, 2.0)
             ),
             regularization = list(
               lambda = 10^(-9:1)
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
         test1 = {
           ## Set the desired options
           options <- list(
             model_names = model_names[c(1,10,13, 2,11,14, 3,12,15)], #[c(7,8,9)],
             model_labels = model_labels[c(1,10,13, 2,11,14, 3,12,15)], # [c(7,8,9)],
             model_colors = model_colors[c(1,10,10, 2,11,11, 3,12,12)], #[c(7,8,9)],
             cpp_script = "RGCCA",
             test_options = list(
               n_reps = 3,
               varying_options = c("n_locs", "sigma_noise") # , "n_nodes_D"
             ),
             domain_and_locations = list(
               name_mesh = "unit_interval",
               locs_eq_nodes = FALSE
             ),
             dimensions = list(
               n_groups = 4,
               n_nodes_D = 21, #c(21, 51),
               n_nodes_HR_grid_D = 200,
               n = 200,
               n_locs = c(21, 51, 101, 201)
             ),
             model_options = list(          
               n_comp = 3
             ),
             data = list(
               # delta = c(0.05, 0.02, 0.01, 0.005)  locations 
             ),
             noise = list(
               sigma_noise = c(0.01, 0.1, 0.5, 0.8)
             ),
             regularization = list(
               lambda = 1e-4
             )
           )
           
           ## File naming policy
           name_fun <- function(opts_i, comb_row) {
             paste(
               name_main_test,
               ## Include all the varying options!
               "nl", sprintf("%04d", comb_row$n_locs),
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
         test2 = {
           ## Set the desired options
           options <- list(
             model_names = model_names[c(3,17,12,15)],
             model_labels = model_labels[c(3,17,12,15)],
             model_colors = model_colors[c(3,13,12,12)],
             cpp_script = "RGCCA",
             test_options = list(
               n_reps = 10,
               varying_options = c("n_locs", "sigma_noise")
             ),
             domain_and_locations = list(
               name_mesh = "unit_interval",
               locs_eq_nodes = FALSE
             ),
             dimensions = list(
               n_groups = 4,
               n_nodes_D = 21, #c(21, 51),
               n_nodes_HR_grid_D = 200,
               n = 200,
               n_locs = c(101)
             ),
             model_options = list(          
               n_comp = 3
             ),
             data = list(
               # delta = c(0.01)  locations 
             ),
             noise = list(
               sigma_noise = c(0.01, 0.1, 0.5, 0.8)
             ),
             regularization = list(
               lambda = 1e-4
             )
           )
           
           ## File naming policy
           name_fun <- function(opts_i, comb_row) {
             paste(
               name_main_test,
               ## Include all the varying options!
               "nl", sprintf("%04d", comb_row$n_locs),
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
