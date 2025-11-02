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
    "fda_splines", 
    "fdaPDE_splines_E", "fdaPDE_splines_H", 
    "fdaPDE_fem_E", "fdaPDE_fem_H",
    "fdaPDE_graph_E", "fdaPDE_graph_H"
  )
  model_labels <- c(
    "fda splines", 
    "fdaPDE splines - Exact", "fdaPDE splines - Hutchinson",
    "fdaPDE fem - Exact", "fdaPDE fem - Hutchinson",
    "fdaPDE graph - Exact", "fdaPDE graph - Hutchinson"
  )
  
  ## Define the color palette
  # model_colors <- brewer.pal(length(model_labels), "Set1")
  model_colors <- c(
    brewer.pal(3, "Greys")[3], 
    brewer.pal(3, "Blues")[2:3], 
    brewer.pal(3, "Purples")[2:3], 
    brewer.pal(3, "Greens")[2:3]
  )
  
  
  ## Options that you want to be common across tests
  lambda_grid <- 10^seq(-12, 1, by = 0.5)
  
  switch(name_main_test,
         test0 = {
           ## Set the desired options
           options <- list(
             model_names = model_names,
             model_labels = model_labels,
             model_colors = model_colors,
             cpp_script = "smoothing-1D",
             test_options = list(
               n_reps = 30,
               varying_options = c("n_knots")
             ),
             domain_and_locations = list(
               name_mesh = "unit_interval",
               locs_eq_knots = TRUE
             ),
             dimensions = list(
               n_knots = c(51, 101, 201, 401),
               n_knots_grid = 1000
             ),
             model_options = list(
               ## ....
             ),
             data = list(
               ## ....
             ),
             noise = list(
               NSR = 0
             ),
             regularization = list(
               lambda_grid = 1e-12
             )
           )
           
           ## File naming policy
           name_fun <- function(opts_i, comb_row) {
             paste(
               name_main_test,
               ## Include all the varying options!
               "nn", sprintf("%04d", comb_row$n_knots),
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
             model_names = model_names,
             model_labels = model_labels,
             model_colors = model_colors,
             cpp_script = "smoothing-1D",
             test_options = list(
               n_reps = 30,
               varying_options = c("n_knots", "NSR")
             ),
             domain_and_locations = list(
               name_mesh = "unit_interval",
               locs_eq_knots = TRUE
             ),
             dimensions = list(
               n_knots = c(51, 101, 201, 401),
               n_knots_grid = 1000
             ),
             model_options = list(
               ## ....
             ),
             data = list(
               ## ....
             ),
             noise = list(
               NSR = seq(0, 1, length = 5)
             ),
             regularization = list(
               lambda_grid = lambda_grid
             )
           )
           
           ## File naming policy
           name_fun <- function(opts_i, comb_row) {
             paste(
               name_main_test,
               ## Include all the varying options!
               "nn", sprintf("%04d", comb_row$n_knots),
               "nsr", sprintf("%.3f", comb_row$NSR),
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
             model_names = model_names[1:5],
             model_labels = model_labels[1:5],
             model_colors = model_colors[1:5],
             cpp_script = "smoothing-1D",
             test_options = list(
               n_reps = 30,
               varying_options = c("n_knots", "NSR", "n_locs")
             ),
             domain_and_locations = list(
               name_mesh = "unit_interval",
               locs_eq_knots = FALSE
             ),
             dimensions = list(
               n_knots = c(51, 101, 201),
               n_locs = c(25, 50, 100, 200, 400),
               n_knots_grid = 1000
             ),
             model_options = list(
               ## ....
             ),
             data = list(
               ## ....
             ),
             noise = list(
               NSR = seq(0, 1, length = 5)
             ),
             regularization = list(
               lambda_grid = lambda_grid
             )
           )
           
           ## File naming policy
           name_fun <- function(opts_i, comb_row) {
             paste(
               name_main_test,
               ## Include all the varying options!
               "nn", sprintf("%04d", comb_row$n_knots),
               "nl", sprintf("%04d", comb_row$n_locs),
               "nsr", sprintf("%.3f", comb_row$NSR),
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
