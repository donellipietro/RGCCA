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
    "R_RGCCA", "CPP_RGCCA", "CPP_RGCCA_improved", "CPP_fRGCCA", "CPP_tfRGCCA"
  )
  model_labels <- c(
    "RGCCA - R", "RGCCA - CPP", "RGCCA_improved", "fRGCCA", "tfRGCCA"
  )

  ## Define the color palette
  model_colors <- brewer.pal(length(model_labels), "Set1")

  ## Options that you want to be common across tests
  lambda_grid <- 10^seq(-12, 1, by = 1)

  switch(name_main_test,
    test1 = {
      ## Set the desired options
      options <- list(
        model_names = model_names,
        model_labels = model_labels,
        model_colors = model_colors,
        cpp_script = "RGCCA",
        test_options = list(
          n_reps = 15,
          varying_options = c("n_nodes_D", "n_nodes_T", "sigma_noise")
        ),
        domain_and_locations = list(
          name_mesh = "unit_interval",
          T_sec = 200,
          locs_eq_nodes = FALSE
        ),
        dimensions = list(
          n_groups = 4,
          n_nodes_D = c(51, 101, 201, 401),
          n_nodes_T = c(201, 401),
          n_nodes_HR_grid_D = 1000,
          n_nodes_HR_grid_T = 1000
        ),
        model_options = list(          
          n_comp = 3,
          init = "svd",
          tau = "optimal",
          scheme = "factorial"
        ),
        data = list(
          delta = 0.02,
          TR = 2
          ## ....
        ),
        noise = list(
          sigma_noise = seq(0, 0.5, length = 5)
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
          "nnd", sprintf("%04d", comb_row$n_nodes_D),
          "nnt", sprintf("%04d", comb_row$n_nodes_T),
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
