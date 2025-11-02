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
    ## ....
  )
  model_labels <- c(
    ## ....
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
        cpp_script = "fPCA-2D",
        test_options = list(
          n_reps = 10,
          varying_options = c("n_nodes", "n_stat_units", "NSR")
        ),
        domain_and_locations = list(
          name_mesh = "unit_square",
          locs_eq_nodes = TRUE
        ),
        dimensions = list(
          n_nodes = c(100, 200, 400),
          # n_locs = c(100, 200, 400),
          n_stat_units = c(50, 100, 200),
          n_nodes_HR_grid = 1000
        ),
        model_options = list(
          ## ....
        ),
        data = list(
          ## ....
        ),
        noise = list(
          NSR = seq(0, 0.5, length = 5),
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
          "nsr", sprintf("%.3f", comb_row$NSR),
          "nn", sprintf("%04d", comb_row$n_nodes),
          "nsu", sprintf("%04d", comb_row$n_stat_units),
          # "nl", sprintf("%04d", comb_row$n_locs),
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
