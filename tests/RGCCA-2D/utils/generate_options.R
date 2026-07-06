# = ========================================================================== =
# - Script: generate_options.R
# - Desc: Generates JSON option files for test configurations.
# = ========================================================================== =

if (!exists("rgcca_model_options") ||
  !exists("rgcca_bootstrap_options")) {
  source("src/utils/rgcca_options.R")
}
#' Generate option JSON files for a test suite and test name.
#'
#' @param test_suite Test-suite directory name.
#' @param name_main_test Main test name or test-group name.
#' @param path_queue Directory where option JSON files are stored.
#' @return The value produced by `generate_options`.
generate_options <- function(test_suite, name_main_test, path_queue) {
  ## Create the directory (if it does not exist yet)
  mkdir(c(path_queue))
  n_reps <- if (exists("SMOKE_TEST") && isTRUE(SMOKE_TEST)) 1 else 30

  ## Names of the models you want to compare
  # - model_names: used for indexing (no spaces, please)
  # - model_labels: used for plotting
  model_names <- c(
    "R_GCCA_cor", "R_RGCCA", "R_GCCA_cov",
    "CPP_GCCA_cor", "CPP_RGCCA", "CPP_GCCA_cov",
    "CPP_fGCCA_cor", "CPP_fRGCCA", "CPP_fGCCA_cov",
    "CPP_GCCA_NN_cor", "CPP_RGCCA_NN", "CPP_GCCA_NN_cov",
    "CPP_fGCCA_NN_cor", "CPP_fRGCCA_NN", "CPP_fGCCA_NN_cov",
    "CPP_tfGCCA_NN_cor", "CPP_tfRGCCA_NN", "CPP_tfGCCA_NN_cov"
  )
  model_labels <- c(
    "GCCA - R - cor", "RGCCA - R", "GCCA - R - cov",
    "GCCA - C++ - cor", "RGCCA - C++", "GCCA - C++ - cov",
    "fGCCA - cor", "fRGCCA", "fGCCA - cov",
    "GCCA - C++ - NN - cor", "RGCCA - C++ - NN", "GCCA - C++ - NN - cov",
    "fGCCA - NN - cor", "fRGCCA - NN", "fGCCA - NN - cov",
    "tfGCCA - NN - cor", "tfRGCCA - NN", "tfGCCA - NN - cov"
  )

  ## Define the color palette
  model_colors <- c(
    brewer.pal(5, "Greys")[5:3],
    brewer.pal(5, "Reds")[5:3],
    brewer.pal(5, "Blues")[5:3],
    brewer.pal(5, "Oranges")[5:3],
    brewer.pal(5, "Purples")[5:3],
    brewer.pal(5, "Greens")[5:3]
  )

  #' Write one sensitivity-test option grid for a threading mode.
  #'
  #' @param lambda_values Lambda values for this option grid.
  #' @param threading Threading mode token.
  #' @param model_selection Whether model selection is enabled.
  #' @return The value produced by `write_sensitivity_options`.
  write_sensitivity_options <- function(lambda_values, threading, model_selection) {
    options <- list(
      model_names = model_names[c(2, 3, 4, 5, 6) * 3],
      model_labels = model_labels[c(2, 3, 4, 5, 6) * 3],
      model_colors = model_colors[c(2, 3, 4, 5, 6) * 3],
      cpp_script = "RGCCA-2D",
      test_options = list(
        n_reps = n_reps,
        varying_options = c("lambda", "sigma_noise"),
        threading = threading
      ),
      domain_and_locations = list(
        name_mesh = paste0("region_", 1:4),
        T_sec = 200
      ),
      dimensions = list(
        n_groups = 4,
        n_nodes_T = c(51),
        n_nodes_HR_grid_D = c(4 * 1e3, 2 * 1e3, 2 * 1e3, 4 * 1e3),
        n_nodes_HR_grid_T = 501,
        n_locs_D = c(300),
        n_locs_mult = c(3, 2, 2, 3),
        n_times = c(1200)
      ),
      model_options = rgcca_model_options(
        n_comp = 3,
        lambda_selection_weights = model_selection,
        block_deactivation = model_selection,
        connection_deactivation = model_selection,
        component_significance = FALSE
      ),
      bootstrap_options = rgcca_bootstrap_options(
        B_max = 5000,
        adaptive = TRUE,
        stable_checks_required = 10
      ),
      noise = list(
        sigma_noise = c(0, 1, 2, 4, 6)
      ),
      regularization = list(
        lambda = lambda_values,
        lambda_grid = 10^(-9:2)
      )
    )

    name_fun <- function(opts_i, comb_row) {
      paste(
        name_main_test,
        "l", sprintf("%.0e", comb_row$lambda),
        "sd", sprintf("%.3f", comb_row$sigma_noise),
        sep = "_"
      )
    }

    options_list <- explode_options(
      options,
      by = options$test_options$varying_options,
      name_fun = name_fun
    )

    write_options_json(
      options_list,
      dir = path_queue,
      name_field = "name_test"
    )
  }

  switch(name_main_test,
    testSensitivity = {
      stop(
        paste(
          "testSensitivity is a grouped test. Run one of:",
          "testSensitivitySingleThread, testSensitivityMultiThread"
        )
      )
    },
    testSensitivitySingleThread = {
      write_sensitivity_options(
        lambda_values = c(0, 10^(-6:4)),
        threading = "single",
        model_selection = FALSE
      )
    },
    testSensitivityMultiThread = {
      write_sensitivity_options(
        lambda_values = -1,
        threading = "multi",
        model_selection = TRUE
      )
    },
    testResampling = {
      ## Set the desired options
      options <- list(
        model_names = model_names[c(3, 5) * 3],
        model_labels = model_labels[c(3, 5) * 3],
        model_colors = model_colors[c(3, 5) * 3],
        cpp_script = "RGCCA-2D",
        test_options = list(
          n_reps = n_reps,
          varying_options = c("stationary_block_length", "sigma_noise"),
          threading = "multi"
        ),
        domain_and_locations = list(
          name_mesh = paste0("region_", 1:4),
          T_sec = 200
        ),
        dimensions = list(
          n_groups = 4,
          n_nodes_T = c(51),
          n_nodes_HR_grid_D = c(4 * 1e3, 2 * 1e3, 2 * 1e3, 4 * 1e3),
          n_nodes_HR_grid_T = 501,
          n_locs_D = c(300),
          n_locs_mult = c(3, 2, 2, 3),
          n_times = c(1200)
        ),
        model_options = rgcca_model_options(
          n_comp = 3,
          lambda_selection_weights = TRUE,
          block_deactivation = TRUE,
          connection_deactivation = TRUE,
          component_significance = FALSE
        ),
        bootstrap_options = rgcca_bootstrap_options(
          B_max = 5000,
          adaptive = TRUE,
          stable_checks_required = 10,
          resampling_strategy = "Stationary",
          stationary_block_length = c(0, 1, 10, 50, 100)
        ),
        noise = list(
          sigma_noise = c(6)
        ),
        regularization = list(
          lambda = -1,
          lambda_grid = 10^(-9:2)
        )
      )

      ## File naming policy
      #' Build a stable option file stem for one expanded option combination.
      #'
      #' @param opts_i Expanded option object.
      #' @param comb_row One row of the option grid.
      #' @return The value produced by `name_fun`.
      name_fun <- function(opts_i, comb_row) {
        paste(
          name_main_test,
          ## Include all the varying options!
          "sbl", sprintf("%04d", comb_row$stationary_block_length),
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
      options_list <- lapply(options_list, function(opts_i) {
        if (!is.null(opts_i$bootstrap_options$stationary_block_length) &&
          opts_i$bootstrap_options$stationary_block_length <= 0) {
          opts_i$bootstrap_options$resampling_strategy <- "Ordinary"
        }
        opts_i
      })

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
