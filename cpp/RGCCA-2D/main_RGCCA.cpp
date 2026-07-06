#include <fdaPDE/models.h>
using namespace fdapde;
using fdapde::rgcca::IndependentSampling;
using fdapde::rgcca::InitStrategy;

#include "../include/json.hpp"
using nlohmann::json;

#include <Eigen/Dense>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <variant>

using matrix_t = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>;
using sparse_matrix_t = Eigen::SparseMatrix<double>;

#include "../include/rgcca_driver_options.hpp"

/// @brief Run the C++ RGCCA driver for one JSON parameter file.
/// @param argc Number of command-line arguments.
/// @param argv Command-line arguments; argv[1] must be the parameter JSON path.
/// @return Process exit status.
int main(int argc, char *argv[]) {

  // Check for argument
  if (argc < 2) {
    std::cerr << "Usage: " << argv[0] << " <params.json>" << std::endl;
    return 1;
  }
  std::string params_path = argv[1];

  // Load JSON
  std::ifstream input(params_path);
  if (!input) {
    throw std::runtime_error("Cannot open params.json in current directory.");
  }
  json jroot = json::parse(input);

  // Close and delete the file
  input.close();
  std::filesystem::remove(params_path);

  // Extract paths
  std::string path_mesh =
      rgcca_driver::resolve_path(jroot["path_list"].value("mesh", "./mesh/"));
  std::string path_data =
      rgcca_driver::resolve_path(jroot["path_list"].value("data", "./data/"));
  std::string path_results = rgcca_driver::resolve_path(
      jroot["path_list"].value("results", "./results/"));

  // Extract options
  int n_obs = jroot["model_options"].value("n_obs", 101);
  int n_comp = jroot["model_options"].value("n_comp", 3);
  double tau = jroot["model_options"].value("tau", 0.);

  // Choose options
  RGCCA<IndependentSampling>::Options options;
  options.init_strategy = InitStrategy::Uniform;
  rgcca_driver::apply_rgcca_options(jroot["model_options"], options, tau);

  RGCCA<IndependentSampling>::BootstrapConfig bootstrap_config;
  rgcca_driver::apply_bootstrap_options(jroot["bootstrap_options"],
                                        bootstrap_config);

  // Model initialization
  RGCCA<IndependentSampling> rgcca(n_obs, options, n_comp);

  // Add blocks
  for (int i = 1; i <= 4; ++i) {
    Eigen::Matrix<double, Dynamic, Dynamic> X =
        read_csv<double>(path_data + "X" + std::to_string(i) + ".csv")
            .as_matrix();
    rgcca.add_multivariate_block("X" + std::to_string(i), std::move(X));
  }

  rgcca_driver::apply_regularization_options(rgcca, jroot["model_options"],
                                             options);

  // Add connections
  rgcca_driver::apply_C_matrix(rgcca, jroot["model_options"], 4);

  // Bootstrap
  rgcca.set_bootstrap_config(bootstrap_config);

  // Fit
  const auto results = rgcca.fit();
  std::cout << results << std::endl;

  // Save results
  int id = 1;
  for (const auto &block : rgcca.blocks()) {
    write_csv(path_results + "A" + std::to_string(id) + "_hat_locs.csv",
              block->weights_m());
    write_csv(path_results + "A_star" + std::to_string(id) + "_hat_locs.csv",
              block->weights_star_m());
    write_csv(path_results + "E" + std::to_string(id) + "_hat_locs.csv",
              block->components_m());
    id++;
  }

  for (int h = 0; h < n_comp; h++) {
    write_csv(path_results + "objective" + std::to_string(h + 1) + ".csv",
              results[h].obj_history);
    write_csv(path_results + "covariance_matrix" + std::to_string(h + 1) +
                  ".csv",
              results[h].covariance_matrix);
    write_csv(path_results + "correlation_matrix" + std::to_string(h + 1) +
                  ".csv",
              results[h].correlation_matrix);
    write_csv(path_results + "tau" + std::to_string(h + 1) + ".csv",
              results[h].tau_values);
    write_csv(path_results + "lambda_components" + std::to_string(h + 1) +
                  ".csv",
              results[h].lambda_components_values);
    write_csv(
        path_results + "lambda_weights" + std::to_string(h + 1) + ".csv",
        rgcca_driver::first_value_column(results[h].lambda_weights_values));
  }

  const bool model_selection =
      rgcca_driver::bootstrap_selection_requested(options);
  rgcca_driver::write_component_diagnostics(path_results, results);

  if (model_selection) {
    const auto &boot_results = rgcca.bootstrap_selection_results();
    const bool save_resamples =
        rgcca_driver::save_bootstrap_resamples(jroot["bootstrap_options"]);

    for (std::size_t h = 0; h < boot_results.size(); ++h) {
      const auto &boot = boot_results[h];
      rgcca_driver::write_bootstrap_metadata(path_results, boot, h,
                                             save_resamples);

      write_csv(path_results + "bootstrap_lambda_grid" + std::to_string(h + 1) +
                    ".csv",
                boot.lambda_grid);
      write_csv(path_results + "bootstrap_criterion" + std::to_string(h + 1) +
                    ".csv",
                boot.criterion);
      write_csv(path_results + "bootstrap_lambda_opt" + std::to_string(h + 1) +
                    ".csv",
                std::vector<double>{boot.lambda_opt});

      for (std::size_t i = 0; i < boot.lambda_grid.size(); ++i) {
        write_csv(path_results + "bootstrap_corr_min_comp" +
                      std::to_string(h + 1) + "_lambda" +
                      std::to_string(i + 1) + ".csv",
                  boot.corr_min_by_lambda[i]);
        write_csv(path_results + "bootstrap_corr_ci_low_comp" +
                      std::to_string(h + 1) + "_lambda" +
                      std::to_string(i + 1) + ".csv",
                  boot.corr_ci_low_by_lambda[i]);
        write_csv(path_results + "bootstrap_corr_ci_high_comp" +
                      std::to_string(h + 1) + "_lambda" +
                      std::to_string(i + 1) + ".csv",
                  boot.corr_ci_high_by_lambda[i]);

        for (std::size_t j = 0; j < boot.w_fit_by_lambda[i].size(); ++j) {
          const auto &block = rgcca.blocks()[j];

          write_csv(path_results + "bootstrap_weights_fit_comp" +
                        std::to_string(h + 1) + "_lambda" +
                        std::to_string(i + 1) + "_block" +
                        std::to_string(j + 1) + "_locs.csv",
                    block->Psi_D() * boot.w_fit_by_lambda[i][j]);
          if (save_resamples &&
              rgcca_driver::has_bootstrap_weight_resamples(boot, i, j)) {
            write_csv(path_results + "bootstrap_weights_boot_comp" +
                          std::to_string(h + 1) + "_lambda" +
                          std::to_string(i + 1) + "_block" +
                          std::to_string(j + 1) + "_locs.csv",
                      block->Psi_D() * boot.w_boot_by_lambda[i][j]);
          }
          write_csv(path_results + "bootstrap_weights_wmin_comp" +
                        std::to_string(h + 1) + "_lambda" +
                        std::to_string(i + 1) + "_block" +
                        std::to_string(j + 1) + "_locs.csv",
                    block->Psi_D() * boot.w_min_by_lambda[i][j]);

          auto [w_ci_low_locs, w_ci_high_locs] =
              rgcca.bootstrap_weights_ci(h, i, j, block->Psi_D());
          write_csv(path_results + "bootstrap_weights_ci_comp" +
                        std::to_string(h + 1) + "_lambda" +
                        std::to_string(i + 1) + "_block" +
                        std::to_string(j + 1) + "_locs.csv",
                    rgcca_driver::make_ci_matrix(w_ci_low_locs, w_ci_high_locs),
                    std::vector<std::string>{"lower", "upper"});
        }
      }
    }
  }

  return 0;
}
