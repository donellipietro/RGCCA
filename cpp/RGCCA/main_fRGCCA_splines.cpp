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

  std::cout << std::endl;

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

  int n_obs = jroot["model_options"].value("n_obs", 101);
  int n_comp = jroot["model_options"].value("n_comp", 3);
  double tau = jroot["model_options"].value("tau", 0.);

  // Load geometry
  Triangulation<1, 1> I_D(path_mesh + "knots_D.csv", true, true);

  // Load data
  matrix_t grid_D = read_csv<double>(path_data + "grid_D.csv").as_matrix();

  // Physics (isotropic Laplacian)
  BsSpace Bh(I_D, 3);
  TrialFunction f_D(Bh);
  TestFunction v_D(Bh);
  ZeroField<1> u_D;
  auto a_D = integral(I_D)(dxx(f_D) * dxx(v_D));
  auto F_D = integral(I_D)(u_D * v_D);

  // Choose options
  RGCCA<IndependentSampling>::Options options;
  options.init_strategy = InitStrategy::Uniform;
  rgcca_driver::apply_rgcca_options(jroot["model_options"], options, tau);

  // Bootstrap
  RGCCA<IndependentSampling>::BootstrapConfig bootstrap_config;
  bootstrap_config.patience = 1;
  bootstrap_config.check_every = 5;
  bootstrap_config.stable_checks_required = 3;
  bootstrap_config.active_block_tol = 1e-3;
  rgcca_driver::apply_bootstrap_options(jroot["bootstrap_options"],
                                        bootstrap_config);

  // Model initialization
  RGCCA<IndependentSampling> rgcca(n_obs, options, n_comp);

  // Add blocks
  for (int i = 1; i <= 4; ++i) {
    GeoFrame gf(I_D);
    Eigen::Matrix<double, Dynamic, Dynamic> X =
        read_csv<double>(path_data + "X" + std::to_string(i) + ".csv")
            .as_matrix();
    auto &level =
        gf.insert_scalar_layer<POINT>("data", path_data + "locs_D" + ".csv");
    level.load_blk("X" + std::to_string(i), X.transpose());
    rgcca.add_functional_block("X" + std::to_string(i), gf, std::move(X),
                               bs_normcovmax_elliptic(a_D, F_D));
  }

  // Set regularization
  rgcca_driver::apply_regularization_options(rgcca, jroot["model_options"],
                                             options);

  // Bootstrap
  rgcca.set_bootstrap_config(bootstrap_config);

  // Add connections
  rgcca_driver::apply_C_matrix(rgcca, jroot["model_options"], 4);

  // Fit
  const auto results = rgcca.fit();
  std::cout << results << std::endl;
  sparse_matrix_t Psi_grid = internals::point_basis_eval(Bh, grid_D);
  ;

  // Save fitted results
  int id = 1;
  for (const auto &block : rgcca.blocks()) {
    write_csv(path_results + "A" + std::to_string(id) + "_hat_locs.csv",
              block->weights_m());

    write_csv(path_results + "A_star" + std::to_string(id) + "_hat_locs.csv",
              block->weights_star_m());

    write_csv(path_results + "E" + std::to_string(id) + "_hat_locs.csv",
              block->components_m());

    write_csv(path_results + "A" + std::to_string(id) + "_hat_grid.csv",
              Psi_grid * block->weights());

    write_csv(path_results + "A_star" + std::to_string(id) + "_hat_grid.csv",
              Psi_grid * block->weights_star());

    ++id;
  }

  // Save component-wise fit info
  for (int h = 0; h < n_comp; ++h) {
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

  // Save bootstrap lambda-selection results
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

          // fit on real data, at locations
          write_csv(path_results + "bootstrap_weights_fit_comp" +
                        std::to_string(h + 1) + "_lambda" +
                        std::to_string(i + 1) + "_block" +
                        std::to_string(j + 1) + "_locs.csv",
                    block->Psi_D() * boot.w_fit_by_lambda[i][j]);

          if (save_resamples) {
            write_csv(path_results + "bootstrap_weights_boot_comp" +
                          std::to_string(h + 1) + "_lambda" +
                          std::to_string(i + 1) + "_block" +
                          std::to_string(j + 1) + "_locs.csv",
                      block->Psi_D() * boot.w_boot_by_lambda[i][j]);
          }

          // componentwise minimum, at locations
          write_csv(path_results + "bootstrap_weights_wmin_comp" +
                        std::to_string(h + 1) + "_lambda" +
                        std::to_string(i + 1) + "_block" +
                        std::to_string(j + 1) + "_locs.csv",
                    block->Psi_D() * boot.w_min_by_lambda[i][j]);

          // fit on real data, at grid
          write_csv(path_results + "bootstrap_weights_fit_comp" +
                        std::to_string(h + 1) + "_lambda" +
                        std::to_string(i + 1) + "_block" +
                        std::to_string(j + 1) + "_grid.csv",
                    Psi_grid * boot.w_fit_by_lambda[i][j]);

          if (save_resamples) {
            write_csv(path_results + "bootstrap_weights_boot_comp" +
                          std::to_string(h + 1) + "_lambda" +
                          std::to_string(i + 1) + "_block" +
                          std::to_string(j + 1) + "_grid.csv",
                      Psi_grid * boot.w_boot_by_lambda[i][j]);
          }
          // componentwise minimum, at grid
          write_csv(path_results + "bootstrap_weights_wmin_comp" +
                        std::to_string(h + 1) + "_lambda" +
                        std::to_string(i + 1) + "_block" +
                        std::to_string(j + 1) + "_grid.csv",
                    Psi_grid * boot.w_min_by_lambda[i][j]);

          auto [w_ci_low_locs, w_ci_high_locs] =
              rgcca.bootstrap_weights_ci(h, i, j, block->Psi_D());
          write_csv(path_results + "bootstrap_weights_ci_comp" +
                        std::to_string(h + 1) + "_lambda" +
                        std::to_string(i + 1) + "_block" +
                        std::to_string(j + 1) + "_locs.csv",
                    rgcca_driver::make_ci_matrix(w_ci_low_locs, w_ci_high_locs),
                    std::vector<std::string>{"lower", "upper"});

          auto [w_ci_low_grid, w_ci_high_grid] =
              rgcca.bootstrap_weights_ci(h, i, j, Psi_grid);
          write_csv(path_results + "bootstrap_weights_ci_comp" +
                        std::to_string(h + 1) + "_lambda" +
                        std::to_string(i + 1) + "_block" +
                        std::to_string(j + 1) + "_grid.csv",
                    rgcca_driver::make_ci_matrix(w_ci_low_grid, w_ci_high_grid),
                    std::vector<std::string>{"lower", "upper"});
        }
      }
    }
  }

  return 0;
}
