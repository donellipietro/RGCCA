#include <fdaPDE/models.h>
using namespace fdapde;
using fdapde::rgcca::IndependentSampling;
using fdapde::rgcca::InitStrategy;
using fdapde::rgcca::TimeDependentSampling;

#include "../include/json.hpp"
using nlohmann::json;

#include <Eigen/Dense>
#include <algorithm>
#include <cmath>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <limits>
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

  int n_obs = jroot["model_options"].value("n_obs", 101);
  int n_comp = jroot["model_options"].value("n_comp", 3);
  double tau = jroot["model_options"].value("tau", 0.);

  // Choose options
  RGCCA<TimeDependentSampling>::Options options;
  rgcca_driver::apply_rgcca_options(jroot["model_options"], options, tau);

  // Load time interval and grid
  Triangulation<1, 1> I_T(path_mesh + "knots_T.csv", true, true);
  matrix_t grid_T = read_csv<double>(path_data + "grid_T.csv").as_matrix();

  // Model initialization
  RGCCA<TimeDependentSampling> rgcca(n_obs, I_T, options, n_comp);

  // Add blocks
  Eigen::Matrix<double, Dynamic, Dynamic> times =
      read_csv<double>(path_data + "locs_T" + ".csv").as_matrix();
  std::vector<sparse_matrix_t> Psi_grid_D_vec;
  Psi_grid_D_vec.reserve(4);
  for (int i = 1; i <= 4; ++i) {

    // Load geometry
    Triangulation<2, 2> D(
        path_mesh + "points_D_" + std::to_string(i) + ".csv",
        path_mesh + "elements_D_" + std::to_string(i) + ".csv",
        path_mesh + "boundary_D_" + std::to_string(i) + ".csv", true, true);

    // Physics space (isotropic Laplacian)
    FeSpace Vh(D, P1<1>);
    TrialFunction f_D(Vh);
    TestFunction v_D(Vh);
    ZeroField<2> u_D;
    auto a_D = integral(D)(dot(grad(f_D), grad(v_D)));
    auto F_D = integral(D)(u_D * v_D);

    matrix_t grid_D =
        read_csv<double>(path_data + "grid_D_" + std::to_string(i) + ".csv")
            .as_matrix();
    Psi_grid_D_vec.push_back(internals::point_basis_eval(Vh, grid_D));

    GeoFrame gf(D);
    Eigen::Matrix<double, Dynamic, Dynamic> X =
        read_csv<double>(path_data + "X" + std::to_string(i) + ".csv")
            .as_matrix();
    auto &level = gf.insert_scalar_layer<POINT>(
        "data", path_data + "locs_D_" + std::to_string(i) + ".csv");
    level.load_blk("X" + std::to_string(i), X.transpose());
    rgcca.add_functional_block("X" + std::to_string(i), times, gf, std::move(X),
                               fe_normcovmax_elliptic(a_D, F_D));
  }

  // Set regularization
  rgcca_driver::apply_regularization_options(rgcca, jroot["model_options"],
                                             options);

  // Add connections
  rgcca_driver::apply_C_matrix(rgcca, jroot["model_options"], 4);

  // Fit
  const auto results = rgcca.fit();
  const int n_comp_effective = rgcca.n_comp_effective();
  std::cout << results << std::endl;
  write_csv(path_results + "n_comp_effective.csv",
            std::vector<double>{static_cast<double>(n_comp_effective)});

  BsSpace Bh_T(I_T, 3);
  sparse_matrix_t Psi_grid_T = internals::point_basis_eval(Bh_T, grid_T);

  // Save results
  int id = 1;
  for (const auto &block : rgcca.blocks()) {
    write_csv(path_results + "A" + std::to_string(id) + "_hat_locs.csv",
              block->weights_m().leftCols(n_comp_effective));
    write_csv(path_results + "A_star" + std::to_string(id) + "_hat_locs.csv",
              block->weights_star_m().leftCols(n_comp_effective));
    write_csv(path_results + "E" + std::to_string(id) + "_hat_locs.csv",
              block->components_m().leftCols(n_comp_effective));
    write_csv(path_results + "E" + std::to_string(id) + "_hat_grid.csv",
              Psi_grid_T * block->components().leftCols(n_comp_effective));
    write_csv(path_results + "A" + std::to_string(id) + "_hat_grid.csv",
              Psi_grid_D_vec[id - 1] *
                  block->weights().leftCols(n_comp_effective));
    write_csv(path_results + "A_star" + std::to_string(id) + "_hat_grid.csv",
              Psi_grid_D_vec[id - 1] *
                  block->weights_star().leftCols(n_comp_effective));
    id++;
  }

  for (std::size_t h = 0; h < results.size(); h++) {
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

  rgcca_driver::write_component_diagnostics(path_results, results);

  return 0;
}
