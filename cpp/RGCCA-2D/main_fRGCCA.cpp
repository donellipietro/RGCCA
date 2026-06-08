#include <fdaPDE/models.h>
using namespace fdapde;

#include "../include/json.hpp"
using nlohmann::json;

#include <Eigen/Dense>
#include <fstream>
#include <filesystem>
#include <iostream>
#include <variant>

using matrix_t = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>;
using vector_t = Eigen::Matrix<double, Eigen::Dynamic, 1>;
using sparse_matrix_t = Eigen::SparseMatrix<double>;

std::vector<std::vector<double>> parse_lambda_grid_weights(const nlohmann::json& options) {
  if (!options.contains("lambda_grid"))
    return {};
  
  const auto& jgrid = options.at("lambda_grid");
  
  if (!jgrid.is_array() || jgrid.empty())
    throw std::runtime_error("lambda_grid must be a non-empty array");
  
  // Case 1: lambda_grid = [1e-3, 1e-2]
  if (jgrid.front().is_number()) {
    return { jgrid.get<std::vector<double>>() };
  }
  
  // Case 2: lambda_grid = [[...], [...], ...]
  if (jgrid.front().is_array()) {
    return jgrid.get<std::vector<std::vector<double>>>();
  }
  
  throw std::runtime_error("lambda_grid must be either vector<double> or vector<vector<double>>");
}

int main(int argc, char* argv[]) {
  
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
  std::string path_mesh = "../../" + jroot["path_list"].value("mesh", "./mesh/");
  std::string path_data = "../../" + jroot["path_list"].value("data", "./data/");
  std::string path_results = "../../" + jroot["path_list"].value("results", "./results/");
  
  int n_obs = jroot["options"].value("n_obs", 101);
  int n_comp = jroot["options"].value("n_comp", 3);
  double sd_noise = jroot["options"].value("sd_noise", 0.);
  double tau = jroot["options"].value("tau", 0.);
  double lambda = jroot["options"].value("lambda", 1.);
  bool non_negative_weights = jroot["options"].value("non_negative_weights", false);
  bool lambda_selection_weights = jroot["options"].value("lambda_selection_weights", false);
  int n_bootstrap_samples = jroot["options"].value("n_bootstrap_samples", 10);
  int stationary_block_length = jroot["options"].value("stationary_block_length", 20);
  
  // Chose options
  RGCCA<IndependentSampling>::Options options;
  options.init_strategy = InitStrategy::Uniform;
  
  if(lambda_selection_weights) {
    options.lambda_selection_weights = LambdaSelection::Automatic;
  } else {
    options.lambda_selection_weights = LambdaSelection::Manual;
  }
  
  if (tau < 0.0) {
    options.mode = Mode::Regularized;
  } else if (tau > 0.5) {
    options.mode = Mode::CovMax;
  } else {
    options.mode = Mode::CorMax;
  }
  
  if (non_negative_weights) {
    options.weight_sign_constraint = WeightSignConstraint::NonNegative;
  } else {
    options.weight_sign_constraint = WeightSignConstraint::None;
  }
  
  // Model initialization
  RGCCA<IndependentSampling> rgcca(n_obs, options, n_comp);
  
  // Add blocks
  std::vector<sparse_matrix_t> Psi_grid_D_vec;
  Psi_grid_D_vec.reserve(4);
  for (int i = 1; i <= 4; ++i) {
    
    // Load geometry
    Triangulation<2,2> D(
        path_mesh + "points_D_" + std::to_string(i) + ".csv",
        path_mesh + "elements_D_" + std::to_string(i) + ".csv",
        path_mesh + "boundary_D_" + std::to_string(i) + ".csv",
        true, true
    );
    
    // Physics space (isotropic Laplacian)
    FeSpace Vh(D, P1<1>);
    TrialFunction f_D(Vh);
    TestFunction v_D(Vh);
    ZeroField<2> u_D;
    auto a_D = integral(D)(dot(grad(f_D), grad(v_D)));
    auto F_D = integral(D)(u_D * v_D);
    
    matrix_t grid_D = read_csv<double>(path_data + "grid_D_" + std::to_string(i) + ".csv").as_matrix();
    Psi_grid_D_vec.push_back(internals::point_basis_eval(Vh, grid_D));
    
    GeoFrame gf(D);
    Eigen::Matrix<double, Dynamic, Dynamic> X = read_csv<double>(path_data + "X" + std::to_string(i) + ".csv").as_matrix();
    auto& level = gf.insert_scalar_layer<POINT>("data", path_data + "locs_D_" + std::to_string(i) + ".csv");
    // level.load_blk("X" + std::to_string(i), X.transpose());
    rgcca.add_functional_block("X" + std::to_string(i), gf, std::move(X), fe_normcovmax_elliptic(a_D, F_D));
  }
  
  // Set lambda_l
  if(lambda_selection_weights) {
    rgcca.set_n_bootstrap_samples(n_bootstrap_samples);
    auto lambda_grid = parse_lambda_grid_weights(jroot["options"]);
    if (!lambda_grid.empty()) {
      rgcca.set_lambda_grid_weights(lambda_grid);
    }
  } else {
    rgcca.set_lambda_weights_all(lambda);
  }
  
  // resampling strategy
  if (stationary_block_length == 0) {
    rgcca.set_resampling_strategy(ResamplingStrategy::Ordinary);
  } else {
    rgcca.set_resampling_strategy(ResamplingStrategy::Stationary);
    rgcca.set_stationary_block_length(stationary_block_length);
  }
  
  // Add connections
  rgcca.connect(0,1);
  rgcca.connect(0,2);
  rgcca.connect(0,3);
  rgcca.connect(1,3);
  rgcca.connect(2,3);
  
  // Fit
  const auto results = rgcca.fit();
  std::cout << results << std::endl;
  
  // Save fitted results
  int id = 1;
  for (int j=0; j<rgcca.blocks().size(); j++) {
    const auto& block = rgcca.blocks()[j];
    write_csv(path_results + "A" + std::to_string(j+1) + "_hat_locs.csv",
              block->weights_m());
    
    write_csv(path_results + "A_star" + std::to_string(j+1) + "_hat_locs.csv",
              block->weights_star_m());
    
    write_csv(path_results + "E" + std::to_string(j+1) + "_hat_locs.csv",
              block->components_m());
    
    write_csv(path_results + "A" + std::to_string(j+1) + "_hat_grid.csv",
              Psi_grid_D_vec[j] * block->weights());
    
    write_csv(path_results + "A_star" + std::to_string(j+1) + "_hat_grid.csv",
              Psi_grid_D_vec[j] * block->weights_star());
  }
  
  // Save component-wise fit info
  for (int h = 0; h < n_comp; ++h) {
    write_csv(path_results + "objective" + std::to_string(h + 1) + ".csv",
              results[h].obj_history);
    
    write_csv(path_results + "covariance_matrix" + std::to_string(h + 1) + ".csv",
              results[h].covariance_matrix);
    
    write_csv(path_results + "tau" + std::to_string(h + 1) + ".csv",
              results[h].tau_values);
    
    write_csv(path_results + "lambda_components" + std::to_string(h + 1) + ".csv",
              results[h].lambda_components_values);
    
    write_csv(path_results + "lambda_weights" + std::to_string(h + 1) + ".csv",
              results[h].lambda_weights_values);
  }
  
  // Save bootstrap lambda-selection results
  if (lambda_selection_weights) {
    const auto& boot_results = rgcca.bootstrap_selection_results();
    
    for (std::size_t h = 0; h < boot_results.size(); ++h) {
      const auto& boot = boot_results[h];
      
      write_csv(path_results + "bootstrap_lambda_grid" + std::to_string(h + 1) + ".csv",
                boot.lambda_grid);
      
      write_csv(path_results + "bootstrap_criterion" + std::to_string(h + 1) + ".csv",
                boot.criterion);
      
      write_csv(path_results + "bootstrap_lambda_opt" + std::to_string(h + 1) + ".csv",
                std::vector<double>{boot.lambda_opt});
      
      for (std::size_t i = 0; i < boot.lambda_grid.size(); ++i) {
        for (std::size_t j = 0; j < boot.w_fit_by_lambda[i].size(); ++j) {
          
          const auto& block = rgcca.blocks()[j];
          
          // fit on real data, at locations
          write_csv(
            path_results
            + "bootstrap_weights_fit_comp" + std::to_string(h + 1)
            + "_lambda" + std::to_string(i + 1)
            + "_block" + std::to_string(j + 1)
            + "_locs.csv",
            block->Psi_D() * boot.w_fit_by_lambda[i][j]
          );
          
          // bootstrap resamples, at locations
          write_csv(
            path_results
            + "bootstrap_weights_boot_comp" + std::to_string(h + 1)
            + "_lambda" + std::to_string(i + 1)
            + "_block" + std::to_string(j + 1)
            + "_locs.csv",
            block->Psi_D() * boot.w_boot_by_lambda[i][j]
          );
          
          // componentwise minimum, at locations
          write_csv(
            path_results
            + "bootstrap_weights_wmin_comp" + std::to_string(h + 1)
            + "_lambda" + std::to_string(i + 1)
            + "_block" + std::to_string(j + 1)
            + "_locs.csv",
            block->Psi_D() * boot.w_min_by_lambda[i][j]
          );
          
          // fit on real data, at grid
          write_csv(
            path_results
            + "bootstrap_weights_fit_comp" + std::to_string(h + 1)
            + "_lambda" + std::to_string(i + 1)
            + "_block" + std::to_string(j + 1)
            + "_grid.csv",
            Psi_grid_D_vec[j] * boot.w_fit_by_lambda[i][j]
          );
          
          // bootstrap resamples, at grid
          write_csv(
            path_results
            + "bootstrap_weights_boot_comp" + std::to_string(h + 1)
            + "_lambda" + std::to_string(i + 1)
            + "_block" + std::to_string(j + 1)
            + "_grid.csv",
            Psi_grid_D_vec[j] * boot.w_boot_by_lambda[i][j]
          );
          
          // componentwise minimum, at grid
          write_csv(
            path_results
            + "bootstrap_weights_wmin_comp" + std::to_string(h + 1)
            + "_lambda" + std::to_string(i + 1)
            + "_block" + std::to_string(j + 1)
            + "_grid.csv",
            Psi_grid_D_vec[j] * boot.w_min_by_lambda[i][j]
          );
        }
      }
    }
  }
  return 0;
}