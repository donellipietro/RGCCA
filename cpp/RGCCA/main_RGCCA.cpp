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

matrix_t make_ci_matrix(const vector_t &lower, const vector_t &upper) {
  matrix_t ci(lower.size(), 2);
  ci.col(0) = lower;
  ci.col(1) = upper;
  return ci;
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
  
  // Extract options
  int n_obs = jroot["options"].value("n_obs", 101);
  int n_comp = jroot["options"].value("n_comp", 3);
  double tau = jroot["options"].value("tau", 0.);
  bool non_negative_weights = jroot["options"].value("non_negative_weights", false);
  
  // Options
  RGCCA<IndependentSampling>::Options options;
  options.init_strategy = InitStrategy::SVD;
  options.block_deactivation = true;
  options.connection_deactivation = true;
  options.component_significance = true;
  
  if (tau < 0.0) {
    options.mode = Mode::Regularized;
  } else if (tau > 0.5) {
    options.mode = Mode::CovMax;
  } else {
    options.mode = Mode::CorMax;
  }
  
  if(non_negative_weights) {
    options.weight_sign_constraint = WeightSignConstraint::NonNegative;
  } else {
    options.weight_sign_constraint = WeightSignConstraint::None;
  }

  // Model initialization
  RGCCA<IndependentSampling> rgcca(n_obs, options, n_comp);
  
  // Add blocks
  for (int i = 1; i <=4; ++i) {
    Eigen::Matrix<double, Dynamic, Dynamic> X = read_csv<double>(path_data + "X" + std::to_string(i) + ".csv").as_matrix();
    rgcca.add_multivariate_block("X" + std::to_string(i), std::move(X));
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
  for (const auto &block : rgcca.blocks()) {
    write_csv(path_results + "A" + std::to_string(id) + "_hat_locs.csv",
              block->weights_m());
    
    write_csv(path_results + "A_star" + std::to_string(id) + "_hat_locs.csv",
              block->weights_star_m());
    
    write_csv(path_results + "E" + std::to_string(id) + "_hat_locs.csv",
              block->components_m());
    
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
    
    write_csv(path_results + "lambda_weights" + std::to_string(h + 1) + ".csv",
              results[h].lambda_weights_values);
  }
  
  // Save bootstrap lambda-selection results
  if (options.block_deactivation || options.connection_deactivation || options.component_significance) {
    const auto &boot_results = rgcca.bootstrap_selection_results();
    
    for (std::size_t h = 0; h < boot_results.size(); ++h) {
      const auto &boot = boot_results[h];
      
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
          
          // bootstrap resamples, at locations
          write_csv(path_results + "bootstrap_weights_boot_comp" +
            std::to_string(h + 1) + "_lambda" +
            std::to_string(i + 1) + "_block" +
            std::to_string(j + 1) + "_locs.csv",
            block->Psi_D() * boot.w_boot_by_lambda[i][j]);
          
          // componentwise minimum, at locations
          write_csv(path_results + "bootstrap_weights_wmin_comp" +
            std::to_string(h + 1) + "_lambda" +
            std::to_string(i + 1) + "_block" +
            std::to_string(j + 1) + "_locs.csv",
            block->Psi_D() * boot.w_min_by_lambda[i][j]);
          
          // fdaPDE bootstrap confidence intervals, evaluated at data locations
          auto [w_ci_low_locs, w_ci_high_locs] = rgcca.bootstrap_weights_ci(h, i, j, block->Psi_D());
          write_csv(
            path_results + "bootstrap_weights_ci_comp" +
              std::to_string(h + 1) + "_lambda" + std::to_string(i + 1) +
              "_block" + std::to_string(j + 1) + "_locs.csv",
              make_ci_matrix(w_ci_low_locs, w_ci_high_locs),
              std::vector<std::string>{"lower", "upper"});
        }
      }
    }
  }
  

  return 0;
}