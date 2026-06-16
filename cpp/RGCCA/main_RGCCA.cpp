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
  
  // Save results
  int id = 1;
  for (const auto& block : rgcca.blocks()) {
    write_csv(path_results + "A"+ std::to_string(id) + "_hat_locs.csv", block -> weights_m());
    write_csv(path_results + "A_star"+ std::to_string(id) + "_hat_locs.csv", block -> weights_star_m());
    write_csv(path_results + "E"+ std::to_string(id) + "_hat_locs.csv", block -> components_m());
    id ++;
  }
  
  for(int h = 0; h < n_comp; h++) {
    write_csv(path_results + "objective"+ std::to_string(h+1) +".csv", results[h].obj_history);
  }

  return 0;
}