//
// TEMPLATE: Minimal R ↔ C++ JSON interface
// Reads paths and options, loads data, calls `fit_model`, saves output
//

#include <fdaPDE/models.h>
using namespace fdapde;

#include "../include/json.hpp"
using nlohmann::json;

#include <Eigen/Dense>
#include <fstream>
#include <filesystem>
#include <iostream>
#include <variant>

// Type aliases
using matrix_t = Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>;
using vector_t = Eigen::Matrix<double, Eigen::Dynamic, 1>;
using sparse_matrix_t = Eigen::SparseMatrix<double>;

// Example function
auto fit_model(int n_obs,
               int n_comp,
               double sd_noise,
               double tau,
               const std::string& path_data,
               const std::string& path_results) {
  
  // std::cout << "Fit: RGCCA" << std::endl;
  // std::cout << "- n_obs: " << n_obs << std::endl;
  // std::cout << "- n_comp: " << n_comp << std::endl;
  // std::cout << "- sd_noise: " << sd_noise << std::endl;
  // std::cout << std::endl;
  
  // Chose options
  RGCCA<IndependentSampling>::Options options;
  options.scheme = Scheme::Factorial();
  
  options.flip_and_scale = false;
  options.bias = true;
  
  if(tau < 0) options.tau_selection = TauSelection::Automatic;
  else options.tau_selection = TauSelection::Manual;
  
  // Model initialization
  RGCCA<IndependentSampling> rgcca(n_obs, options, n_comp);
  
  // Set empirical noise variance
  if(sd_noise > 0){
    rgcca.set_noise_variance(sd_noise*sd_noise);
    options.allow_blocks_deactivation = true; // default
  } else {
    options.allow_blocks_deactivation = false;
  }
  
  // Add blocks
  for (int i = 1; i <=4; ++i) {
    Eigen::Matrix<double, Dynamic, Dynamic> X = read_csv<double>(path_data + "X" + std::to_string(i) + ".csv").as_matrix();
    rgcca.add_multivariate_block("X" + std::to_string(i), X, tau);
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
    write_csv(path_results + "A"+ std::to_string(id) + "_hat_locs.csv", block -> loadings_m());
    write_csv(path_results + "E"+ std::to_string(id) + "_hat_locs.csv", block -> components_m());
    id ++;
  }
  
  for(int h = 0; h < n_comp; h++) {
    write_csv(path_results + "objective"+ std::to_string(h+1) +".csv", results[h].obj_history);
  }
  
  return 1;
}

// Main
int main(int argc, char* argv[]) {
  
  std::cout << std::endl;
  
  // Check for argument
  if (argc < 2) {
    std::cerr << "Usage: " << argv[0] << " <params.json>" << std::endl;
    return 1;
  }
  
  std::string params_path = argv[1];
  // std::cout << "Reading parameters from: " << params_path << std::endl;
  
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
  
  // std::cout << std::endl;
  // std::cout << "Paths:" << std::endl;
  // std::cout << "- Mesh: " << path_mesh << std::endl;
  // std::cout << "- Data: " << path_data << std::endl;
  // std::cout << "- Results: " << path_results << std::endl;
  // std::cout << std::endl;
  
  // Extract options
  int n_obs = jroot["options"].value("n_obs", 101);
  int n_comp = jroot["options"].value("n_comp", 3);
  double sd_noise = jroot["options"].value("sd_noise", 0.);
  double tau = jroot["options"].value("tau", 0.);
  
  // std::cout << "Options:" << std::endl;
  // std::cout << "- n_obs: " << n_obs << std::endl;
  // std::cout << "- n_comp: " << n_comp << std::endl;
  // std::cout << "- sd_noise: " << sd_noise << std::endl;
  
  // Fit the model
  fit_model(n_obs, n_comp, sd_noise, tau, path_data, path_results);
  
  // Results
  // std::cout << "Results:" << std::endl;
  // std::cout << "- Results written to: " << path_results << "output.csv\n";
  // std::cout << std::endl;
  
  return 0;
}