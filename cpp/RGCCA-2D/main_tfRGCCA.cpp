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
auto fit_model(Triangulation<1,1> I_T,
               int n_obs,
               int n_comp,
               double sd_noise,
               double tau,
               const matrix_t& grid_T,
               // const std::vector<double>& lambda_grid,
               const std::string& path_mesh,
               const std::string& path_data,
               const std::string& path_results) {
  
  // std::cout << "Fit: tfRGCCA" << std::endl;
  // std::cout << "- n_obs: " << n_obs << std::endl;
  // std::cout << "- n_comp: " << n_comp << std::endl;
  // std::cout << "- sd_noise: " << sd_noise << std::endl;
  // std::cout << "- Lambda grid has " << lambda_grid.size() << " values.\n";
  // std::cout << std::endl;

  // Chose options
  RGCCA<TimeDependentSampling>::Options options;
  options.scheme = Scheme::Factorial();
  
  options.flip_and_scale = false;
  options.bias = true;
  
  if(tau < 0) options.tau_selection = TauSelection::Automatic;
  else options.tau_selection = TauSelection::Manual;
  
  // Model initialization
  RGCCA<TimeDependentSampling> rgcca(n_obs, I_T, options, n_comp);

  // Set empirical noise variance
  // if(sd_noise > 0){
  //   rgcca.set_noise_variance(sd_noise*sd_noise);
  //   options.allow_blocks_deactivation = true; // default
  // } else {
  options.allow_blocks_deactivation = false;
  // }
  
  // Add blocks
  Eigen::Matrix<double, Dynamic, Dynamic> times = read_csv<double>(path_data + "locs_T" +".csv").as_matrix();
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
    level.load_blk("X" + std::to_string(i), X.transpose());
    rgcca.add_functional_block("X" + std::to_string(i), times, gf, fe_ls_elliptic(a_D, F_D), tau);
    
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
  BsSpace Bh_T(I_T, 3);
  sparse_matrix_t Psi_grid_T = internals::point_basis_eval(Bh_T, grid_T);
  
  // Save results
  int id = 1;
  for (const auto& block : rgcca.blocks()) {
    write_csv(path_results + "A"+ std::to_string(id) + "_hat_locs.csv", block -> loadings_m());
    write_csv(path_results + "E"+ std::to_string(id) + "_hat_locs.csv", block -> components_m());
    write_csv(path_results + "A"+ std::to_string(id) + "_hat_grid.csv", Psi_grid_D_vec[id-1] * block -> loadings());
    write_csv(path_results + "E"+ std::to_string(id) + "_hat_grid.csv", Psi_grid_T * block -> components());
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
  // std::string calibration = jroot["options"].value("calibration", "none");
  // std::vector<double> lambda_grid;
  // if (calibration == "none") {
  //   double lambda = jroot["options"].value("lambda_grid", 1e-4);
  //   lambda_grid.push_back(lambda);
  // } else {
  //   lambda_grid = jroot["options"].at("lambda_grid").get<std::vector<double>>();
  // }
  int n_obs = jroot["options"].value("n_obs", 101);
  int n_comp = jroot["options"].value("n_comp", 3);
  double sd_noise = jroot["options"].value("sd_noise", 0.);
  double tau = jroot["options"].value("tau", 0.);
  
  // std::cout << "Options:" << std::endl;
  // std::cout << "- n_obs: " << n_obs << std::endl;
  // std::cout << "- n_comp: " << n_comp << std::endl;
  // std::cout << "- sd_noise: " << sd_noise << std::endl;
  /*
  if (calibration == "none") {
    std::cout << "- Lambda : " << lambda_grid[0] << std::endl;
  } else {
    std::cout << "- Lambda grid: [" << lambda_grid[0] << " " << lambda_grid[1] << " ... " << lambda_grid.back() << "]" << std::endl;
  }
  std::cout << "- Trace mode: " << trace_mode << std::endl;
  std::cout << std::endl;
   */
  
  // Load geometry
  Triangulation<1,1> I_T(path_mesh + "knots_T.csv", true, true);
  
  // Load data
  matrix_t grid_T = read_csv<double>(path_data + "grid_T.csv").as_matrix();
  
  // std::cout << "Loaded data:" << std::endl;
  // std::cout << "- z(" << z.size() << ")" << std::endl;
  // std::cout << "- grid_T(" << grid_T.rows() << ", " << grid_T.cols() << ")" << std::endl;
  // std::cout << std::endl;
  
  // Fit the model
  fit_model(I_T, n_obs, n_comp, sd_noise, tau, grid_T, path_mesh, path_data, path_results);
  
  // std::cout << "Results:" << std::endl;
  // std::cout << "- Results written to: " << path_results << "output.csv\n";
  // std::cout << std::endl;
  
  return 0;
}