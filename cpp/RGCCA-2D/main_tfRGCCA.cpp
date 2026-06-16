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
  
  int n_obs = jroot["options"].value("n_obs", 101);
  int n_comp = jroot["options"].value("n_comp", 3);
  double sd_noise = jroot["options"].value("sd_noise", 0.);
  double tau = jroot["options"].value("tau", 0.);
  double lambda = jroot["options"].value("lambda", 1.);
  bool non_negative_weights = jroot["options"].value("non_negative_weights", false);
  
  // Chose options
  RGCCA<TimeDependentSampling>::Options options;
  
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
  
  // Load time interval and grid
  Triangulation<1,1> I_T(path_mesh + "knots_T.csv", true, true);
  matrix_t grid_T = read_csv<double>(path_data + "grid_T.csv").as_matrix();
  
  
  // Model initialization
  RGCCA<TimeDependentSampling> rgcca(n_obs, I_T, options, n_comp);
  
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
    rgcca.add_functional_block("X" + std::to_string(i), times, gf, std::move(X), fe_normcovmax_elliptic(a_D, F_D));
    
  }
  
  // Set lambda_l
  rgcca.set_lambda_weights_all(lambda);
  
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
    write_csv(path_results + "A"+ std::to_string(id) + "_hat_locs.csv", block -> weights_m());
    write_csv(path_results + "A_star"+ std::to_string(id) + "_hat_locs.csv", block -> weights_star_m());
    write_csv(path_results + "E"+ std::to_string(id) + "_hat_locs.csv", block -> components_m());
    write_csv(path_results + "E"+ std::to_string(id) + "_hat_grid.csv", Psi_grid_T * block -> components());
    write_csv(path_results + "A"+ std::to_string(id) + "_hat_grid.csv", Psi_grid_D_vec[id-1] * block -> weights());
    write_csv(path_results + "A_star"+ std::to_string(id) + "_hat_grid.csv", Psi_grid_D_vec[id-1] * block -> weights_star());
    id ++;
  }
  
  for(int h = 0; h < n_comp; h++) {
    write_csv(path_results + "objective"+ std::to_string(h+1) +".csv", results[h].obj_history);
  }
  
  return 0;
}