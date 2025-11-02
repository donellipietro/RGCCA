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
auto fit_model(Graph G,
               GraphTriangulation<1> GT,
               const vector_t& z,
               const std::vector<double>& lambda_grid,
               const std::string& trace_mode,
               const std::string& path_results) {
  
  std::cout << "Fit: 1D-graph" << std::endl;
  std::cout << "- Lambda grid has " << lambda_grid.size() << " values.\n";
  std::cout << std::endl;
  
  // Physics (isotropic Laplacian)
  GraphSpace Gh(G);
  TrialFunction f(Gh);
  TestFunction v(Gh);
  ZeroField<1> u;
  auto a = integral(G)(lapG(f) * lapG(v));
  auto F = integral(G)(u * v);
  
  // GeoFrame
  GeoFrame data(GT);
  auto& l = data.insert_scalar_layer<POINT>("l", MESH_NODES);
  l.load_blk("z", z);
  
  // Initialize the model
  SRPDE model("z ~ f", data, gr_ls_elliptic(a, F));
  if(trace_mode == "Exact") model.set_trace_mode(TraceMode::Exact);
  else model.set_trace_mode(TraceMode::Hutchinson);
  
  // Model Calibration
  GridSearch<1> optimizer;
  vector_t lambda_opt(1); lambda_opt[0] = lambda_grid[0];
  std::vector<double> gcv_scores(1);
  if(lambda_grid.size() > 1) {
    optimizer.optimize(model.gcv(100, 476813), lambda_grid);
    lambda_opt[0] = optimizer.optimum()[0];
    gcv_scores = optimizer.values();
  } 
  
  // Final fit
  model.fit(lambda_opt);
  
  // Save results ----
  write_csv(path_results + "f_coeffs.csv", model.f());
  write_csv(path_results + "f_locs.csv", model.f());
  write_csv(path_results + "gcv_scores.csv", gcv_scores);
  write_csv(path_results + "lambda.csv", lambda_opt);
  
  // Ensure the file ends with a newline (optional cleanup)
  std::ofstream fix_newline(path_results + "gcv_scores.csv", std::ios::app);
  fix_newline << std::endl;
  
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
  std::cout << "Reading parameters from: " << params_path << std::endl;
  
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
  
  std::cout << std::endl;
  std::cout << "Paths:" << std::endl;
  std::cout << "- Mesh: " << path_mesh << std::endl;
  std::cout << "- Data: " << path_data << std::endl;
  std::cout << "- Results: " << path_results << std::endl;
  std::cout << std::endl;
  
  // Extract options
  std::string calibration = jroot["options"].value("calibration", "none");
  std::vector<double> lambda_grid;
  if (calibration == "none") {
    double lambda = jroot["options"].value("lambda_grid", 1e-4);
    lambda_grid.push_back(lambda);
  } else {
    lambda_grid = jroot["options"].at("lambda_grid").get<std::vector<double>>();
  }
  std::string trace_mode = jroot["options"].value("trace_mode", "Exact");
  
  std::cout << "Options:" << std::endl;
  std::cout << "- Calibration: " << calibration << std::endl;
  if (calibration == "none") {
    std::cout << "- Lambda : " << lambda_grid[0] << std::endl;
  } else {
    std::cout << "- Lambda grid: [" << lambda_grid[0] << " " << lambda_grid[1] << " ... " << lambda_grid.back() << "]" << std::endl;
  }
  std::cout << "- Trace mode: " << trace_mode << std::endl;
  std::cout << std::endl;
  
  // Load geometry
  vector_t nodes = read_csv<double>(path_mesh + "points.csv").as_matrix();
  Graph G = Graph::Path(nodes.size());
  GraphTriangulation GT = GraphTriangulation<1>::FromGraph(G, nodes);
  
  // Load data
  vector_t z = read_csv<double>(path_data + "z.csv").as_matrix();
  
  std::cout << "Loaded data:" << std::endl;
  std::cout << "- z(" << z.size() << ")" << std::endl;
  std::cout << std::endl;
  
  if(z.size() != nodes.size()) {
    throw std::runtime_error("GraphTriangulation does not accepts sampling at locations (nodes!=locs)");
  }
  
  // Fit the model
  fit_model(G, GT, z, lambda_grid, trace_mode, path_results);
  
  std::cout << "Results:" << std::endl;
  std::cout << "- Results written to: " << path_results << "output.csv\n";
  std::cout << std::endl;
  
  return 0;
}