#ifndef RGCCA_DRIVER_OPTIONS_HPP
#define RGCCA_DRIVER_OPTIONS_HPP

#include "json.hpp"

#include <Eigen/Dense>
#include <algorithm>
#include <cctype>
#include <cmath>
#include <filesystem>
#include <initializer_list>
#include <limits>
#include <stdexcept>
#include <string>
#include <vector>

namespace rgcca_driver {

using json = nlohmann::json;

inline std::string resolve_path(const std::string &path) {
  if (std::filesystem::path(path).is_absolute()) {
    return path;
  }
  return "../../" + path;
}

inline Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>
make_ci_matrix(const Eigen::Matrix<double, Eigen::Dynamic, 1> &lower,
               const Eigen::Matrix<double, Eigen::Dynamic, 1> &upper) {
  Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> ci(lower.size(), 2);
  ci.col(0) = lower;
  ci.col(1) = upper;
  return ci;
}

inline std::string normalize_token(std::string value) {
  std::string out;
  out.reserve(value.size());
  for (unsigned char ch : value) {
    if (ch == '_' || ch == '-' || std::isspace(ch)) {
      continue;
    }
    out.push_back(static_cast<char>(std::tolower(ch)));
  }
  return out;
}

inline bool is_default_token(const std::string &value) {
  const auto token = normalize_token(value);
  return token.empty() || token == "default";
}

inline const json *find_option(const json &options,
                               std::initializer_list<const char *> names) {
  for (const char *name : names) {
    if (options.contains(name) && !options.at(name).is_null()) {
      return &options.at(name);
    }
  }
  return nullptr;
}

inline bool has_option(const json &options,
                       std::initializer_list<const char *> names) {
  return find_option(options, names) != nullptr;
}

template <typename T>
T read_number_option(const json &options,
                     std::initializer_list<const char *> names,
                     const T default_value) {
  const json *value = find_option(options, names);
  if (!value) {
    return default_value;
  }
  return value->get<T>();
}

inline bool read_bool_value(const json &value, const std::string &name) {
  if (value.is_boolean()) {
    return value.get<bool>();
  }
  if (value.is_number_integer() || value.is_number_unsigned()) {
    return value.get<int>() != 0;
  }
  if (value.is_string()) {
    const auto token = normalize_token(value.get<std::string>());
    if (token == "true" || token == "t" || token == "yes" || token == "y" ||
        token == "1" || token == "auto" || token == "automatic") {
      return true;
    }
    if (token == "false" || token == "f" || token == "no" || token == "n" ||
        token == "0" || token == "manual" || token == "none") {
      return false;
    }
  }
  throw std::invalid_argument("Invalid boolean option: " + name);
}

inline bool read_bool_option(const json &options,
                             std::initializer_list<const char *> names,
                             const bool default_value) {
  const json *value = find_option(options, names);
  if (!value) {
    return default_value;
  }
  return read_bool_value(*value, *names.begin());
}

inline std::string read_string_value(const json &value,
                                     const std::string &name) {
  if (!value.is_string()) {
    throw std::invalid_argument("Option must be a string: " + name);
  }
  return value.get<std::string>();
}

inline fdapde::rgcca::InitStrategy
parse_init_strategy(const std::string &value,
                    const fdapde::rgcca::InitStrategy current) {
  if (is_default_token(value)) {
    return current;
  }
  const auto token = normalize_token(value);
  if (token == "none") {
    return fdapde::rgcca::InitStrategy::None;
  }
  if (token == "svd") {
    return fdapde::rgcca::InitStrategy::SVD;
  }
  if (token == "uniform") {
    return fdapde::rgcca::InitStrategy::Uniform;
  }
  if (token == "warmstart") {
    return fdapde::rgcca::InitStrategy::WarmStart;
  }
  throw std::invalid_argument("Unknown init_strategy: " + value);
}

inline fdapde::rgcca::LambdaSelection
parse_lambda_selection(const json &value, const fdapde::rgcca::LambdaSelection current,
                       const std::string &name) {
  if (value.is_boolean() || value.is_number_integer() ||
      value.is_number_unsigned()) {
    return read_bool_value(value, name) ? fdapde::rgcca::LambdaSelection::Automatic
                                        : fdapde::rgcca::LambdaSelection::Manual;
  }
  const auto raw = read_string_value(value, name);
  if (is_default_token(raw)) {
    return current;
  }
  const auto token = normalize_token(raw);
  if (token == "manual") {
    return fdapde::rgcca::LambdaSelection::Manual;
  }
  if (token == "auto" || token == "automatic") {
    return fdapde::rgcca::LambdaSelection::Automatic;
  }
  throw std::invalid_argument("Unknown lambda selection: " + raw);
}

inline fdapde::rgcca::Mode mode_from_tau(const double tau) {
  if (tau < 0.0) {
    return fdapde::rgcca::Mode::Regularized;
  }
  if (tau > 0.5) {
    return fdapde::rgcca::Mode::CovMax;
  }
  return fdapde::rgcca::Mode::CorMax;
}

inline fdapde::rgcca::Mode parse_mode(const std::string &value,
                               const fdapde::rgcca::Mode current) {
  if (is_default_token(value)) {
    return current;
  }
  const auto token = normalize_token(value);
  if (token == "cor" || token == "cormax" || token == "correlation") {
    return fdapde::rgcca::Mode::CorMax;
  }
  if (token == "regularized" || token == "rgcca" || token == "reg") {
    return fdapde::rgcca::Mode::Regularized;
  }
  if (token == "cov" || token == "covmax" || token == "covariance") {
    return fdapde::rgcca::Mode::CovMax;
  }
  throw std::invalid_argument("Unknown mode: " + value);
}

inline fdapde::rgcca::WeightSignConstraint
parse_weight_sign_constraint(const std::string &value,
                             const fdapde::rgcca::WeightSignConstraint current) {
  if (is_default_token(value)) {
    return current;
  }
  const auto token = normalize_token(value);
  if (token == "none") {
    return fdapde::rgcca::WeightSignConstraint::None;
  }
  if (token == "nonnegative" || token == "nn") {
    return fdapde::rgcca::WeightSignConstraint::NonNegative;
  }
  throw std::invalid_argument("Unknown weight_sign_constraint: " + value);
}

inline fdapde::rgcca::Deflation parse_deflation(const std::string &value,
                                         const fdapde::rgcca::Deflation current) {
  if (is_default_token(value)) {
    return current;
  }
  const auto token = normalize_token(value);
  if (token == "none") {
    return fdapde::rgcca::Deflation::None;
  }
  if (token == "scores" || token == "score") {
    return fdapde::rgcca::Deflation::Scores;
  }
  throw std::invalid_argument("Unknown deflation_mode: " + value);
}

inline fdapde::rgcca::Scheme parse_scheme(const std::string &value,
                                   const fdapde::rgcca::Scheme &current) {
  if (is_default_token(value)) {
    return current;
  }
  const auto token = normalize_token(value);
  if (token == "horst") {
    return fdapde::rgcca::Scheme::Horst();
  }
  if (token == "centroid") {
    return fdapde::rgcca::Scheme::Centroid();
  }
  if (token == "factorial") {
    return fdapde::rgcca::Scheme::Factorial();
  }
  throw std::invalid_argument("Unknown scheme: " + value);
}

inline fdapde::rgcca::ResamplingStrategy
parse_resampling_strategy(const std::string &value,
                          const fdapde::rgcca::ResamplingStrategy current) {
  if (is_default_token(value)) {
    return current;
  }
  const auto token = normalize_token(value);
  if (token == "ordinary") {
    return fdapde::rgcca::ResamplingStrategy::Ordinary;
  }
  if (token == "stationary") {
    return fdapde::rgcca::ResamplingStrategy::Stationary;
  }
  throw std::invalid_argument("Unknown resampling_strategy: " + value);
}

template <typename Options>
void apply_rgcca_options(const json &joptions, Options &options,
                         const double tau) {
  options.max_iter =
      read_number_option<int>(joptions, {"max_iter"}, options.max_iter);
  options.tol = read_number_option<double>(joptions, {"tol"}, options.tol);
  options.cache_covariances = read_bool_option(joptions, {"cache_covariances"},
                                               options.cache_covariances);
  options.bias = read_bool_option(joptions, {"bias"}, options.bias);

  if (const json *value = find_option(joptions, {"init_strategy"})) {
    options.init_strategy = parse_init_strategy(
        read_string_value(*value, "init_strategy"), options.init_strategy);
  }

  if (const json *value = find_option(joptions, {"lambda_selection_weights"})) {
    options.lambda_selection_weights = parse_lambda_selection(
        *value, options.lambda_selection_weights, "lambda_selection_weights");
  }
  if (const json *value =
          find_option(joptions, {"lambda_selection_components"})) {
    options.lambda_selection_components =
        parse_lambda_selection(*value, options.lambda_selection_components,
                               "lambda_selection_components");
  }

  options.component_significance = read_bool_option(
      joptions, {"component_significance"}, options.component_significance);
  options.block_importance =
      read_bool_option(joptions, {"block_importance"}, options.block_importance);
  options.inactive_block_signal_test =
      read_bool_option(joptions, {"inactive_block_signal_test"},
                       options.inactive_block_signal_test);
  options.block_deactivation = read_bool_option(
      joptions, {"block_deactivation"}, options.block_deactivation);
  options.connection_deactivation = read_bool_option(
      joptions, {"connection_deactivation"}, options.connection_deactivation);

  options.mode = mode_from_tau(tau);
  if (const json *value = find_option(joptions, {"mode"})) {
    options.mode = parse_mode(read_string_value(*value, "mode"), options.mode);
  }

  const bool non_negative_weights =
      read_bool_option(joptions, {"non_negative_weights"}, false);
  options.weight_sign_constraint =
      non_negative_weights ? fdapde::rgcca::WeightSignConstraint::NonNegative
                           : fdapde::rgcca::WeightSignConstraint::None;
  if (const json *value = find_option(joptions, {"weight_sign_constraint"})) {
    const auto raw = read_string_value(*value, "weight_sign_constraint");
    if (!is_default_token(raw)) {
      options.weight_sign_constraint =
          parse_weight_sign_constraint(raw, options.weight_sign_constraint);
    }
  }

  if (const json *value = find_option(joptions, {"deflation_mode"})) {
    options.deflation_mode = parse_deflation(
        read_string_value(*value, "deflation_mode"), options.deflation_mode);
  }
  if (const json *value = find_option(joptions, {"scheme"})) {
    options.scheme =
        parse_scheme(read_string_value(*value, "scheme"), options.scheme);
  }
}

template <typename BootstrapConfig>
void apply_bootstrap_options(const json &joptions, BootstrapConfig &config) {
  config.seed = read_number_option<unsigned>(joptions, {"seed"}, config.seed);
  config.max_threads =
      read_number_option<int>(joptions, {"max_threads"}, config.max_threads);

  const bool explicit_B_min = has_option(joptions, {"B_min"});
  config.B_min = read_number_option<int>(joptions, {"B_min"}, config.B_min);
  config.B_max = read_number_option<int>(joptions, {"B_max"}, config.B_max);
  config.check_every =
      read_number_option<int>(joptions, {"check_every"}, config.check_every);
  config.check_every_block_deactivation = read_number_option<int>(
      joptions, {"check_every_block_deactivation"},
      config.check_every_block_deactivation);
  config.check_every_connection_deactivation = read_number_option<int>(
      joptions, {"check_every_connection_deactivation"},
      config.check_every_connection_deactivation);
  config.fit_max_iter =
      read_number_option<int>(joptions, {"fit_max_iter"}, config.fit_max_iter);

  config.adaptive = read_bool_option(joptions, {"adaptive"}, config.adaptive);
  config.adaptive_tol = read_number_option<double>(joptions, {"adaptive_tol"},
                                                   config.adaptive_tol);
  config.stable_checks_required = read_number_option<int>(
      joptions, {"stable_checks_required"}, config.stable_checks_required);
  config.active_block_tol = read_number_option<double>(
      joptions, {"active_block_tol"}, config.active_block_tol);
  config.active_connection_sign_stability =
      read_number_option<double>(joptions, {"active_connection_sign_stability"},
                                 config.active_connection_sign_stability);
  config.active_connection_min_abs_corr =
      read_number_option<double>(joptions, {"active_connection_min_abs_corr"},
                                 config.active_connection_min_abs_corr);
  config.ci_level =
      read_number_option<double>(joptions, {"ci_level"}, config.ci_level);
  config.patience =
      read_number_option<int>(joptions, {"patience"}, config.patience);

  const bool explicit_resampling =
      has_option(joptions, {"resampling_strategy"});
  if (const json *value = find_option(joptions, {"resampling_strategy"})) {
    config.resampling_strategy = parse_resampling_strategy(
        read_string_value(*value, "resampling_strategy"),
        config.resampling_strategy);
  }

  if (const json *value = find_option(joptions, {"stationary_block_length"})) {
    config.stationary_block_length = value->get<double>();
    if (config.stationary_block_length <= 0.0) {
      config.resampling_strategy = fdapde::rgcca::ResamplingStrategy::Ordinary;
    } else if (!explicit_resampling) {
      config.resampling_strategy = fdapde::rgcca::ResamplingStrategy::Stationary;
    }
  }

  config.component_significance_resamples =
      read_number_option<int>(joptions, {"component_significance_resamples"},
                              config.component_significance_resamples);
  config.component_significance_alpha =
      read_number_option<double>(joptions, {"component_significance_alpha"},
                                 config.component_significance_alpha);
  config.block_importance_resamples =
      read_number_option<int>(joptions, {"block_importance_resamples"},
                              config.block_importance_resamples);
  config.block_importance_alpha =
      read_number_option<double>(joptions, {"block_importance_alpha"},
                                 config.block_importance_alpha);
  config.inactive_block_signal_resamples =
      read_number_option<int>(joptions, {"inactive_block_signal_resamples"},
                              config.inactive_block_signal_resamples);
  config.inactive_block_signal_alpha =
      read_number_option<double>(joptions, {"inactive_block_signal_alpha"},
                                 config.inactive_block_signal_alpha);

  if (!explicit_B_min && config.B_max > 0 && config.B_min > config.B_max) {
    config.B_min = config.B_max;
  }
}

inline std::vector<std::vector<double>>
parse_lambda_grid_weights(const json &joptions) {
  if (!joptions.contains("lambda_grid") ||
      joptions.at("lambda_grid").is_null()) {
    return {};
  }

  const auto &jgrid = joptions.at("lambda_grid");
  if (jgrid.is_null() || (jgrid.is_object() && jgrid.empty())) {
    return {};
  }
  if (!jgrid.is_array() || jgrid.empty()) {
    throw std::runtime_error("lambda_grid must be a non-empty array");
  }

  if (jgrid.front().is_number()) {
    return {jgrid.get<std::vector<double>>()};
  }
  if (jgrid.front().is_array()) {
    return jgrid.get<std::vector<std::vector<double>>>();
  }

  throw std::runtime_error(
      "lambda_grid must be either vector<double> or vector<vector<double>>");
}

template <typename Model, typename Options>
void apply_regularization_options(Model &rgcca, const json &joptions,
                                  const Options &options) {
  if (options.lambda_selection_weights == fdapde::rgcca::LambdaSelection::Automatic) {
    const auto lambda_grid = parse_lambda_grid_weights(joptions);
    if (!lambda_grid.empty()) {
      rgcca.set_lambda_grid_weights(lambda_grid);
    }
  } else {
    const double lambda =
        read_number_option<double>(joptions, {"lambda"}, 0.0);
    if (std::isfinite(lambda) && lambda > 0.0) {
      rgcca.set_lambda_weights_all(lambda);
    }
  }

  if (options.lambda_selection_components == fdapde::rgcca::LambdaSelection::Manual) {
    const double lambda_components =
        read_number_option<double>(joptions, {"lambda_components"},
                                   std::numeric_limits<double>::quiet_NaN());
    if (std::isfinite(lambda_components) && lambda_components > 0.0) {
      rgcca.set_lambda_components_all(lambda_components);
    }
  }
}

template <typename Model>
bool apply_C_matrix(Model &rgcca, const json &joptions, const int n_blocks) {
  const json *value = find_option(joptions, {"C"});
  if (!value) {
    return false;
  }
  if (value->is_string()) {
    const auto token = normalize_token(value->get<std::string>());
    if (token == "full" || token == "complete" || token == "fullyconnected" ||
        token == "all") {
      for (int j = 0; j < n_blocks; ++j) {
        for (int k = j + 1; k < n_blocks; ++k) {
          rgcca.connect(j, k, true);
        }
      }
      return true;
    }
    if (token == "empty" || token == "none" || token == "zero" ||
        token == "zeros") {
      for (int j = 0; j < n_blocks; ++j) {
        for (int k = j + 1; k < n_blocks; ++k) {
          rgcca.connect(j, k, false);
        }
      }
      return true;
    }
    throw std::runtime_error("Unknown C matrix token: " +
                             value->get<std::string>());
  }
  if (!value->is_array() || static_cast<int>(value->size()) != n_blocks) {
    throw std::runtime_error("C must be an n_blocks x n_blocks array");
  }

  for (int j = 0; j < n_blocks; ++j) {
    const auto &row = value->at(j);
    if (!row.is_array() || static_cast<int>(row.size()) != n_blocks) {
      throw std::runtime_error("C must be an n_blocks x n_blocks array");
    }
    for (int k = j + 1; k < n_blocks; ++k) {
      rgcca.connect(j, k, read_bool_value(row.at(k), "C"));
    }
  }
  return true;
}

template <typename Options>
bool bootstrap_selection_requested(const Options &options) {
  return options.block_deactivation || options.connection_deactivation ||
         options.lambda_selection_weights == fdapde::rgcca::LambdaSelection::Automatic;
}

inline bool save_bootstrap_resamples(const json &joptions) {
  return read_bool_option(joptions, {"save_bootstrap_resamples"}, false);
}

inline Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>
bool_matrix_to_double(
    const Eigen::Matrix<bool, Eigen::Dynamic, Eigen::Dynamic> &m) {
  Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> out(m.rows(), m.cols());
  for (int i = 0; i < m.rows(); ++i) {
    for (int j = 0; j < m.cols(); ++j) {
      out(i, j) = m(i, j) ? 1.0 : 0.0;
    }
  }
  return out;
}

inline Eigen::Matrix<double, Eigen::Dynamic, 1>
bool_vector_to_double_column(const std::vector<bool> &v) {
  Eigen::Matrix<double, Eigen::Dynamic, 1> out(v.size());
  for (std::size_t i = 0; i < v.size(); ++i) {
    out(static_cast<int>(i)) = v[i] ? 1.0 : 0.0;
  }
  return out;
}

inline Eigen::Matrix<double, Eigen::Dynamic, 1>
double_vector_to_column(const std::vector<double> &v) {
  Eigen::Matrix<double, Eigen::Dynamic, 1> out(v.size());
  for (std::size_t i = 0; i < v.size(); ++i) {
    out(static_cast<int>(i)) = v[i];
  }
  return out;
}

inline Eigen::Matrix<double, Eigen::Dynamic, 1>
int_vector_to_double_column(const std::vector<int> &v) {
  Eigen::Matrix<double, Eigen::Dynamic, 1> out(v.size());
  for (std::size_t i = 0; i < v.size(); ++i) {
    out(static_cast<int>(i)) = static_cast<double>(v[i]);
  }
  return out;
}

inline Eigen::Matrix<double, Eigen::Dynamic, 1>
inactive_block_signal_actions_to_double_column(
    const std::vector<fdapde::rgcca::InactiveBlockSignalAction> &v) {
  Eigen::Matrix<double, Eigen::Dynamic, 1> out(v.size());
  for (std::size_t i = 0; i < v.size(); ++i) {
    out(static_cast<int>(i)) = static_cast<double>(v[i]);
  }
  return out;
}

inline Eigen::Matrix<double, Eigen::Dynamic, 1>
first_value_column(const std::vector<double> &v) {
  Eigen::Matrix<double, Eigen::Dynamic, 1> out(1);
  out(0) = !v.empty() ? v[0] : std::numeric_limits<double>::quiet_NaN();
  return out;
}

template <typename Derived>
inline Eigen::Matrix<double, Eigen::Dynamic, 1>
first_value_column(const Eigen::MatrixBase<Derived> &v) {
  Eigen::Matrix<double, Eigen::Dynamic, 1> out(1);
  out(0) = v.size() > 0 ? v(0) : std::numeric_limits<double>::quiet_NaN();
  return out;
}

inline void
write_component_diagnostics(const std::string &path_results,
                            const std::vector<fdapde::rgcca::Result> &results) {
  Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> diagnostics(
      results.size(), 10);

  for (std::size_t h = 0; h < results.size(); ++h) {
    const auto &result = results[h];
    fdapde::write_csv(path_results + "C" + std::to_string(h + 1) + ".csv",
                      bool_matrix_to_double(result.C));
    fdapde::write_csv(path_results + "active_blocks" + std::to_string(h + 1) +
                          ".csv",
                      bool_vector_to_double_column(result.active_blocks));
    fdapde::write_csv(path_results + "block_importance" +
                          std::to_string(h + 1) + ".csv",
                      double_vector_to_column(result.block_importance));
    fdapde::write_csv(path_results + "block_importance_p_values" +
                          std::to_string(h + 1) + ".csv",
                      double_vector_to_column(result.block_importance_p_values));
    fdapde::write_csv(path_results + "block_importance_significant" +
                          std::to_string(h + 1) + ".csv",
                      bool_vector_to_double_column(
                          result.block_importance_significant));
    fdapde::write_csv(path_results + "inactive_block_signal_actions" +
                          std::to_string(h + 1) + ".csv",
                      inactive_block_signal_actions_to_double_column(
                          result.inactive_block_signal_actions));

    const double final_objective =
        result.obj_history.empty() ? std::numeric_limits<double>::quiet_NaN()
                                   : result.obj_history.back();
    diagnostics(static_cast<int>(h), 0) = static_cast<double>(h + 1);
    diagnostics(static_cast<int>(h), 1) = static_cast<double>(result.n_blocks);
    diagnostics(static_cast<int>(h), 2) = static_cast<double>(result.iters);
    diagnostics(static_cast<int>(h), 3) = result.monotone ? 1.0 : 0.0;
    diagnostics(static_cast<int>(h), 4) = final_objective;
    diagnostics(static_cast<int>(h), 5) = result.rho_tot;
    diagnostics(static_cast<int>(h), 6) = result.rho_tot_p_value;
    diagnostics(static_cast<int>(h), 7) =
        static_cast<double>(result.rho_tot_bootstrap_count);
    diagnostics(static_cast<int>(h), 8) =
        result.component_significant ? 1.0 : 0.0;
    diagnostics(static_cast<int>(h), 9) =
        static_cast<double>(result.block_importance_bootstrap_count);
  }

  fdapde::write_csv(path_results + "component_diagnostics.csv", diagnostics,
                    std::vector<std::string>{"component", "n_blocks", "iters",
                                             "monotone", "objective", "rho_tot",
                                             "p_value", "bootstrap_count",
                                             "significant",
                                             "block_importance_bootstrap_count"});
}

template <typename BootstrapResult>
void write_bootstrap_metadata(const std::string &path_results,
                              const BootstrapResult &boot,
                              const std::size_t component_index,
                              const bool save_resamples) {
  Eigen::Matrix<double, 1, 5> metadata;
  metadata(0, 0) = static_cast<double>(component_index + 1);
  metadata(0, 1) = static_cast<double>(boot.B);
  metadata(0, 2) = boot.lambda_opt;
  metadata(0, 3) = static_cast<double>(boot.lambda_opt_index + 1);
  metadata(0, 4) = boot.ci_level;

  fdapde::write_csv(path_results + "bootstrap_metadata" +
                        std::to_string(component_index + 1) + ".csv",
                    metadata,
                    std::vector<std::string>{"component", "B", "lambda_opt",
                                             "lambda_opt_index", "ci_level"});
  fdapde::write_csv(path_results + "bootstrap_B_used" +
                        std::to_string(component_index + 1) + ".csv",
                    int_vector_to_double_column(boot.B_used_by_lambda));

  for (std::size_t i = 0; i < boot.lambda_grid.size(); ++i) {
    if (save_resamples && i < boot.corr_boot_by_lambda.size() &&
        boot.corr_boot_by_lambda[i].cols() > 0) {
      fdapde::write_csv(path_results + "bootstrap_corr_boot_comp" +
                            std::to_string(component_index + 1) + "_lambda" +
                            std::to_string(i + 1) + ".csv",
                        boot.corr_boot_by_lambda[i]);
    }
  }
}

template <typename BootstrapResult>
bool has_bootstrap_weight_resamples(const BootstrapResult &boot,
                                    const std::size_t lambda_index,
                                    const std::size_t block_index) {
  return lambda_index < boot.w_boot_by_lambda.size() &&
         block_index < boot.w_boot_by_lambda[lambda_index].size() &&
         boot.w_boot_by_lambda[lambda_index][block_index].cols() > 0;
}

} // namespace rgcca_driver

#endif
