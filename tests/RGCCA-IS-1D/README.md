# RGCCA-IS-1D Simulation Study

This suite studies RGCCA models on synthetic one-dimensional functional data
observed by independent sampling on the unit interval. The simulations are
designed around three questions:

- `testMultivariate`: agreement between the R and C++ multivariate
  implementations.
- `testSensitivity`: effect of regularization and efficacy of model selection.
- `testBootstrap`: effect of bootstrap size and efficacy of the adaptive
  bootstrap.

Unless `SMOKE_TEST = TRUE` is set in `config.R`, each configuration is repeated
over 30 independent Monte Carlo replicates.

## Data-Generating Mechanism

Each dataset contains four data blocks observed on the unit interval. For each
block, the signal is a low-rank functional structure:

```text
X_g = H_g A_g' + E_g,    g = 1, ..., 4.
```

The true loading functions are smooth Gaussian bumps centered at `1/4`, `1/2`,
and `3/4`. Some block-component pairs have exactly zero loadings, which defines
the true active-block structure. The default candidate block graph is fully
connected, and the true active-connection structure is defined by the nonzero
cross-block correlations of the latent scores for each component.

The latent component scores are generated from a multivariate normal
distribution with prescribed cross-block correlations. Loadings are normalized
to unit Euclidean norm and scores to unit standard deviation before centered
Gaussian noise is added. The noise level is controlled by `sigma_noise`; all
simulation grids include the noiseless case `sigma_noise = 0`.

The default design uses four blocks and three components. Functional methods are
evaluated on the observed locations and, when available, on a high-resolution
grid used for visualization.

## Methods Compared

The suite compares RGCCA-family models across implementation, regularization
mode, discretization, and sign constraints.

Implementation:

- R backend through the `RGCCA` package.
- C++ backend through the compiled project solvers.

Regularization mode:

- `cor`: correlation-oriented GCCA, with `tau = 0`.
- `RGCCA` / `fRGCCA`: regularized mode.
- `cov`: covariance-oriented GCCA, with `tau = 1`.

Discretization:

- Multivariate discretization.
- Functional FEM discretization.
- Functional spline discretization.

Constraint:

- Unconstrained weights.
- Non-negative weights, denoted by `_NN_` in model names.

Estimated components are aligned to the synthetic truth before computing
accuracy metrics, using the best matching between estimated and true loadings
and a sign normalization.

## `testMultivariate`: R vs C++ Implementation

This experiment compares the R and C++ multivariate implementations under
different sample sizes and spatial resolutions.

Design:

- Replicates: 30 by default.
- Sample sizes: `n = 200, 300, 400`.
- Observation locations: `n_locs = 101, 501`.
- Noise level: `sigma_noise = 1`.
- Regularization: `lambda = 0`.
- Model-selection options are disabled.

Methods:

- R and C++ versions of `GCCA_cor`, `RGCCA`, and `GCCA_cov`.

Purpose:

The expected outcome is comparable estimation accuracy and objective behavior
between R and C++ implementations of the same model. Runtime is also evaluated
as sample size and number of locations increase.

## `testSensitivity`: Regularization and Model Selection

This experiment studies how estimation changes with the regularization level
and the noise level. It also evaluates whether bootstrap-based model selection
recovers the active blocks and active connections of the data-generating model.

Common design:

- Replicates: 30 by default.
- Sample size: `n = 1200`.
- Observation locations: `n_locs = 101`.
- High-resolution grid size: `200`.
- Noise levels: `sigma_noise = 0, 1, 2, 3, 4, 5`.
- Methods: covariance-mode C++ multivariate, FEM, and spline models, with and
  without non-negative constraints.

Fixed-regularization setting:

- Lambda grid: `lambda = 0, 10^-9, ..., 10^-2`.
- Model selection is disabled.

Model-selection setting:

- Lambda is set to the automatic-selection token.
- Bootstrap model selection is enabled for weight regularization, block
  deactivation, and connection deactivation.
- Adaptive bootstrap is enabled with maximum budget `B_max = 5000`.

Purpose:

The fixed-regularization setting evaluates sensitivity to the magnitude of the
regularization parameter. The model-selection setting evaluates whether the
procedure chooses suitable regularization and recovers the known active-block
and active-connection structures as the signal-to-noise ratio changes.

## `testBootstrap`: Bootstrap Size and Adaptive Bootstrap

This experiment studies how the bootstrap budget affects model selection and
whether adaptive stopping achieves comparable results to a large fixed budget.

Design:

- Replicates: 30 by default.
- Sample size: `n = 1200`.
- Observation locations: `n_locs = 101`.
- High-resolution grid size: `200`.
- Noise levels: `sigma_noise = 0, 1, 2, 3, 4, 5`.
- Bootstrap budgets: `B_max = 10, 100, 500, 1000`.
- Adaptive setting: encoded as `B_max = -1` in the option grid and run as an
  adaptive bootstrap with maximum budget `B_max = 5000`.
- Minimum bootstrap budget: `B_min = 10`.
- Weight selection, block deactivation, and connection deactivation are enabled.

Methods:

- Functional covariance-mode C++ models with spline discretization.
- Functional covariance-mode non-negative C++ models with spline discretization.

Purpose:

The fixed-budget configurations quantify the effect of increasing bootstrap
size on estimation and model-selection accuracy. The adaptive configuration
tests whether the adaptive procedure can reach stable model-selection behavior
without always using the largest possible bootstrap budget.

## Evaluation Metrics

For each fitted model and each replicate, the suite evaluates:

- Execution time.
- Solver objective value for each component.
- Number of solver iterations for each component.
- Effective number of bootstrap resamples for each component.
- Optimal weight-regularization parameter for each component.
- Component-wise normalized RMSE for the block scores `H_g`.
- Component-wise normalized RMSE for the loadings `A_g` at observed locations.
- Component-wise normalized RMSE for the dual/star loadings `A*_g` at observed
  locations.
- Cumulative normalized RMSE over all components, separately for `H_g`, `A_g`,
  and `A*_g` in each block.
- Active-block model-selection accuracy, false positives, false negatives, and
  selected active-block counts.
- Active-connection model-selection accuracy, false positives, false negatives,
  and selected active-connection counts.

The normalized RMSE divides each error by the RMSE of the corresponding true
quantity, with a denominator of one when the true quantity is identically zero.

## Summary Figures

The aggregation script produces PDF summaries for completed simulations:

- `time_complexity.pdf`: execution time across methods and simulation factors.
- `objective.pdf`: solver objective values.
- `diagnostics.pdf`: solver iterations and effective bootstrap resamples.
- `model_selection.pdf`: selected lambdas on a log-y scale, active-block
  accuracy, and active-connection accuracy.
- `rmse_g1.pdf`, ..., `rmse_g4.pdf`: component-wise and cumulative RMSE for
  scores, loadings, and dual/star loadings in each block.

Together, these outputs summarize implementation agreement, regularization
sensitivity, bootstrap/model-selection behavior, and estimation accuracy across
the three simulation experiments.
