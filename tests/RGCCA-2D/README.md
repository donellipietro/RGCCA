# RGCCA-2D Simulation Study

This suite studies RGCCA models on synthetic spatio-temporal data. The
simulations are designed around two questions:

- `testSensitivity`: effect of regularization and efficacy of model selection.
- `testResampling`: efficacy of the stationary resampling method for
  bootstrap-based model selection.

Unless `SMOKE_TEST = TRUE` is set in `config.R`, each configuration is repeated
over 30 independent Monte Carlo replicates.

## Data-Generating Mechanism

Each dataset contains four data blocks observed over time and across spatial
locations in four spatial regions. For block `g`, the noiseless signal is a
low-rank spatio-temporal structure:

```text
X_g = E_g A_g' + N_g,    g = 1, ..., 4.
```

Here `A_g` contains the true spatial loading fields evaluated inside the
corresponding region, and `E_g` contains the true temporal component profiles.
The temporal profiles are sine waves with group-specific frequencies and
component-specific amplitudes. Some group-component temporal profiles are
identically zero, which makes the corresponding block inactive for that
component.

The default candidate block graph is fully connected. The true active-block
structure is defined by nonzero signal contributions in a block and component.
The true active-connection structure is defined by the nonzero cross-block
correlations of the temporal profiles for each component.

The spatial loadings and temporal profiles are normalized component-wise before
centered Gaussian noise is added. The noise level is controlled by
`sigma_noise`; all sensitivity grids include the noiseless case
`sigma_noise = 0`.

The default design uses four blocks, three components, 1200 temporal
observations, and region-specific spatial sample sizes.

## Methods Compared

The suite compares RGCCA-family models across regularization mode, spatial and
temporal discretization, and sign constraints.

Regularization mode:

- `cor`: correlation-oriented GCCA, with `tau = 0`.
- `RGCCA` / `fRGCCA` / `tfRGCCA`: regularized mode.
- `cov`: covariance-oriented GCCA, with `tau = 1`.

Discretization:

- Multivariate discretization.
- Functional spatial discretization.
- Functional spatio-temporal discretization.

Constraint:

- Unconstrained weights.
- Non-negative weights, denoted by `_NN_` in model names.

Estimated components are aligned to the synthetic truth before computing
accuracy metrics, using the best matching between estimated and true spatial
loadings and a sign normalization.

## `testSensitivity`: Regularization and Model Selection

This experiment studies how estimation changes with the regularization level
and the noise level. It also evaluates whether bootstrap-based model selection
recovers the active blocks and active connections of the data-generating model.

Common design:

- Replicates: 30 by default.
- Time interval length: `T_sec = 200`.
- Temporal observations: `n_times = 1200`.
- Temporal mesh nodes: `n_nodes_T = 51`.
- Noise levels: `sigma_noise = 0, 1, 2, 4, 6`.
- Methods: covariance-mode C++ multivariate, functional spatial, and functional
  spatio-temporal models, with and without non-negative constraints.
- Adaptive bootstrap with maximum budget `B_max = 5000`.

Fixed-regularization setting:

- Lambda grid: `lambda = 0, 10^-6, ..., 10^4`.
- Model selection is disabled.

Model-selection setting:

- Lambda is set to the automatic-selection token.
- Bootstrap model selection is enabled for weight regularization, block
  deactivation, and connection deactivation.

Purpose:

The fixed-regularization setting evaluates sensitivity to the magnitude of the
regularization parameter. The model-selection setting evaluates whether the
procedure chooses suitable regularization and recovers the known active-block
and active-connection structures as the signal-to-noise ratio changes.

## `testResampling`: Stationary Resampling

This experiment studies the stationary resampling method used inside adaptive
bootstrap model selection.

Design:

- Replicates: 30 by default.
- Time interval length: `T_sec = 200`.
- Temporal observations: `n_times = 1200`.
- Temporal mesh nodes: `n_nodes_T = 51`.
- Noise level: `sigma_noise = 6`.
- Stationary block lengths: `0, 1, 10, 50, 100`; length `0` denotes ordinary
  bootstrap resampling.
- Lambda is set to the automatic-selection token.
- Adaptive bootstrap is enabled with maximum budget `B_max = 5000`.
- Weight selection, block deactivation, and connection deactivation are enabled.

Methods:

- Covariance-mode functional spatial C++ model.
- Covariance-mode functional spatial non-negative C++ model.

Purpose:

The experiment evaluates how the stationary block length affects estimation and
model-selection accuracy when temporal dependence is handled through
resampling. It also reports how many bootstrap resamples are effectively used by
the adaptive procedure.

## Evaluation Metrics

For each fitted model and each replicate, the suite evaluates:

- Execution time.
- Solver objective value for each component.
- Number of solver iterations for each component.
- Effective number of bootstrap resamples for each component.
- Optimal weight-regularization parameter for each component.
- Component-wise normalized RMSE for the temporal profiles `E_g`.
- Component-wise normalized RMSE for the spatial loadings `A_g` at observed
  locations.
- Component-wise normalized RMSE for the dual/star loadings `A*_g` at observed
  locations.
- Cumulative normalized RMSE over all components, separately for `E_g`, `A_g`,
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
  temporal profiles, spatial loadings, and dual/star loadings in each block.

Together, these outputs summarize regularization sensitivity, stationary
resampling behavior, bootstrap/model-selection behavior, and estimation
accuracy across the two simulation experiments.
