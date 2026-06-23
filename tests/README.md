# Test Suite Guide

The `tests/` directory contains independent experiment suites. A suite should be
small enough for a reader to understand locally, but structured enough for the
shared runners to execute it automatically.

## Minimal Suite Contract

Each suite needs these files:

```text
tests/<suite>/
├── config.R
├── main.R
├── aggregate_results.R
└── utils/
    ├── generate_options.R
    ├── generate_data.R
    ├── wrappers.R
    ├── fit_and_evaluate.R
    ├── model_evaluation.R
    └── adjust_results.R
```

Optional plotting and inspection helpers can live beside them.

## Responsibilities

- `config.R`: names the suite, sets defaults, and can define grouped tests.
- `generate_options.R`: creates JSON option files in the queue.
- `generate_data.R`: creates synthetic or resampled data for one batch.
- `wrappers.R`: maps model names to R or C++ fitting code.
- `fit_and_evaluate.R`: handles caching, fitting, and batch evaluation.
- `model_evaluation.R`: computes metrics from fitted models and truth.
- `aggregate_results.R`: reloads all finished options and produces summaries.

## Normal Workflow

1. `src/init.R` calls the suite option generator.
2. A runner executes `main.R <test_name> <option_file>` for each JSON file.
3. `main.R` repeats data generation and model fitting for `n_reps` batches.
4. Batch results are saved under `PATH_RESULTS/<suite>/<test>/<option>/`.
5. `aggregate_results.R` reloads those outputs and writes plots.

## Creating a New Suite

Start from the closest existing suite, then make these changes in order:

1. Rename the suite folder and update `config.R`.
2. Simplify `generate_options.R` until it creates one tiny option file.
3. Make `generate_data.R` return all truth and observed data needed by the
   metrics.
4. Implement one model wrapper and verify it returns the standard model object.
5. Add the metric computation.
6. Run a small local test.
7. Expand the option grid only after the small test works.
