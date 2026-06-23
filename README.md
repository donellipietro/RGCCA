# Model Test Bench

This repository is a reusable test bench for running simulation studies on
statistical or scientific-computing models. It was created from a template and
currently contains RGCCA-oriented experiments, but the workflow is intentionally
generic: generate option files, run one model-fitting script per option, save
results, and aggregate the outputs.

## Quick User Guide

### 1. Choose a profile

Runtime paths, compilers, output directories, and cluster settings are stored in
`config.R`. Build the active profile once:

```bash
make build TESTBENCH_PROFILE=macbook
```

This writes `.env`, creates generated directories, and installs the configured R
dependencies. Later commands reuse `.env`.

Useful profile commands:

```bash
make config
make build TESTBENCH_PROFILE=donders_hcp
make clean
make distclean
```

### 2. Compile C++ models when needed

If a test option uses compiled solvers, compile the matching model folder:

```bash
make compile MODEL=RGCCA
make compile MODEL=RGCCA TARGET=fit_model_RGCCA
make compile MODEL=RGCCA TARGET=all
```

On a Slurm cluster:

```bash
make compile_slurm MODEL=RGCCA TARGET=fit_model_RGCCA
make compile_all_slurm
```

Slurm compilation defaults to the profile's multithread resources. Override the
allocation with `SLURM_COMPILE_CPUS`, `SLURM_COMPILE_MEM`, or
`SLURM_COMPILE_TIME`; override compiler fan-out with `SLURM_COMPILE_JOBS`.

### 3. Run a test

Sequential run:

```bash
make run_test TEST_SUITE=RGCCA-2D TEST_NAME=testResampling
```

Local parallel run:

```bash
make run_test_parallel TEST_SUITE=RGCCA-2D TEST_NAME=testResampling
```

Slurm job-array run:

```bash
make run_test_slurm TEST_SUITE=RGCCA-2D TEST_NAME=testResampling
make run_test_slurm TEST_SUITE=RGCCA-2D TEST_NAME=all
```

Outputs are written under the configured `results`, `images`, `data/tests`, and
`tmp` locations. If those locations are outside the repository, `make build`
creates root-level symlinks when possible.

## Detailed Maintainer Guide

### Repository Layout

```text
.
├── config.R                  # machine/profile configuration
├── Makefile                  # common build, compile, run, and clean commands
├── cpp/                      # compiled model drivers and runtime wrappers
├── data/mesh/                # shared mesh and region inputs
├── src/                      # shared R utilities and CLI helpers
└── tests/                    # test suites
```

The two current suites, `RGCCA-2D` and `RGCCA-IS-1D`, are examples of the same
template pattern. New model families can add their own suite under `tests/`
without changing the runner scripts.

### Execution Flow

1. `src/init.R` reads the requested suite and test name.
2. The suite-specific `utils/generate_options.R` writes one JSON file per
   option combination into the queue directory.
3. A runner script executes `tests/<suite>/main.R` once per JSON option.
4. `main.R` generates data for each batch, fits every requested model, evaluates
   it, and saves batch-level `.RData` plus CSV outputs.
5. `tests/<suite>/aggregate_results.R` reloads all batch results and creates
   aggregate figures.

The queue is the central coordination point. Local runners loop over JSON files;
the Slurm runner submits one array task per JSON file.

### Test Suites

Each suite should contain:

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
    ├── adjust_results.R
    ├── plot_results.R
    └── load_qualitative_results.R
```

The suite `config.R` defines the suite name, default test, execution flags, and
optional test groups. Test groups allow one public name to expand into multiple
child tests, for example a single-thread and multi-thread version of the same
experiment.

### Options

`generate_options()` creates JSON files. The shared helpers in
`src/utils/options.R` are used to expand a nested option object over selected
fields:

```r
options_list <- explode_options(
  options,
  by = options$test_options$varying_options,
  name_fun = name_fun
)
write_options_json(options_list, dir = path_queue)
```

Keep option names stable because they become result folder names, log names, and
plot labels.

### Model Wrappers

Suite wrappers translate a model name into a fitted model object with this common
shape:

```r
list(
  options = ...,
  diagnostics = ...,
  model_selection = ...,
  results = ...,
  model_traits = ...
)
```

R backends fit directly inside `wrappers.R`. C++ backends write temporary CSV and
JSON inputs, call `cpp/run.sh`, then reload CSV outputs. The shared functions in
`src/utils/rgcca_options.R` keep R and C++ option handling aligned.

### Generated Paths

Profiles can keep large outputs outside the repository. The important generated
paths are:

- `PATH_QUEUE`: JSON option files waiting to be run.
- `PATH_RESULTS`: fitted models, evaluation objects, and per-option outputs.
- `PATH_IMAGES`: aggregate plots.
- `PATH_TEST_DATA`: saved generated data useful for qualitative plots.
- `PATH_TMP_DATA` and `PATH_TMP_RESULTS`: temporary C++ exchange files.
- `PATH_BUILD`: compiled executables.

`make clean` removes temporary files and profile symlinks. `make distclean`
removes generated results, images, test data, compiled files, and `.env`.

### Slurm Notes

`make run_test_slurm` prepares the queue locally, detects each child test's
threading mode, and submits array jobs. Important overrides include:

```bash
SLURM_ARRAY_LIMIT=20
SLURM_DRY_RUN=1
SLURM_COMPILE=1
SLURM_CPUS=1
SLURM_MEM=16GB
SLURM_TIME=12:00:00
SLURM_MULTI_CPUS=20
SLURM_MULTI_MEM=32GB
SLURM_MULTI_TIME=72:00:00
```

The runners set `OMP_NUM_THREADS=1`, `OPENBLAS_NUM_THREADS=1`, and
`VECLIB_MAXIMUM_THREADS=1` by default so outer parallelism does not accidentally
multiply inner BLAS/OpenMP threads.

## Adding a New Model Family

1. Copy an existing suite under `tests/` and rename it.
2. Update its `config.R` suite names and default test.
3. Rewrite `generate_options.R` so it emits the option grid you need.
4. Rewrite `generate_data.R` for the data-generating process.
5. Add or replace model names in `wrappers.R`.
6. Ensure each wrapper returns the standard model object.
7. Update `model_evaluation.R` and plotting helpers for the metrics you care
   about.
8. Run a tiny smoke test before launching a large grid.

## License

This repository is licensed under GPL v3. See `LICENSE` for details.
