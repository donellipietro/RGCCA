# fdaPDE Methods Test Bench

## Overview

This repository serves as a test bench specifically designed for evaluating methods related to [fdaPDE](https://fdapde.github.io) (Physics-Informed Spatial and Functional Data Analysis). It provides a collection of utilities and scripts to facilitate the testing process, including model evaluation metrics computation, plot generation, and automation of tests with various parameter configurations.

## Features

- **Model Evaluation Metrics:** Utilities are available for computing various model evaluation metrics (`RMSE`, `IRMSE`, `...`), allowing for comprehensive assessment of fdaPDE methods' performance.
- **Plot Generation:** The repository includes tools for generating plots to visualize the results of the tested methods, aiding in the interpretation and analysis of the experimental outcomes.
- **Test Automation:** Scripts are provided for automating the execution of tests with different parameter configurations. The `tests/run_tests.sh` script facilitates the setup and execution of test suites, streamlining the testing process.

## Usage

### Running Tests

To run tests using the provided utilities, follow these steps::

1. Create a new directory in test containing all the tests scripts (an example test has been created as a reference).
2. Execute the `tests/run_tests.sh` script, passing the test suite name and test name as arguments. For example:

   ```bash
   ./tests/run_tests.sh example test1
   ```

3. The script initializes the test environment, then iterates over the options files in the specified directory, executing the main test script (main.R) for each file found.
4. After completing the tests, the script performs post-processing tasks, including runtime complexity analysis.

Local tests can also be launched through `make`:

```bash
make run_test TEST_SUITE=RGCCA-2D TEST_NAME=testResampling
make run_test_parallel TEST_SUITE=RGCCA-2D TEST_NAME=testResampling
```

The active profile is stored in `.env`. To switch machine/profile, rebuild once:

```bash
make build TESTBENCH_PROFILE=macbook
make build TESTBENCH_PROFILE=donders_hcp
```

After that, regular `make` targets reuse the profile from `.env`.
`make clean` preserves `.env`; `make distclean` removes it.

If a profile stores generated folders outside the repository, `make build`
creates root-level links for `results`, `images`, `data/tests`, `tmp`, and
`build` when those paths are not already regular files or directories.
`make clean` removes these root-level links without removing regular folders
that happen to exist at the same paths.
Some SSH/SFTP clients render POSIX symlinks as executable-looking files; the
actual target paths are always available with `make config`.

On a Slurm cluster, submit one array task per generated JSON option file:

```bash
make run_test_slurm TEST_SUITE=RGCCA-2D TEST_NAME=testResampling
```

Compile C++ executables explicitly when a model folder contains multiple
`main*.cpp` files:

```bash
make compile MODEL=RGCCA
make compile MODEL=RGCCA TARGET=fit_model_RGCCA
make compile MODEL=RGCCA TARGET=all
```

On the cluster, submit compilation to Slurm instead of running it on the login
node:

```bash
make compile_slurm MODEL=RGCCA TARGET=fit_model_RGCCA SLURM_COMPILE_MEM=64GB
```

Useful Slurm options:

```bash
# Use the heavy resource class from config.R
make run_test_slurm TEST_SUITE=RGCCA-2D TEST_NAME=testResampling SLURM_RESOURCES=heavy

# Limit the number of simultaneously running array tasks
make run_test_slurm TEST_SUITE=RGCCA-2D TEST_NAME=testResampling SLURM_ARRAY_LIMIT=20

# Preview sbatch commands without submitting
make run_test_slurm TEST_SUITE=RGCCA-2D TEST_NAME=testResampling SLURM_DRY_RUN=1

# Compile C++ models before submission, using the selected profile/compiler setup
make run_test_slurm TEST_SUITE=RGCCA-2D TEST_NAME=testResampling SLURM_COMPILE=1

# Compile a specific executable before submission and make the array depend on it
make run_test_slurm TEST_SUITE=RGCCA-IS-1D TEST_NAME=testMultivariate \
  SLURM_COMPILE=1 SLURM_COMPILE_MODEL=RGCCA SLURM_COMPILE_TARGET=fit_model_RGCCA
```

Tests can be declared as groups in a suite `config.R`. For example,
`RGCCA-IS-1D` exposes `testSensitivity` as a parent for
`testSensitivitySingleThread` and `testSensitivityMultiThread`; running the
parent prepares and runs both child queues, then aggregates them together.
Child options declare `test_options$threading` as `"single"` or `"multi"`.
Local parallel runs keep multi-thread children sequential, while Slurm submits
multi-thread children with `MULTITHREAD_CPUS`, `MULTITHREAD_MEM`, and
`MULTITHREAD_TIME` from `config.R` unless overridden with
`SLURM_MULTI_CPUS`, `SLURM_MULTI_MEM`, or `SLURM_MULTI_TIME`.

The runners export `OMP_NUM_THREADS=1`, `OPENBLAS_NUM_THREADS=1`, and
`VECLIB_MAXIMUM_THREADS=1` by default so each parallel test worker does not
spawn additional BLAS/OpenMP threads unless explicitly overridden.

### Makefile

The `Makefile` provided in this repository includes several targets to automate common tasks related to installation, testing, building, and cleaning up the project environment. Below is a brief description of each target:

- `install_fdaPDE2`: Installs the `fdaPDE2` package by executing the `install_fdaPDE2.R` script located in the `src/installation/` directory.
- `install_femR`: Installs the `femR` package by executing the `install_femR.R` script located in the `src/installation/ directory`.
- `install`: Combines the `install_fdaPDE2` and `install_femR` targets to install both the `fdaPDE2` and `femR` packages.
- `test_example`: Runs example tests by executing the `tests/run_tests.sh` script with the arguments example and test1.
- `tests`: Combines the `build`, `clean`, and `test_example` targets to perform a complete test run, ensuring that tests are executed on a clean environment after building.
- `build`: Creates necessary directories for data, results, and images. This target ensures the directory structure required for the project is in place.
- `clean_options`: Cleans temporary files by removing the `queue/` directory.
- `clean`: Combines the `clean_options` target with additional cleanup tasks, such as removing auxiliary files, R history, and data files.
- `distclean`: Combines the `clean` target with further cleanup actions, including the removal of additional generated files like images and results. It prompts for confirmation before executing to avoid accidental deletion.

These targets can be executed using the make command followed by the target name, for example, to run all tests:

```bash
make tests
```

Refer to the [`Makefile`](./Makefile) for implementation details and additional customization options.

## Repository structure

```bash
.
├── LICENSE
├── Makefile
├── README.md
├── data
│   └── mesh
│       ├── ...
├── src
│   ├── installation
│   │   ├── install_fdaPDE2.R
│   │   └── install_femR.R
│   └── utils
│       ├── cat.R
│       ├── directories.R
│       ├── meshes.R
│       ├── domain_and_locations.R
│       ├── errors.R
│       ├── results_management.R
│       ├── plots.R
│       └── wrappers.R
├── analysis
└── tests
    ├── run_tests.sh
    ├── run_tests_parallel.sh
    └── run_tests_slurm.sh
```

**Files**:

- **LICENSE**: GPL v3 License file specifying the terms and conditions for using the repository.
- **Makefile**: Makefile for automating build tasks or running commands.
- **README.md**: This documentation file providing an overview of the repository and its usage instructions.
- **tests/run_tests.sh**: Script for automating the execution of tests with different parameter configurations.

**Directories**:

- **data**: Directory storing general data files, including mesh data used in tests.
- **src**: Source code directory containing installation scripts and utility functions.
- **tests**: Directory for storing test scripts and related resources.
- **analysis**: Directory containing analysis-related scripts or resources.

## Authorship

This test bench repository is maintained by Pietro Donelli.

## License

This repository is licensed under the GPL v3 License. See the [LICENSE](./LICENSE) file for details.
