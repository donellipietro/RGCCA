#!/bin/bash
set -euo pipefail

# $1: test suite name
# $2: test name


start=$(date +%s.%N)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

cd "${PROJECT_DIR}"
if [[ ! -f "${PROJECT_DIR}/.env" ]]; then
  echo "Error: .env not found. Run: make build TESTBENCH_PROFILE=<profile>"
  exit 1
fi

set -a
source "${PROJECT_DIR}/.env"
set +a

export OMP_NUM_THREADS="${OMP_NUM_THREADS:-1}"
export OPENBLAS_NUM_THREADS="${OPENBLAS_NUM_THREADS:-1}"
export VECLIB_MAXIMUM_THREADS="${VECLIB_MAXIMUM_THREADS:-1}"

cd "${PATH_REPO}"

# Define the number of high-performance cores
NUM_CORES=12

# Define the total number of CPU cores
if command -v sysctl >/dev/null 2>&1 && sysctl -n hw.physicalcpu >/dev/null 2>&1; then
  TOTAL_CORES=$(sysctl -n hw.physicalcpu)
elif command -v nproc >/dev/null 2>&1; then
  TOTAL_CORES=$(nproc)
else
  TOTAL_CORES=1
fi

# Calculate the number of cores to be used for parallel processing
PARALLEL_CORES=$((TOTAL_CORES - 4))

# Ensure that PARALLEL_CORES does not exceed the available high-performance cores
if [ "$PARALLEL_CORES" -gt "$NUM_CORES" ]; then
  PARALLEL_CORES=$NUM_CORES
fi


###############################################################################


Rscript src/init.R "$1" "$2"

# Set the directory containing the files
directory="${PATH_QUEUE}/$1/$2/"

# Check if the directory exists
if [ ! -d "$directory" ]; then
echo "Directory $directory not found."
exit 1
fi

# Change to the specified directory
cd "$directory" || exit 1
export PATH_REPO
export TEST_SUITE="$1"
export TEST_NAME="$2"

# Run tasks in parallel using GNU Parallel
find . -maxdepth 1 -type f -name '*.json' -exec basename {} \; | sort | \
  parallel -j "$PARALLEL_CORES" --halt soon,fail=1 \
    'cd "$PATH_REPO" && Rscript tests/"$TEST_SUITE"/main.R "$TEST_NAME" {}'

## Run time complexity analysis
cd "${PATH_REPO}"
Rscript tests/"$1"/aggregate_results.R "$2"


###############################################################################


# End timer
end=$(date +%s.%N)
execution_time=$(echo "$end - $start" | bc)

# Print total execution time
echo ""
echo "Total execution time: $execution_time seconds"
echo ""
