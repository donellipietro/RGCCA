#!/bin/bash
set -euo pipefail

# $1: test suite name
# $2: test name


start=$(date +%s.%N)

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROFILE="${RGCCA_PROFILE:-macbook}"

cd "${PROJECT_DIR}"
Rscript config.R --profile "${PROFILE}" --write-env --env-file "${PROJECT_DIR}/.env"

set -a
source "${PROJECT_DIR}/.env"
set +a

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
export directory

# Define your task function
task_function() {
  cd "${PATH_REPO}"
  # Run RScript with the current file as an argument
  Rscript tests/"$1"/main.R "$2" "$3"
  cd "$directory" || exit 1
}

# Export the task function so that it can be used by GNU Parallel
export -f task_function

# Run tasks in parallel using GNU Parallel
ls * | parallel -j "$PARALLEL_CORES" task_function "$1" "$2"

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
