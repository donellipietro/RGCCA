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

# Iterate over the files in the directory
for file in *; do
    # Check if the item is a file
    if [ -f "$file" ]; then
        # Run RScript with the current file as an argument
        cd "${PATH_REPO}"
        Rscript tests/"$1"/main.R "$2" "$file"
        cd "$directory" || exit 1
    fi
done

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
