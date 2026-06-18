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
