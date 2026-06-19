#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage:
  ./cpp/compile_slurm.sh <model_name> [target]
  ./cpp/compile_slurm.sh --all

Options:
  --parsable              Print only the submitted job id.
  --dependency JOBID      Add an afterok dependency.
  -h, --help              Show this help.

Environment options:
  SLURM_COMPILE_CPUS      Compile job cpus-per-task (default: 1)
  SLURM_COMPILE_MEM       Compile job memory (default: HEAVY_MEM or DEFAULT_MEM)
  SLURM_COMPILE_TIME      Compile job walltime (default: HEAVY_TIME or DEFAULT_TIME)
  SLURM_DRY_RUN           Print sbatch command without submitting (0/1)
  SLURM_PARTITION         Optional Slurm partition
  SLURM_ACCOUNT           Optional Slurm account
  SLURM_QOS               Optional Slurm QoS
USAGE
}

is_truthy() {
  case "${1:-}" in
    1|true|TRUE|yes|YES|y|Y) return 0 ;;
    *) return 1 ;;
  esac
}

CPP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "${CPP_DIR}/.." && pwd)"
PARSABLE=0
DEPENDENCY=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --parsable)
      PARSABLE=1
      shift
      ;;
    --dependency)
      if [[ $# -lt 2 ]]; then
        echo "Error: --dependency requires a job id." >&2
        exit 1
      fi
      DEPENDENCY="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --)
      shift
      break
      ;;
    *)
      break
      ;;
  esac
done

if [[ ! -f "${PROJECT_DIR}/.env" ]]; then
  echo "Error: .env not found. Run: make build TESTBENCH_PROFILE=<profile>" >&2
  exit 1
fi

set -a
source "${PROJECT_DIR}/.env"
set +a

if [[ $# -lt 1 ]]; then
  usage
  exit 1
fi

if ! is_truthy "${SLURM_DRY_RUN:-0}" && ! command -v sbatch >/dev/null 2>&1; then
  echo "Error: sbatch not found in PATH." >&2
  exit 1
fi

COMPILE_ARGS=()
JOB_TARGET=""
case "$1" in
  --all)
    COMPILE_ARGS=(--all)
    JOB_TARGET="all"
    ;;
  *)
    COMPILE_ARGS=("$1")
    JOB_TARGET="$1"
    if [[ -n "${2:-}" ]]; then
      COMPILE_ARGS+=("$2")
      JOB_TARGET="${JOB_TARGET}_$2"
    fi
    ;;
esac

CPUS="${SLURM_COMPILE_CPUS:-${COMPILE_CPUS:-1}}"
MEM="${SLURM_COMPILE_MEM:-${COMPILE_MEM:-${HEAVY_MEM:-${DEFAULT_MEM:-16GB}}}}"
TIME="${SLURM_COMPILE_TIME:-${COMPILE_TIME:-${HEAVY_TIME:-${DEFAULT_TIME:-04:00:00}}}}"
LOG_DIR="${PATH_LOGS}/slurm/compile"
mkdir -p "${LOG_DIR}"

JOB_SLUG="compile_${JOB_TARGET}"
JOB_SLUG="${JOB_SLUG//[^A-Za-z0-9_]/_}"
JOB_SLUG="${JOB_SLUG:0:48}"

compile_cmd=("./cpp/compile.sh" "${COMPILE_ARGS[@]}")
printf -v compile_cmd_quoted ' %q' "${compile_cmd[@]}"
compile_cmd_quoted="${compile_cmd_quoted# }"

COMPILE_WRAP=$(cat <<EOF
set -euo pipefail

cd '${PATH_REPO}'
set -a
source '${PROJECT_DIR}/.env'
set +a

export OMP_NUM_THREADS="\${OMP_NUM_THREADS:-1}"
export OPENBLAS_NUM_THREADS="\${OPENBLAS_NUM_THREADS:-1}"
export VECLIB_MAXIMUM_THREADS="\${VECLIB_MAXIMUM_THREADS:-1}"

echo '========================================'
echo 'Compiling C++ target: ${JOB_TARGET}'
echo 'Started at:' \$(date)
echo 'Working directory:' \$(pwd)
echo "SLURM_JOB_ID: \${SLURM_JOB_ID:-unset}"
echo "SLURM_CPUS_PER_TASK: \${SLURM_CPUS_PER_TASK:-unset}"
${compile_cmd_quoted}
echo 'Finished at:' \$(date)
echo '========================================'
EOF
)

SBATCH_ARGS=(
  --parsable
  --job-name="tb_${JOB_SLUG}"
  --output="${LOG_DIR}/tb_${JOB_SLUG}_%j.out"
  --error="${LOG_DIR}/tb_${JOB_SLUG}_%j.err"
  --time="${TIME}"
  --mem="${MEM}"
  --cpus-per-task="${CPUS}"
)

if [[ -n "${DEPENDENCY}" ]]; then
  SBATCH_ARGS+=(--dependency="afterok:${DEPENDENCY}")
fi
if [[ -n "${SLURM_PARTITION:-}" ]]; then
  SBATCH_ARGS+=(--partition="${SLURM_PARTITION}")
fi
if [[ -n "${SLURM_ACCOUNT:-}" ]]; then
  SBATCH_ARGS+=(--account="${SLURM_ACCOUNT}")
fi
if [[ -n "${SLURM_QOS:-}" ]]; then
  SBATCH_ARGS+=(--qos="${SLURM_QOS}")
fi
SBATCH_ARGS+=(--wrap="${COMPILE_WRAP}")

if is_truthy "${SLURM_DRY_RUN:-0}"; then
  printf 'sbatch'
  printf ' %q' "${SBATCH_ARGS[@]}"
  printf '\n'
  exit 0
fi

JOB_ID="$(sbatch "${SBATCH_ARGS[@]}")"
if [[ "${PARSABLE}" -eq 1 ]]; then
  printf '%s\n' "${JOB_ID}"
else
  echo "Submitted compile job: ${JOB_ID}"
  echo "Logs: ${LOG_DIR}"
fi
