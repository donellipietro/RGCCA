#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

SESSION="${SESSION:-RGCCA}"
TEST_SUITE="${TEST_SUITE:-RGCCA-2D}"
TEST_NAME="${TEST_NAME:-testResampling}"
PROFILE="${PROFILE:-}"

make_words() {
  printf "make"
  if [ -n "$PROFILE" ]; then
    printf " PROFILE=%q" "$PROFILE"
  fi
  for word in "$@"; do
    printf " %q" "$word"
  done
}

run_help() {
  clear
  make_args=()
  if [ -n "$PROFILE" ]; then
    make_args+=("PROFILE=$PROFILE")
  fi
  make "${make_args[@]}" help
  exec "${SHELL:-/bin/bash}"
}

run_status() {
  clear
  printf "RGCCA workspace\n\n"
  printf "root: %s\n" "$ROOT"
  printf "branch: "
  git branch --show-current 2>/dev/null || true
  printf "\n"
  git status --short
  printf "\n"

  if [ -f .env ]; then
    set -a
    # shellcheck disable=SC1091
    . ./.env
    set +a
    printf ".env: present\n"
    printf "profile: %s\n" "${TESTBENCH_PROFILE:-unknown}"
  else
    printf ".env: missing, run: make build PROFILE=<profile>\n"
    printf "profile: %s\n" "${PROFILE:-macbook}"
  fi

  printf "\n"
  make_words config
  printf "\n"
  exec "${SHELL:-/bin/bash}"
}

run_files() {
  while :; do
    clear
    date
    printf "\nGenerated files\n"
    find tmp results images data/tests -maxdepth 3 -mindepth 1 2>/dev/null | sort | tail -60 || true
    sleep 5
  done
}

run_logs() {
  while :; do
    clear
    date
    log_dir="tmp/logs"
    if [ -f .env ]; then
      set -a
      # shellcheck disable=SC1091
      . ./.env
      set +a
      log_dir="${PATH_LOGS:-$log_dir}"
    fi

    printf "\nLogs: %s\n\n" "$log_dir"
    if [ -d "$log_dir" ]; then
      latest=""
      while IFS= read -r file; do
        if [ -z "$latest" ] || [ "$file" -nt "$latest" ]; then
          latest="$file"
        fi
      done < <(find "$log_dir" -maxdepth 3 -type f 2>/dev/null)
      if [ -n "$latest" ]; then
        printf "%s\n\n" "$latest"
        tail -80 "$latest"
      else
        printf "No log files yet.\n"
      fi
    else
      printf "No log directory yet.\n"
    fi
    sleep 5
  done
}

run_suites() {
  clear
  printf "Test suites\n\n"
  find tests -mindepth 1 -maxdepth 1 -type d -exec basename {} \; | sort
  printf "\nDefaults\n"
  grep -H "name_main_test_default" tests/*/config.R 2>/dev/null || true
  exec "${SHELL:-/bin/bash}"
}

run_slurm() {
  clear
  if command -v squeue >/dev/null 2>&1; then
    watch -n 10 "squeue -u '$USER'"
  else
    printf "squeue not found on this machine.\n"
    exec "${SHELL:-/bin/bash}"
  fi
}

case "${1:-}" in
  --help-pane) run_help ;;
  --status) run_status ;;
  --files) run_files ;;
  --logs) run_logs ;;
  --suites) run_suites ;;
  --slurm) run_slurm ;;
esac

if ! command -v tmux >/dev/null 2>&1; then
  printf "tmux is not installed or not on PATH.\n" >&2
  exit 1
fi

if tmux has-session -t "$SESSION" 2>/dev/null; then
  exec tmux attach -t "$SESSION"
fi

run_cmd="$(make_words run_test "TEST_SUITE=$TEST_SUITE" "TEST_NAME=$TEST_NAME")"
slurm_cmd="$(make_words run_test_slurm "SLURM_DRY_RUN=1" "TEST_SUITE=$TEST_SUITE" "TEST_NAME=$TEST_NAME")"
inspect_cmd="$(make_words inspect_results "TEST_SUITE=$TEST_SUITE" "TEST_NAME=$TEST_NAME")"

tmux new-session -d -s "$SESSION" -n main -c "$ROOT" "./scripts/tmux.sh --status"
tmux split-window -h -t "$SESSION:main" -c "$ROOT" "./scripts/tmux.sh --help-pane"
tmux split-window -v -t "$SESSION:main.1" -c "$ROOT" "./scripts/tmux.sh --status"
tmux select-layout -t "$SESSION:main" main-vertical

tmux new-window -t "$SESSION:" -n run -c "$ROOT"
tmux send-keys -t "$SESSION:run.0" "$run_cmd"
tmux split-window -h -t "$SESSION:run" -c "$ROOT" "./scripts/tmux.sh --files"
tmux split-window -v -t "$SESSION:run.1" -c "$ROOT" "./scripts/tmux.sh --logs"
tmux select-layout -t "$SESSION:run" main-vertical

tmux new-window -t "$SESSION:" -n slurm -c "$ROOT"
tmux send-keys -t "$SESSION:slurm.0" "$slurm_cmd"
tmux split-window -h -t "$SESSION:slurm" -c "$ROOT" "./scripts/tmux.sh --slurm"
tmux select-layout -t "$SESSION:slurm" even-horizontal

tmux new-window -t "$SESSION:" -n inspect -c "$ROOT"
tmux send-keys -t "$SESSION:inspect.0" "$inspect_cmd"
tmux split-window -h -t "$SESSION:inspect" -c "$ROOT" "./scripts/tmux.sh --suites"
tmux select-layout -t "$SESSION:inspect" even-horizontal

tmux select-window -t "$SESSION:main"
exec tmux attach -t "$SESSION"
