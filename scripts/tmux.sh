#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

SESSION="${SESSION:-RGCCA}"

run_watch() {
  while :; do
    clear
    date
    printf "\nSlurm jobs\n"
    if command -v squeue >/dev/null 2>&1; then
      squeue -u "$USER" 2>/dev/null || true
    else
      printf "squeue not found\n"
    fi

    printf "\nLocal test processes\n"
    ps -Ao pid,etime,command 2>/dev/null | grep -E "tests/run_tests|run_tests_|src/init.R|aggregate_results|Rscript.*tests/" | grep -v grep || printf "none\n"

    printf "\nNewest logs\n"
    find tmp logs results -maxdepth 4 -type f \( -name "*.log" -o -name "*.out" -o -name "*.err" \) 2>/dev/null | sort | tail -20 || true
    sleep 5
  done
}

case "${1:-}" in
  --watch) run_watch ;;
esac

if ! command -v tmux >/dev/null 2>&1; then
  printf "tmux is not installed or not on PATH.\n" >&2
  exit 1
fi

if tmux has-session -t "$SESSION" 2>/dev/null; then
  exec tmux attach -t "$SESSION"
fi

tmux new-session -d -s "$SESSION" -n main -c "$ROOT" "clear; exec ${SHELL:-/bin/bash}"
tmux split-window -h -p 40 -t "$SESSION:main" -c "$ROOT" "./scripts/tmux.sh --watch"

tmux select-window -t "$SESSION:main"
exec tmux attach -t "$SESSION"
