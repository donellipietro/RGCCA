#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage:
  ./cpp/compile.sh <model_name>
  ./cpp/compile.sh --all
USAGE

  if [[ -n "${PATH_CPP:-}" && -d "${PATH_CPP}" ]]; then
    echo ""
    echo "Available models:"
    list_models | sed 's/^/- /'
  fi
}

usage_make() {
  cat <<'USAGE'
Usage:
  make compile MODEL=<model_name>

Available models:
USAGE

  if [[ -n "${PATH_CPP:-}" && -d "${PATH_CPP}" ]]; then
    list_models | while IFS= read -r model; do
      printf -- '- %s (make compile MODEL=%s)\n' "${model}" "${model}"
    done
  fi
}

CPP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "${CPP_DIR}/.." && pwd)"

if [[ ! -f "${PROJECT_DIR}/.env" ]]; then
  echo "Error: .env not found. Run: make build TESTBENCH_PROFILE=<profile>" >&2
  exit 1
fi

set -a
source "${PROJECT_DIR}/.env"
set +a

path_required() {
  local name="$1"
  local value="${!name:-}"

  if [[ -z "${value}" ]]; then
    echo "Error: ${name} is not set. Check config.R and rebuild .env." >&2
    exit 1
  fi
}

list_models() {
  local model_dir model mains

  shopt -s nullglob
  for model_dir in "${PATH_CPP}"/*; do
    [[ -d "${model_dir}" ]] || continue
    model="$(basename "${model_dir}")"
    [[ "${model}" != "include" ]] || continue

    mains=("${model_dir}"/main*.cpp)
    if [[ "${#mains[@]}" -gt 0 ]]; then
      printf '%s\n' "${model}"
    fi
  done | sort
}

split_flags() {
  local value="$1"

  if [[ -n "${value}" ]]; then
    read -r -a SPLIT_FLAGS_RESULT <<< "${value}"
  else
    SPLIT_FLAGS_RESULT=()
  fi
}

add_if_set() {
  local -n target="$1"
  local prefix="$2"
  local value="$3"

  if [[ -n "${value}" ]]; then
    target+=("${prefix}${value}")
  fi
}

compile_flags() {
  local eigen_compat_header="${PATH_CPP}/include/eigen_compat.h"
  local eigen_compat_flags=()
  local configured_flags

  include_flags=()
  add_if_set include_flags "-I" "${PATH_FDAPDE_CPP:-}"
  add_if_set include_flags "-I" "${PATH_FDAPDE_CORE:-}"
  add_if_set include_flags "-I" "${PATH_IPOPT_INCLUDE:-}"
  add_if_set include_flags "-I" "${PATH_EIGEN_INCLUDE:-}"

  if [[ -n "${CXXFLAGS:-}" ]]; then
    split_flags "${CXXFLAGS}"
    configured_flags=("${SPLIT_FLAGS_RESULT[@]}")
  else
    configured_flags=(
      -O3
      -Wno-psabi
      -std=c++20
      -march=native
      -DFDAPDE_ENABLE_COUT
    )
  fi

  # Temporary workaround for the Eigen version in the current Singularity image.
  # Remove this block and cpp/include/eigen_compat.h once the image exposes Eigen::all.
  if [[ -n "${SINGULARITY_IMAGE:-}" ]]; then
    if [[ ! -f "${eigen_compat_header}" ]]; then
      echo "Error: Eigen compatibility header not found: ${eigen_compat_header}" >&2
      exit 1
    fi

    eigen_compat_flags=(
      -DEIGEN_COMPAT_FORCE_PLACEHOLDER_ALL
      -include
      "${eigen_compat_header}"
    )
  fi

  cxx_flags=("${configured_flags[@]}" "${include_flags[@]}" "${eigen_compat_flags[@]}")

  if [[ -n "${LDFLAGS:-}" ]]; then
    split_flags "${LDFLAGS}"
    ld_flags=("${SPLIT_FLAGS_RESULT[@]}")
  else
    ld_flags=()
    add_if_set ld_flags "-L" "${PATH_IPOPT_LIB:-}"
  fi

  if [[ -n "${LDLIBS:-}" ]]; then
    split_flags "${LDLIBS}"
    ld_libs=("${SPLIT_FLAGS_RESULT[@]}")
  else
    ld_libs=(-lipopt)
  fi
}

binary_name_for_source() {
  local base="$1"
  local stem

  case "${base}" in
    main.cpp)
      printf 'fit_model\n'
      ;;
    main_*.cpp)
      stem="${base#main_}"
      stem="${stem%.cpp}"
      printf 'fit_model_%s\n' "${stem}"
      ;;
    *)
      return 1
      ;;
  esac
}

compile_model() {
  local model="$1"
  local model_dir="${PATH_CPP}/${model}"
  local build_dir="${PATH_BUILD}/${model}"
  local mains src base bin out

  if [[ ! -d "${model_dir}" ]]; then
    echo "Error: model directory not found: ${model_dir}" >&2
    exit 1
  fi

  shopt -s nullglob
  mains=("${model_dir}"/main*.cpp)
  if [[ "${#mains[@]}" -eq 0 ]]; then
    echo "No main*.cpp found in ${model_dir}" >&2
    exit 1
  fi

  printf '\nCompiling mains in cpp/%s ...\n' "${model}"
  "${CPP_DIR}/run.sh" --describe
  echo "Compiler: ${CXX:-g++}"

  mkdir -p "${build_dir}"
  compile_flags

  for src in "${mains[@]}"; do
    base="$(basename "${src}")"
    bin="$(binary_name_for_source "${base}")"
    out="${build_dir}/${bin}"

    if [[ ! -f "${out}" || "${src}" -nt "${out}" ]]; then
      echo "- ${src}  ==>  ${out}"
      cmd=(
        "${CXX:-g++}" \
        "${cxx_flags[@]}" \
        -o "${out}" \
        "${src}" \
        "${ld_flags[@]}" \
        "${ld_libs[@]}"
      )
      printf '  Command:'
      printf ' %q' "${cmd[@]}"
      printf '\n'
      "${CPP_DIR}/run.sh" --quiet -- "${cmd[@]}"
    else
      echo "- ${out} is up to date"
    fi
  done

  printf 'All the source files have been compiled!\n\n'
}

path_required PATH_CPP
path_required PATH_BUILD

if [[ $# -eq 1 && -z "${1}" ]]; then
  set --
fi

case "${1:-}" in
  -h|--help)
    usage
    exit 0
    ;;
  --make-help)
    usage_make
    exit 0
    ;;
  --all)
    mapfile -t models < <(list_models)
    if [[ "${#models[@]}" -eq 0 ]]; then
      echo "No models with main*.cpp found in ${PATH_CPP}" >&2
      exit 1
    fi

    printf '\nCompiling all models in %s...\n' "${PATH_CPP}"
    for model in "${models[@]}"; do
      compile_model "${model}"
    done
    printf 'All models compiled successfully.\n\n'
    ;;
  "")
    usage
    exit 1
    ;;
  *)
    compile_model "$1"
    ;;
esac
