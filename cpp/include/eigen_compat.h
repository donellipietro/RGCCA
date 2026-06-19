#pragma once

#include <Eigen/Core>

#ifdef EIGEN_COMPAT_FORCE_PLACEHOLDER_ALL
// TODO: remove this with the matching flag in cpp/compile.sh once the
// Singularity image exposes Eigen::all.
namespace Eigen {
using placeholders::all;
}
#endif
