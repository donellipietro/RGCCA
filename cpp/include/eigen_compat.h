#pragma once

#include <Eigen/Core>

#ifndef EIGEN_VERSION_AT_LEAST
#define RGCCA_EIGEN_VERSION_AT_LEAST(x, y, z) \
  (EIGEN_WORLD_VERSION > x || \
   (EIGEN_WORLD_VERSION >= x && \
    (EIGEN_MAJOR_VERSION > y || \
     (EIGEN_MAJOR_VERSION >= y && EIGEN_MINOR_VERSION >= z))))
#else
#define RGCCA_EIGEN_VERSION_AT_LEAST(x, y, z) EIGEN_VERSION_AT_LEAST(x, y, z)
#endif

#if !RGCCA_EIGEN_VERSION_AT_LEAST(3, 4, 0)
namespace Eigen {
using placeholders::all;
}
#endif

#undef RGCCA_EIGEN_VERSION_AT_LEAST
