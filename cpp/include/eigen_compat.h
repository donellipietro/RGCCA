#pragma once

#include <Eigen/Core>

#if !EIGEN_VERSION_AT_LEAST(3, 4, 0)
namespace Eigen {
using placeholders::all;
}
#endif
