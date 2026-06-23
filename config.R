# = ========================================================================== =
# - Script: config.R
# - Desc: Editable runtime profiles for this repository.
# = ========================================================================== =

source("src/utils/config.R")


# Profiles ----

TESTBENCH_CONFIG_PROFILES <- list(
  macbook = local({
    PATH_REPO <- "/Users/pietrodonelli/Documents/University/projects/RGCCA"
    PATH_OUTPUT <- "/Users/pietrodonelli/Documents/University/projects/RGCCA"
    PATH_TMP <- file.path(PATH_OUTPUT, "tmp")
    PATH_FDAPDE_CPP <- "/Users/pietrodonelli/Documents/University/fdaPDE/fdaPDE-cpp"

    list(
      PATH_REPO = PATH_REPO,

      # Generated folders. Move these if you want outputs on another disk.
      PATH_RESULTS = file.path(PATH_OUTPUT, "results"),
      PATH_IMAGES = file.path(PATH_OUTPUT, "images"),
      PATH_TEST_DATA = file.path(PATH_OUTPUT, "data/tests"),
      PATH_TMP = PATH_TMP,
      PATH_QUEUE = file.path(PATH_TMP, "queue"),
      PATH_LOGS = file.path(PATH_TMP, "logs"),
      PATH_TMP_DATA = file.path(PATH_TMP, "data"),
      PATH_TMP_RESULTS = file.path(PATH_TMP, "results"),
      PATH_BUILD = file.path(PATH_OUTPUT, "build"),

      # Compiler
      CC = "/opt/homebrew/bin/gcc-15",
      CXX = "/opt/homebrew/bin/g++-15",
      PATH_FDAPDE_CPP = PATH_FDAPDE_CPP,
      PATH_FDAPDE_CORE = file.path(PATH_FDAPDE_CPP, "fdaPDE/core"),
      PATH_IPOPT_INCLUDE = "/opt/homebrew/opt/ipopt/include/coin-or",
      PATH_IPOPT_LIB = "/opt/homebrew/opt/ipopt/lib",
      PATH_EIGEN_INCLUDE = "/opt/homebrew/include/eigen3",
      IPOPT_LINEAR_SOLVER = "ma57",
      IPOPT_HSL_LIBRARY = "/Users/pietrodonelli/local/hsl/lib/libcoinhsl.dylib",

      # If SINGULARITY_IMAGE is empty, compile on the host machine.
      SINGULARITY_IMAGE = "",
      SINGULARITY_BIND_PATHS = PATH_REPO,
      MULTITHREAD_CPUS = 12,
      MULTITHREAD_MEM = "32GB",
      MULTITHREAD_TIME = "12:00:00",
      R_CRAN_REPO = "https://cloud.r-project.org",
      R_LIBS_USER = Sys.getenv("R_LIBS_USER", unset = ""),
      R_LIBS_SITE = Sys.getenv("R_LIBS_SITE", unset = "")
    )
  }),
  donders_hcp = local({
    PATH_HOME <- "/home/preclineu/piedon"
    PATH_REPO <- file.path(PATH_HOME, "Documents/RGCCA")
    PATH_OUTPUT <- "/project/3022000.05/piedon/RGCCA"
    PATH_TMP <- file.path(PATH_OUTPUT, "tmp")
    PATH_FDAPDE_CPP <- file.path(PATH_HOME, "fdaPDE-cpp")
    PATH_HSL <- file.path(PATH_HOME, "local/hsl")

    list(
      PATH_REPO = PATH_REPO,

      # Generated folders. Defaults keep large outputs off the home profile.
      PATH_RESULTS = file.path(PATH_OUTPUT, "results"),
      PATH_IMAGES = file.path(PATH_OUTPUT, "images"),
      PATH_TEST_DATA = file.path(PATH_OUTPUT, "data/tests"),
      PATH_TMP = PATH_TMP,
      PATH_QUEUE = file.path(PATH_TMP, "queue"),
      PATH_LOGS = file.path(PATH_TMP, "logs"),
      PATH_TMP_DATA = file.path(PATH_TMP, "data"),
      PATH_TMP_RESULTS = file.path(PATH_TMP, "results"),
      PATH_BUILD = file.path(PATH_OUTPUT, "build"),

      # Use paths as seen by the compiler. If SINGULARITY_IMAGE is set,
      # these should be the paths inside the container.
      CC = "gcc",
      CXX = "g++",
      PATH_FDAPDE_CPP = PATH_FDAPDE_CPP,
      PATH_FDAPDE_CORE = file.path(PATH_FDAPDE_CPP, "fdaPDE/core"),
      PATH_IPOPT_INCLUDE = "/usr/include/coin-or/",
      PATH_IPOPT_LIB = "/usr/lib/ipopt",
      PATH_EIGEN_INCLUDE = "/usr/include/eigen3",
      IPOPT_LINEAR_SOLVER = "ma57",
      IPOPT_HSL_LIBRARY = "/home/preclineu/piedon/local/hsl/lib64/libcoinhsl.so",

      # If SINGULARITY_IMAGE is filled, compile through Singularity.
      SINGULARITY_IMAGE = file.path(PATH_HOME, "fdapde-docker_ipopt.sif"),
      SINGULARITY_BIND_PATHS = paste(
        unique(c(PATH_REPO, PATH_OUTPUT, PATH_FDAPDE_CPP, PATH_HSL)),
        collapse = ","
      ),
      DEFAULT_CPUS = 1,
      DEFAULT_MEM = "16GB",
      DEFAULT_TIME = "12:00:00",
      MULTITHREAD_CPUS = 20,
      MULTITHREAD_MEM = "32GB",
      MULTITHREAD_TIME = "12:00:00",
      R_CRAN_REPO = "https://cloud.r-project.org",
      R_LIBS_USER = "/home/preclineu/piedon/R/x86_64-pc-linux-gnu-library/4.3",
      R_LIBS_SITE = "/opt/R-packages/4.3.3:/opt/R/4.3.3/lib64/R/library"
    )
  })
)


# Command line entry point ----

if (config_is_cli("config.R")) {
  config_main()
}
