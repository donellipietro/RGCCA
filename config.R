# = ========================================================================== =
# - Script: config.R
# - Desc: Editable runtime profiles for this repository.
# = ========================================================================== =

source("src/utils/config.R")


# Profiles ----

TESTBENCH_CONFIG_PROFILES <- list(
  macbook = local({
    PATH_REPO <- "/Users/pietrodonelli/Documents/University/projects/RGCCA"
    PATH_TMP <- file.path(PATH_REPO, "tmp")
    PATH_FDAPDE_CPP <- "/Users/pietrodonelli/Documents/University/fdaPDE/fdaPDE-cpp"

    list(
      PATH_REPO = PATH_REPO,

      # Generated folders. Move these if you want outputs on another disk.
      PATH_RESULTS = file.path(PATH_REPO, "results"),
      PATH_IMAGES = file.path(PATH_REPO, "images"),
      PATH_TEST_DATA = file.path(PATH_REPO, "data/tests"),
      PATH_TMP = PATH_TMP,
      PATH_QUEUE = file.path(PATH_TMP, "queue"),
      PATH_LOGS = file.path(PATH_TMP, "logs"),
      PATH_TMP_DATA = file.path(PATH_TMP, "data"),
      PATH_TMP_RESULTS = file.path(PATH_TMP, "results"),
      PATH_BUILD = file.path(PATH_REPO, "build"),

      # Compiler
      CC = "/opt/homebrew/bin/gcc-15",
      CXX = "/opt/homebrew/bin/g++-15",
      PATH_FDAPDE_CPP = PATH_FDAPDE_CPP,
      PATH_FDAPDE_CORE = file.path(PATH_FDAPDE_CPP, "fdaPDE/core"),
      PATH_IPOPT_INCLUDE = "/opt/homebrew/opt/ipopt/include/coin-or",
      PATH_IPOPT_LIB = "/opt/homebrew/opt/ipopt/lib",
      PATH_EIGEN_INCLUDE = "/opt/homebrew/include/eigen3",

      # If SINGULARITY_IMAGE is empty, compile on the host machine.
      SINGULARITY_IMAGE = "",
      SINGULARITY_BIND_PATHS = PATH_REPO,
      R_LIBS_USER = Sys.getenv("R_LIBS_USER", unset = ""),
      R_LIBS_SITE = Sys.getenv("R_LIBS_SITE", unset = "")
    )
  }),
  donders_hcp = local({
    PATH_REPO <- "/home/preclineu/piedon/Documents/RGCCA"
    PATH_OUTPUT <- "/project/3022000.05/piedon/RGCCA"
    PATH_TMP <- file.path(PATH_OUTPUT, "tmp")

    list(
      PATH_REPO = PATH_REPO,

      # Generated folders. Defaults keep heavy outputs off the home profile.
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
      PATH_FDAPDE_CPP = "",
      PATH_FDAPDE_CORE = "",
      PATH_IPOPT_INCLUDE = "",
      PATH_IPOPT_LIB = "",
      PATH_EIGEN_INCLUDE = "",

      # If SINGULARITY_IMAGE is filled, compile through Singularity.
      SINGULARITY_IMAGE = "",
      SINGULARITY_BIND_PATHS = paste(c(PATH_REPO, PATH_TMP), collapse = ","),
      DEFAULT_CPUS = 1,
      HEAVY_CPUS = 20,
      DEFAULT_MEM = "16GB",
      HEAVY_MEM = "32GB",
      DEFAULT_TIME = "12:00:00",
      HEAVY_TIME = "72:00:00",
      R_LIBS_USER = "/home/preclineu/piedon/R/x86_64-pc-linux-gnu-library/4.3",
      R_LIBS_SITE = "/opt/R-packages/4.3.3:/opt/R/4.3.3/lib64/R/library"
    )
  })
)


# Command line entry point ----

if (config_is_cli("config.R")) {
  config_main()
}
