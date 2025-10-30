## Install devtools if missing ---
if (!("devtools" %in% installed.packages()[, "Package"])) {
  install.packages("devtools")
}

## Check if femR is already installed and install it if not
if ("femR" %in% installed.packages()[, "Package"]) {
  # remove.packages("femR")
  cat("The package femR is already installed")
} else {
  devtools::install_github("fdaPDE/femR", ref = "stable")
}
