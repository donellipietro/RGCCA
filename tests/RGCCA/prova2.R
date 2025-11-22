library(neuRosim)

simulate_fmri_design_cov <- function(
    P, TR = 2, totaltime = 600,
    onsets_list, durations_list,
    betas = NULL,
    base = 0,
    noise = c("temporal"),
    SNR = 1,
    ar_rho = 0.3,
    freq_low = 128, freq_heart = 1.17, freq_resp = 0.20,
    seed = NULL
){
  if (!is.null(seed)) set.seed(seed)
  noise <- match.arg(noise)
  
  # ----- 1) HRF-convolved design -----
  K <- length(onsets_list); stopifnot(K == length(durations_list))
  
  # IMPORTANT: effectsize as a LIST (one entry per condition)
  eff_list <- as.list(rep(1, K))
  
  # (a) Prepare temporal design object (optional, useful if you later want simTSfmri task-related)
  design_list <- simprepTemporal(
    totaltime = totaltime,
    onsets    = onsets_list,
    durations = durations_list,
    TR        = TR,
    effectsize= eff_list,          # <-- list, not numeric vector
    hrf       = "double-gamma"
  )
  
  # (b) Build the actual design matrix X (T x K)
  X <- specifydesign(
    onsets    = onsets_list,
    durations = durations_list,
    totaltime = totaltime,
    TR        = TR,
    effectsize= eff_list,          # <-- list, not numeric vector
    conv      = "double-gamma"
  )
  Tpts <- nrow(X)
  
  # ----- 2) Region-specific response weights (P x K) -----
  if (is.null(betas)) {
    betas <- matrix(NA_real_, nrow = P, ncol = K)
    latent <- matrix(rnorm(K * 2, sd = 0.7), ncol = 2)
    for (p in 1:P) betas[p,] <- 0.6 * (latent %*% rnorm(2)) + rnorm(K, sd = 0.4)
  } else {
    stopifnot(is.matrix(betas), nrow(betas) == P, ncol(betas) == K)
  }
  
  # ----- 3) Noise generator (per ROI) -----
  gen_noise <- function(){
    neuRosim::simTSfmri(
      design = list(),   # empty => noise-only series
      base   = base,
      nscan  = Tpts,
      TR     = TR,
      SNR    = SNR,
      noise  = noise,
      type   = "gaussian",
      rho        = ar_rho,
      freq.low   = freq_low,
      freq.heart = freq_heart,
      freq.resp  = freq_resp,
      verbose    = FALSE
    )
  }
  
  # ----- 4) Compose Y = X %*% beta_p + noise_p -----
  Y <- matrix(NA_real_, nrow = Tpts, ncol = P)
  for (p in 1:P) {
    act <- as.numeric(X %*% betas[p, ])
    eps <- gen_noise()
    Y[, p] <- base + act + eps
  }
  colnames(Y) <- paste0("roi_", seq_len(P))
  
  list(
    Y = Y,
    TR = TR,
    times = seq(0, by = TR, length.out = Tpts),
    X = X,
    betas = betas,
    sample_cov = crossprod(scale(Y, center = TRUE, scale = FALSE)) / nrow(Y)
  )
}

# ---------- Minimal example ----------
TR <- 2; totaltime <- 600
onsets_list <- list(
  c(20, 140, 260, 380, 500),
  c(60, 180, 300, 420, 540),
  c(100, 220, 340, 460, 580)
)
durations_list <- list(rep(16,5), rep(16,5), rep(16,5))

set.seed(42)
sim <- simulate_fmri_design_cov(
  P = 6, TR = TR, totaltime = totaltime,
  onsets_list = onsets_list, durations_list = durations_list,
  noise = "temporal", SNR = 1, ar_rho = 0.4
)

round(sim$sample_cov, 3)