# --- Simulate fMRI-like time series with target covariance across P regions ---
# Depends optionally on neuRosim (for HRF); otherwise uses a built-in SPM-like HRF.
#
# Sigma:      P x P SPD covariance to match (empirical across time)
# P:          number of time series (regions)
# T:          number of time points
# TR:         repetition time (seconds)
# fire_rate:  expected spikes per second (per region) for Bernoulli events
# refractory: refractory period in seconds after a spike (integer multiple of TR)
# hrf_dur:    HRF support duration in seconds
# ar1_rho:    optional AR(1) noise coefficient (0 = none)
# noise_sd:   SD of added noise before covariance matching
# seed:       RNG seed
simulate_fmri_with_cov <- function(Sigma, 
                                   P = nrow(Sigma),
                                   T = 300,
                                   TR = 2,
                                   fire_rate = 0.1,
                                   refractory = 4,     # seconds
                                   hrf_dur = 32,
                                   ar1_rho = 0.0,
                                   noise_sd = 0.0,
                                   seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  stopifnot(is.matrix(Sigma), nrow(Sigma) == ncol(Sigma), nrow(Sigma) == P)
  
  # --- Cholesky of target covariance (regularize if needed) ---
  p <- P
  Sigma_sym <- (Sigma + t(Sigma))/2
  eig <- eigen(Sigma_sym, symmetric = TRUE)
  eig$values[eig$values < 1e-10] <- 1e-10
  Sigma_reg <- eig$vectors %*% diag(eig$values, p) %*% t(eig$vectors)
  L <- chol(Sigma_reg)
  
  # --- Time grid ---
  times <- (0:(T-1)) * TR
  
  # --- HRF: try neuRosim if present; else SPM-like double gamma ---
  build_hrf <- function(TR, duration) {
    if (requireNamespace("neuRosim", quietly = TRUE)) {
      # neuRosim provides HRFs; use canonical double-gamma (SPM-like)
      # 'neuRosim::spmHRF' exists in recent versions; fallback to 'hrf' if needed.
      if ("spmHRF" %in% getNamespaceExports("neuRosim")) {
        return(as.numeric(neuRosim::spmHRF(TR=TR, duration=duration)))
      } else if ("hrf" %in% getNamespaceExports("neuRosim")) {
        # neuRosim::hrf(type="double-gamma", ...)
        return(as.numeric(neuRosim::hrf(hrf="double-gamma", onsets=0, durations=0,
                                        totaltime=duration, TR=TR)))
      }
    }
    # Fallback: SPM-like double gamma
    # Parameters roughly matching SPM canonical HRF
    t <- seq(0, duration, by = TR)
    gg <- function(t, p, d) (t^(p-1) * exp(-t/d)) / ((d^p) * gamma(p))
    h <- gg(t, p=6, d=1) - 0.35 * gg(t, p=12, d=1)
    h <- h / sum(abs(h))
    as.numeric(h)
  }
  hrf <- build_hrf(TR, hrf_dur)
  
  # --- Generate spike trains with refractory constraint ---
  # Bernoulli with prob = fire_rate * TR per time step
  prob <- min(0.99, max(0, fire_rate * TR))
  ref_steps <- max(0, round(refractory / TR))
  E <- matrix(0L, T, P)
  for (j in 1:P) {
    t <- 1
    while (t <= T) {
      if (runif(1) < prob) {
        E[t, j] <- 1L
        t <- t + max(1, ref_steps)  # enforce refractory
      } else {
        t <- t + 1
      }
    }
  }
  
  # --- Convolution with HRF (per region) ---
  conv1 <- function(x, h) {
    y <- convolve(x, rev(h), type = "open")
    y[1:length(x)]
  }
  Y <- apply(E, 2, conv1, h = hrf)
  
  # --- Optional AR(1) + white noise (still before covariance matching) ---
  if (ar1_rho != 0) {
    ar_filter <- function(x, rho) {
      z <- numeric(length(x)); z[1] <- x[1]
      for (t in 2:length(x)) z[t] <- rho * z[t-1] + x[t]
      z
    }
    Y <- apply(Y, 2, ar_filter, rho = ar1_rho)
  }
  if (noise_sd > 0) {
    Y <- Y + matrix(rnorm(T * P, sd = noise_sd), T, P)
  }
  
  # --- Center each series (doesn't affect covariance) ---
  Y <- scale(Y, center = TRUE, scale = FALSE)
  
  # --- Whiten across channels and mix to impose Sigma ---
  # Empirical covariance across time (columns are regions)
  S_y <- crossprod(Y) / T
  eigS <- eigen((S_y + t(S_y))/2, symmetric = TRUE)
  S_inv_sqrt <- eigS$vectors %*% diag(1/sqrt(pmax(eigS$values, 1e-12)), p) %*% t(eigS$vectors)
  Y_wh <- Y %*% S_inv_sqrt
  X <- Y_wh %*% t(L)
  X <- scale(X, center = TRUE, scale = FALSE)
  
  achieved_cov <- crossprod(X) / T
  list(
    times = times,
    spikes = E,                # T x P spike trains
    hrf = hrf,                 # HRF used
    raw_bold = Y,              # pre-matching convolved signals
    X = X,                     # final signals with target covariance
    achieved_cov = achieved_cov,
    target_cov = Sigma_reg,
    max_abs_cov_error = max(abs(achieved_cov - Sigma_reg))
  )
}

# --------------------------
# Example usage
# --------------------------
# Build a target covariance for P=4 regions
set.seed(123)
P <- 4
A <- diag(P)
B <- diag(P)
B[1,2] <- 0.5
B[2,1] <- 0.5
B[2,3] <- 0.8
B[3,2] <- 0.8
Sigma <- A %*% B %*% A


sim <- simulate_fmri_with_cov(
  Sigma = Sigma,
  P = P,
  T = 20*600/10,         # 20 minutes at TR=2s
  TR = 2/20,
  fire_rate = 0.08,
  refractory = 4,
  hrf_dur = 32,
  ar1_rho = 0.3,
  noise_sd = 0.00,
  seed = 42
)

# Inspect covariance match
sim$max_abs_cov_error
sim$achieved_cov
sim$target_cov

# Quick plots
op <- par(mfrow=c(2,1), mar=c(3,4,2,1))
matplot(sim$times, sim$X, type="l", lty=1, xlab="Time (s)", ylab="Simulated BOLD (cov-matched)")
plot(seq_along(sim$hrf)*2 - 2, sim$hrf, type="l", xlab="Lag (s)", ylab="HRF", main="HRF kernel")
par(op)