##############################
## Spiking experiment toolbox
##############################

## ---- palettes (robust fallbacks) ----
get_region_cols <- function(P) {
  if (requireNamespace("viridis", quietly = TRUE)) {
    viridis::viridis(P, option = "D")
  } else {
    hcl.colors(P, "Dark 3")
  }
}
get_task_cols <- function(K) {
  if (requireNamespace("RColorBrewer", quietly = TRUE)) {
    RColorBrewer::brewer.pal(min(8, K), "Set2")[1:K]
  } else {
    hcl.colors(K, "Pastel 1")
  }
}

## ---- High-res task boxcars at neural dt ----
# Build a per-time design matrix U (T x K) from onsets/durations at resolution dt
build_design_dt <- function(totaltime, dt, onsets_list, durations_list, amplitude = NULL) {
  K <- length(onsets_list); stopifnot(K == length(durations_list))
  Tn <- round(totaltime / dt)
  times <- seq(0, by = dt, length.out = Tn)
  if (is.null(amplitude)) amplitude <- rep(1, K)
  
  U <- matrix(0, nrow = Tn, ncol = K)
  for (k in 1:K) {
    ons <- onsets_list[[k]]; durs <- durations_list[[k]]
    stopifnot(length(ons) == length(durs))
    for (i in seq_along(ons)) {
      t0 <- ons[i]; t1 <- ons[i] + durs[i]
      idx <- which(times >= t0 & times < t1)
      if (length(idx)) U[idx, k] <- amplitude[k]
    }
  }
  colnames(U) <- paste0("task_", seq_len(K))
  list(U = U, times = times)
}

## ---- Spike simulator (prob depends on dt, not TR) ----
simulate_spikes_dt <- function(U, B, bias = NULL,
                               dt = 0.005,             # 5 ms
                               refractory_sec = 0.010,  # absolute refractory time (s)
                               max_rate_hz = 40,        # peak rate when saturated
                               gain = 2.0,
                               seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  Tn <- nrow(U); K <- ncol(U); P <- nrow(B); stopifnot(ncol(B) == K)
  if (is.null(bias)) bias <- rep(-3, P)  # low baseline firing
  stopifnot(length(bias) == P)
  
  logistic  <- function(x) 1 / (1 + exp(-x))
  ref_steps <- max(1L, ceiling(refractory_sec / dt))
  
  E <- matrix(0L, nrow = Tn, ncol = P)
  colnames(E) <- if (!is.null(rownames(B))) rownames(B) else paste0("r_", seq_len(P))
  
  for (p in 1:P) {
    t <- 1L
    while (t <= Tn) {
      drive_t <- bias[p] + sum(B[p, ] * U[t, ])
      p_fire  <- max_rate_hz * dt * logistic(gain * drive_t)
      if (p_fire > 1) p_fire <- 1
      if (runif(1) < p_fire) {
        E[t, p] <- 1L
        t <- t + ref_steps
      } else {
        t <- t + 1L
      }
    }
  }
  E
}

## ---- Rate estimation (Gaussian kernel) ----
# Convolve spikes with a Gaussian kernel (area = 1) to get instantaneous rate (Hz)
gaussian_rate <- function(E, dt, sigma = 0.050) {
  Tn <- nrow(E); P <- ncol(E)
  halfw <- ceiling(4 * sigma / dt)
  tgrid <- (-halfw:halfw) * dt
  k <- exp(-0.5 * (tgrid / sigma)^2)
  k <- k / sum(k * dt) # integral 1
  R <- matrix(0, Tn, P)
  for (p in 1:P) {
    r <- convolve(E[, p], rev(k), type = "open")
    R[, p] <- r[seq_len(Tn)]
  }
  colnames(R) <- colnames(E)
  R
}

## ---- ON/OFF rate summaries per task ----
on_off_summary <- function(R, U) {
  P <- ncol(R); K <- ncol(U)
  out <- vector("list", K)
  for (k in 1:K) {
    on  <- U[, k] > 0
    off <- !on
    tab <- data.frame(
      region   = colnames(R),
      rate_on  = if (any(on))  colMeans(R[on,  , drop = FALSE]) else rep(NA, P),
      rate_off = if (any(off)) colMeans(R[off, , drop = FALSE]) else rep(NA, P)
    )
    tab$delta <- tab$rate_on - tab$rate_off
    out[[k]] <- tab
  }
  names(out) <- colnames(U)
  out
}

## ---- HRF kernel (SPM-like double-gamma) at dt ----
hrf_double_gamma <- function(dt, duration = 32,
                             p1 = 6, d1 = 1,
                             p2 = 12, d2 = 1, a2 = 0.35,
                             normalize = c("area", "peak")) {
  normalize <- match.arg(normalize)
  t <- seq(0, duration, by = dt)
  gg <- function(t, p, d) (t^(p - 1) * exp(-t / d)) / ((d^p) * gamma(p))
  h <- gg(t, p1, d1) - a2 * gg(t, p2, d2)
  
  if (normalize == "area") {
    h <- h / sum(h * dt)          # unit area
  } else {
    h <- h / max(abs(h))          # unit peak
  }
  list(h = as.numeric(h), t = t)
}

## ---- Column-wise convolution, keep original length ----
conv_cols <- function(X, k) {
  Tn <- nrow(X); P <- ncol(X)
  Y <- matrix(0, Tn, P)
  for (p in 1:P) {
    y_full <- convolve(X[, p], rev(k), type = "open")
    Y[, p] <- y_full[seq_len(Tn)]
  }
  colnames(Y) <- colnames(X)
  Y
}

## ---- Neural (rate Hz) -> latent BOLD via HRF ----
apply_hrf <- function(R, dt, duration = 32, normalize = "area", nv_scale = NULL) {
  ker <- hrf_double_gamma(dt, duration = duration, normalize = normalize)$h
  B <- conv_cols(R, ker)                 # same sampling as R
  if (!is.null(nv_scale)) {
    stopifnot(length(nv_scale) == ncol(R))
    B <- sweep(B, 2, nv_scale, `*`)      # region-wise neurovascular gain
  }
  B
}

## Low-pass then bin-average to TR (anti-alias)
downsample_dt_to_TR <- function(times_dt, X_dt, TR, dt, lp_sigma = 0.5,
                                method = c("mean", "center")) {
  method <- match.arg(method)
  Tn <- nrow(X_dt); P <- ncol(X_dt)
  
  # --- Low-pass with DC gain = 1 (unit-sum kernel) ---
  if (lp_sigma > 0) {
    halfw <- ceiling(4 * lp_sigma / dt)
    tgrid <- (-halfw:halfw) * dt
    gk <- exp(-0.5 * (tgrid / lp_sigma)^2)
    gk <- gk / sum(gk)
    X_f <- matrix(0, Tn, P)
    for (p in 1:P) {
      y <- convolve(X_dt[, p], rev(gk), type = "open")
      X_f[, p] <- y[seq_len(Tn)]
    }
  } else {
    X_f <- X_dt
  }
  
  # --- Build TR bins ---
  t0 <- min(times_dt); t1 <- max(times_dt)
  nTR <- floor((t1 - t0) / TR) + 1
  times_TR <- t0 + (0:(nTR - 1)) * TR
  
  bin_idx <- vector("list", nTR)
  for (i in 1:nTR) {
    a <- times_TR[i]; b <- a + TR
    bin_idx[[i]] <- which(times_dt >= a & times_dt < b)
  }
  
  # --- Aggregate to TR ---
  X_TR <- matrix(NA_real_, nrow = nTR, ncol = P)
  if (method == "mean") {
    for (i in 1:nTR) {
      idx <- bin_idx[[i]]
      if (length(idx)) X_TR[i, ] <- colMeans(X_f[idx, , drop = FALSE])
    }
  } else {
    centers <- times_TR + TR / 2
    X_TR <- apply(X_f, 2, function(col) approx(times_dt, col, xout = centers, rule = 2)$y)
    X_TR <- matrix(X_TR, ncol = P)
  }
  
  colnames(X_TR) <- colnames(X_dt)
  list(X_TR = X_TR, times_TR = times_TR, bin_idx = bin_idx)
}

## Build global physio + drift signals at dt (same for all regions, with small ROI jitter)
physio_drift_dt <- function(times, dt,
                            drift_amp = 0.003, drift_cutoff = 1/120,  # <~0.008 Hz
                            card_mu = 1.1,  card_var = 0.08, card_amp = 0.003, # ~1 Hz
                            resp_mu = 0.30, resp_var = 0.03, resp_amp = 0.002, # ~0.3 Hz
                            seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  t <- times
  
  # Slow drift as low-frequency cosine mixture
  # Frequencies uniformly between 0 and drift_cutoff
  nf <- 5
  freqs <- runif(nf, min = 0, max = drift_cutoff)
  phases <- runif(nf, 0, 2*pi)
  drift <- rowSums(sapply(1:nf, function(i) cos(2*pi*freqs[i]*t + phases[i])))
  drift <- scale(drift, center = TRUE, scale = TRUE)
  drift <- as.numeric(drift) * drift_amp
  
  # Cardiac & respiratory with slight frequency jitter over time
  # Frequency trajectories (slow random walk)
  rw <- function(mu, var, n) {
    f <- rep(mu, n)
    for (i in 2:n) f[i] <- max(1e-6, f[i-1] + rnorm(1, sd = var * 0.005))
    f
  }
  f_card <- rw(card_mu,  card_var, length(t))
  f_resp <- rw(resp_mu,  resp_var, length(t))
  
  # Integrate phase → oscillation
  phi_card <- cumsum(2*pi * f_card * dt)
  phi_resp <- cumsum(2*pi * f_resp * dt)
  
  cardiac <- sin(phi_card); respiratory <- sin(phi_resp)
  cardiac <- cardiac / sd(cardiac) * card_amp
  respiratory <- respiratory / sd(respiratory) * resp_amp
  
  # Global physio (shared), returned as matrix columns for convenience
  cbind(drift = drift, cardiac = cardiac, respiratory = respiratory)
}

## Add TR-domain noise to a matrix (nTR x P)
add_tr_noise_snr <- function(
    X_TR_clean,                 # nTR x P, clean downsampled BOLD in percent-signal units
    SNR = 4,                    # target SNR (scalar or length-P vector)
    ref_metric = c("std", "rms", "peak", "psc"),
    ref_value = NULL,           # used only if ref_metric == "psc" (e.g., 0.03 for 3%)
    ar1_rho = 0.35,             # AR(1) coefficient at TR
    motion_prob = 0.004,        # probability of a motion spike per volume
    motion_amp  = 0.015,        # ~1.5% amplitude for motion spikes
    seed = NULL
){
  if (!is.null(seed)) set.seed(seed)
  ref_metric <- match.arg(ref_metric)
  nTR <- nrow(X_TR_clean); P <- ncol(X_TR_clean)
  
  # 1) Reference amplitude per ROI
  ref_amp <- numeric(P)
  for (p in 1:P) {
    x <- X_TR_clean[, p]
    if (ref_metric %in% c("std", "rms")) {
      ref_amp[p] <- sd(x, na.rm = TRUE)
    } else if (ref_metric == "peak") {
      ref_amp[p] <- max(abs(x), na.rm = TRUE)
    } else if (ref_metric == "psc") {
      if (is.null(ref_value)) stop("ref_value must be provided when ref_metric = 'psc'.")
      ref_amp[p] <- ref_value
    }
    # guard against degenerate zero
    if (!is.finite(ref_amp[p]) || ref_amp[p] <= 1e-10) ref_amp[p] <- 1e-3
  }
  
  # 2) Resolve SNR vector
  if (length(SNR) == 1) SNR <- rep(SNR, P)
  stopifnot(length(SNR) == P)
  
  # 3) White noise SD per ROI from target SNR
  white_sd <- ref_amp / SNR
  
  # 4) Add AR(1) + white at TR
  Y <- X_TR_clean
  for (p in 1:P) {
    eps <- rnorm(nTR, sd = white_sd[p])
    ar  <- numeric(nTR)
    ar[1] <- eps[1] / sqrt(max(1e-8, 1 - ar1_rho^2))
    for (t in 2:nTR) ar[t] <- ar1_rho * ar[t-1] + eps[t]
    Y[, p] <- Y[, p] + ar
  }
  
  # 5) Motion spikes (shared times, slight ROI variation)
  spikes <- rbinom(nTR, 1, motion_prob)
  if (any(spikes == 1)) {
    bump <- rnorm(P, mean = 1, sd = 0.15) * motion_amp
    for (t in which(spikes == 1)) Y[t, ] <- Y[t, ] + bump
  }
  
  list(Y = Y, white_sd = white_sd, ref_amp = ref_amp, spikes = spikes)
}

## ---- Plotting helpers with palettes ----
plot_spike_raster <- function(times, E, main = "Spike raster", region_cols = NULL, shade_alpha = 0.15) {
  P <- ncol(E); if (is.null(region_cols)) region_cols <- get_region_cols(P)
  plot(range(times), c(0.5, P + 0.5), type="n",
       xlab="Time (s)", ylab="Region", yaxt="n", main=main)
  axis(2, at=1:P, labels=colnames(E), las=1, cex.axis=0.8)
  abline(h = (1:P), col="grey90", lty=3)
  
  # draw shaded task periods
  usr <- par("usr")
  for (k in 1:K) {
    is_on <- which(U[, k] > 0)
    if (length(is_on)) {
      runs <- split(is_on, cumsum(c(1, diff(is_on) != 1)))
      for (idx in runs) {
        rect(times[min(idx)], usr[3], times[max(idx)], usr[4],
             border = NA, col = adjustcolor(task_cols[k], alpha.f = shade_alpha))
      }
    }
  }
  
  for (p in 1:P) {
    idx <- which(E[, p] == 1L)
    if (length(idx))
      points(times[idx], rep(p, length(idx)), pch=15, cex=0.35, col=region_cols[p])
  }

  # legend("topright", legend=colnames(E), col=region_cols, pch=15, bty="n", cex=0.8)
}

plot_tasks <- function(times, U, main = "Task inputs (boxcars)", task_cols = NULL) {
  K <- ncol(U); if (is.null(task_cols)) task_cols <- get_task_cols(K)
  matplot(times, U, type="s", lty=1, lwd=1.5,
          xlab="Time (s)", ylab="Level", main=main, col=task_cols)
  legend("topright", legend=colnames(U), lty=1, col=task_cols, bty="n", cex=0.8)
}

plot_rates_with_tasks <- function(times, R, U,
                                  main = "Firing rates (Hz)",
                                  region_cols = NULL, task_cols = NULL,
                                  shade_alpha = 0.15) {
  P <- ncol(R); K <- ncol(U)
  if (is.null(region_cols)) region_cols <- get_region_cols(P)
  if (is.null(task_cols))   task_cols   <- get_task_cols(K)
  
  # overlay region rates
  matplot(times, R, type="l", lty=1, lwd=1.2,
          xlab="Time (s)", ylab="Rate (Hz)", main=main, col=region_cols)
  
  # draw shaded task periods
  usr <- par("usr")
  for (k in 1:K) {
    is_on <- which(U[, k] > 0)
    if (length(is_on)) {
      runs <- split(is_on, cumsum(c(1, diff(is_on) != 1)))
      for (idx in runs) {
        rect(times[min(idx)], usr[3], times[max(idx)], usr[4],
             border = NA, col = adjustcolor(task_cols[k], alpha.f = shade_alpha))
      }
    }
  }
  
  # legend("topright", legend=colnames(R), col=region_cols, lty=1, bty="n", cex=0.8)
}


plot_bold_with_tasks <- function(times, B, U, main = "Latent BOLD (HRF-convolved)",
                                 region_cols = NULL, task_cols = NULL, shade_alpha = 0.12, ...) {
  P <- ncol(B); K <- ncol(U)
  if (is.null(region_cols)) region_cols <- get_region_cols(P)
  if (is.null(task_cols))   task_cols   <- get_task_cols(K)
  
  matplot(times, B, type = "l", lty = 1, lwd = 1.3,
          xlab = "Time (s)", ylab = "BOLD", main = main, col = region_cols, ...)
  
  # shade task blocks
  usr <- par("usr")
  for (k in 1:K) {
    is_on <- which(U[, k] > 0)
    if (length(is_on)) {
      runs <- split(is_on, cumsum(c(1, diff(is_on) != 1)))
      for (idx in runs) {
        rect(times[min(idx)], usr[3], times[max(idx)], usr[4],
             border = NA, col = adjustcolor(task_cols[k], alpha.f = shade_alpha))
      }
    }
  }
  # legend("topright", legend = colnames(B), col = region_cols, lty = 1, bty = "n", cex = 0.8)
}

##############################
## Example configuration
##############################

## Experiment
totaltime <- 240             # seconds
dt <- 5e-3                   # 5 ms bins

onsets_list <- list(         # three tasks with simple blocks
  c(20, 80, 140, 200),       # task_1
  c(40, 100, 160),           # task_2
  c(65, 125, 185)            # task_3
)
durations_list <- list(
  rep(15, 4),                # task_1 durations
  rep(20, 3),                # task_2 durations
  rep(10, 3)                 # task_3 durations
)

des   <- build_design_dt(totaltime, dt, onsets_list, durations_list)
U     <- des$U; times <- des$times; K <- ncol(U)

## Regions & influences (B: P x K)
P <- 5
B <- matrix(0, nrow = P, ncol = K,
            dimnames = list(paste0("r_", 1:P), colnames(U)))
B["r_1", 1] <- 1
B["r_3", 1] <- 1
B["r_2", 2] <- 1
B["r_5", 2] <- 1
B["r_1", 3] <- 1
B["r_4", 3] <- 1

bias <- rep(-2, P)

## Simulate spikes
E <- simulate_spikes_dt(
  U = U, B = B, bias = bias,
  dt = dt,
  refractory_sec = 20e-3,
  max_rate_hz = 20,
  gain = 2.0,
  seed = 99
)

## Estimate rates
sigma <- 0.50  # seconds (rate smoothing)
R <- gaussian_rate(E, dt, sigma = sigma)

## Palettes (consistent across plots)
region_cols <- get_region_cols(P)
task_cols   <- get_task_cols(K)

## Plots
op <- par(mfrow = c(3,1), mar = c(3,4,2,1))
plot_tasks(times, U, task_cols = task_cols, main = "Task inputs")
plot_spike_raster(times, E, region_cols = region_cols, main = "Spikes")
plot_rates_with_tasks(times, R, U,
                      region_cols = region_cols, task_cols = task_cols,
                      main = paste0("Firing rates (Gaussian σ = ", sigma, " s)"))
par(op)

## ON/OFF summaries (per task)
summ <- on_off_summary(R, U)
# Example: look at task_1 table
# summ$task_1


## --- 1) Latent BOLD at dt, scaled to percent signal change ---
# Use peak-normalized HRF, then scale to ~3% task peaks (tweak as you like)
B_latent <- apply_hrf(R, dt, duration = 32, normalize = "area")
B_latent <- B_latent / max(abs(B_latent)) * 0.03  # 3% signal change peak

## Plots
op <- par(mfrow = c(3,1), mar = c(3,4,2,1))
plot_tasks(times, U, task_cols = task_cols, main = "Task inputs")
plot_spike_raster(times, E, region_cols = region_cols, main = "Spikes")
plot_bold_with_tasks(times, B_latent, U, region_cols = region_cols, task_cols = task_cols,
                     main = paste0("Latent BOLD (HRF=", hrf_len_sec, "s, area-normalized)"), ylim = c(-0.005, 0.035))
par(op)


## --- 2) Add physio+drift at dt (global + small ROI jitter) ---
phys <- physio_drift_dt(times, dt,
                        drift_amp = 0.003,  # 0.3% drift
                        card_mu = 1.1,  card_var = 0.08, card_amp = 0.003,  # ~1 Hz, 0.3%
                        resp_mu = 0.30, resp_var = 0.03, resp_amp = 0.002,  # ~0.3 Hz, 0.2%
                        seed = 123)
# Mix global components with small region-wise weights
P <- ncol(B_latent)
mix_w <- matrix(rnorm(P * ncol(phys), sd = 0.3), P, ncol(phys))  # small ROI variability
phys_dt <- phys %*% t(mix_w)  # Tn x P
phys_dt <- scale(phys_dt, center = TRUE, scale = FALSE) # zero mean

B_dt_noisy <- B_latent + phys_dt

## Plots
op <- par(mfrow = c(3,1), mar = c(3,4,2,1))
plot_spike_raster(times, E, region_cols = region_cols, main = "Spikes")
plot_bold_with_tasks(times, B_latent, U, region_cols = region_cols, task_cols = task_cols,
                     main = paste0("Latent BOLD (HRF=", hrf_len_sec, "s)"), ylim = c(-0.005, 0.035))
plot_bold_with_tasks(times, B_dt_noisy, U, region_cols = region_cols, task_cols = task_cols,
                     main = paste0("BOLD (HRF=", hrf_len_sec, "s)"), ylim = c(-0.005, 0.035))
par(op)


## --- 3) Anti-alias + downsample to TR ---
TR <- 2.0  # seconds
ds <- downsample_dt_to_TR(times, B_latent, TR, dt, lp_sigma = 0.5)  # 0.5 s LP is mild
B_TR <- ds$X_TR
times_TR <- ds$times_TR
bin_idx <- ds$bin_idx
U_TR <- do.call(rbind, lapply(bin_idx, function(idx) colMeans(U[idx, , drop = FALSE])))
colnames(U_TR) <- colnames(U)

## --- 4) Add TR-domain noise: thermal + AR(1) + motion spikes ---
SNR = 3
out <- add_tr_noise_snr(
  X_TR_clean = B_TR,
  SNR = SNR,                 # target effective SNR
  ref_metric = "std",        # use SD of clean signal as the "signal" reference
  ar1_rho = 0.35,
  motion_prob = 0.004,
  motion_amp  = 0.015,
  seed = 777
)
B_TR_noisy <- out$Y

## Plots
op <- par(mfrow = c(3,1), mar = c(3,4,2,1))
plot_bold_with_tasks(times, B_latent, U, region_cols = region_cols, task_cols = task_cols,
                     main = paste0("Latent BOLD (HRF=", hrf_len_sec, "s)"), ylim = c(-0.005, 0.035))
plot_bold_with_tasks(times, B_dt_noisy, U, region_cols = region_cols, task_cols = task_cols,
                     main = paste0("BOLD (HRF=", hrf_len_sec, "s)"), ylim = c(-0.005, 0.035))
plot_bold_with_tasks(times_TR, B_TR_noisy, U_TR, region_cols = region_cols, task_cols = task_cols,
                     main = paste0("BOLD TR (HRF=", hrf_len_sec, "s, SNR = ", SNR, ")"), ylim = c(-0.005, 0.035))
par(op)
