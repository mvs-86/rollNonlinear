# Helper data generators for rollNonlinear tests
# These are sourced automatically by testthat before any test file.

#' AR(1) series
#' @param n Number of observations.
#' @param phi AR(1) coefficient (default 0.7).
#' @param seed Random seed (default 1L).
make_ar1 <- function(n, phi = 0.7, seed = 1L) {
  set.seed(seed)
  x <- numeric(n)
  x[[1L]] <- rnorm(1L)
  for (i in seq_len(n - 1L)) {
    x[[i + 1L]] <- phi * x[[i]] + rnorm(1L)
  }
  x
}

#' Fractional Brownian motion via spectral synthesis
#'
#' Implements the Davies-Harte method using `stats::fft`.
#' @param n Number of observations.
#' @param hurst Hurst exponent (0 < hurst < 1, default 0.7).
#' @param seed Random seed (default 1L).
make_fbm <- function(n, hurst = 0.7, seed = 1L) {
  set.seed(seed)
  # Build the autocovariance of fractional Gaussian noise
  k    <- seq_len(n) - 1L
  r    <- 0.5 * (abs(k + 1)^(2 * hurst) - 2 * abs(k)^(2 * hurst) +
                   abs(k - 1)^(2 * hurst))
  # Embed in a circulant of size 2*(n-1)
  m    <- 2L * (n - 1L)
  row1 <- c(r, rev(r[-c(1L, length(r))]))
  eig  <- Re(stats::fft(row1))
  # Safety: negative eigenvalues from numerical errors
  eig  <- pmax(eig, 0)
  # Generate complex Gaussian noise
  z    <- (rnorm(m) + 1i * rnorm(m)) / sqrt(2)
  z[[1L]] <- Re(z[[1L]]) * sqrt(2)
  if (m %% 2L == 0L) z[[m / 2L + 1L]] <- Re(z[[m / 2L + 1L]]) * sqrt(2)
  w    <- Re(stats::fft(sqrt(eig) * z, inverse = FALSE)) / sqrt(m)
  # Cumulative sum gives fBm from fGn
  cumsum(w[seq_len(n)])
}

#' Regularly-spaced POSIXct timestamp vector
#' @param n Number of observations.
#' @param start Start time string (default `"2024-01-02 09:30:00"`).
#' @param freq_sec Spacing in seconds (default 60L).
make_times <- function(n, start = "2024-01-02 09:30:00", freq_sec = 60L) {
  t0 <- as.POSIXct(start, tz = "UTC")
  t0 + (seq_len(n) - 1L) * freq_sec
}
