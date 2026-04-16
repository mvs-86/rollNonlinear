# ---------------------------------------------------------------------------
# Entropy metric wrappers
# ---------------------------------------------------------------------------
# Implements sample_entropy (SampEn) and approx_entropy (ApEn) using
# built-in pure-R algorithms. nonlinearTseries::sampleEntropy takes a
# corrDim object rather than a raw time series, so both wrappers use their
# own implementations:
#   SampEn: Richman & Moorman (2000)
#   ApEn:   Pincus (1991)
#
# User-facing .args keys:
#   dimension  integer  embedding dimension m (default 2L)
#   r_frac     numeric  tolerance = r_frac * sd(x) (default 0.2)
#   r          numeric  explicit tolerance; overrides r_frac when present
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Shared helper: resolve entropy tolerance r
# ---------------------------------------------------------------------------

#' @noRd
.resolve_entropy_r <- function(x, args) {
  if (!is.null(args[["r"]])) {
    args[["r"]]
  } else {
    r_frac <- args[["r_frac"]] %||% 0.2
    r_frac * stats::sd(x, na.rm = TRUE)
  }
}

# ---------------------------------------------------------------------------
# Internal: sample entropy (Richman & Moorman 2000)
# SampEn(m, r, N) = -log(A / B) where
#   B = # pairs i < j with Chebyshev dist <= r for templates of length m
#   A = # pairs i < j with Chebyshev dist <= r for templates of length m+1
# ---------------------------------------------------------------------------

#' @noRd
.sample_entropy_impl <- function(x, m, r) {
  N <- length(x)

  # Build template matrix: (N-dim) rows, dim columns (SampEn excludes last obs)
  build_tmpl <- function(dim) {
    n <- N - dim
    if (n < 2L) return(NULL)
    tmpl <- matrix(NA_real_, nrow = n, ncol = dim)
    for (k in seq_len(dim)) tmpl[, k] <- x[k:(n + k - 1L)]
    tmpl
  }

  count_pairs <- function(tmpl) {
    n   <- nrow(tmpl)
    dim <- ncol(tmpl)
    cnt <- 0L
    for (i in seq_len(n - 1L)) {
      tail_idx  <- seq(i + 1L, n)
      diff_mat  <- abs(tmpl[tail_idx, , drop = FALSE] -
                         rep(tmpl[i, ], each = length(tail_idx)))
      max_dists <- if (dim == 1L) diff_mat[, 1L] else apply(diff_mat, 1L, max)
      cnt <- cnt + sum(max_dists <= r)
    }
    cnt
  }

  tmpl_m  <- build_tmpl(m)
  tmpl_m1 <- build_tmpl(m + 1L)
  if (is.null(tmpl_m) || is.null(tmpl_m1)) return(NA_real_)

  B <- count_pairs(tmpl_m)
  A <- count_pairs(tmpl_m1)

  if (B == 0L) return(NA_real_)
  -log(A / B)
}

# ---------------------------------------------------------------------------
# Internal: approximate entropy (Pincus 1991)
# ApEn(m, r, N) = Phi(m) - Phi(m+1)
# Phi(m) = mean_i log(C_i^m(r))  where C_i includes self-match
# ---------------------------------------------------------------------------

#' @noRd
.approx_entropy_impl <- function(x, m, r) {
  N <- length(x)

  phi_dim <- function(dim) {
    count <- N - dim + 1L
    if (count < 2L) return(NA_real_)

    tmpl <- matrix(NA_real_, nrow = count, ncol = dim)
    for (k in seq_len(dim)) tmpl[, k] <- x[k:(count + k - 1L)]

    log_sum <- 0
    for (i in seq_len(count)) {
      diff_mat  <- abs(tmpl - rep(tmpl[i, ], each = count))
      max_dists <- if (dim == 1L) diff_mat[, 1L] else apply(diff_mat, 1L, max)
      Ci <- sum(max_dists <= r) / count
      if (Ci > 0) log_sum <- log_sum + log(Ci)
    }
    log_sum / count
  }

  tryCatch(phi_dim(m) - phi_dim(m + 1L), error = function(e) NA_real_)
}

# ---------------------------------------------------------------------------
# Wrapper: sample entropy
# ---------------------------------------------------------------------------

#' @noRd
.metric_sample_entropy <- function(x, .args = list()) {
  defaults <- list(dimension = 2L, r_frac = 0.2)
  args     <- utils::modifyList(defaults, .args)

  if (stats::sd(x, na.rm = TRUE) == 0) return(c(sample_entropy = NA_real_))

  r     <- .resolve_entropy_r(x, args)
  m_val <- as.integer(args[["dimension"]] %||% 2L)

  result <- tryCatch(
    .sample_entropy_impl(x, m = m_val, r = r),
    error = function(e) NA_real_
  )
  c(sample_entropy = result)
}

# ---------------------------------------------------------------------------
# Wrapper: approximate entropy
# ---------------------------------------------------------------------------

#' @noRd
.metric_approx_entropy <- function(x, .args = list()) {
  defaults <- list(dimension = 2L, r_frac = 0.2)
  args     <- utils::modifyList(defaults, .args)

  if (stats::sd(x, na.rm = TRUE) == 0) return(c(approx_entropy = NA_real_))

  r     <- .resolve_entropy_r(x, args)
  m_val <- as.integer(args[["dimension"]] %||% 2L)

  result <- tryCatch(
    .approx_entropy_impl(x, m = m_val, r = r),
    error = function(e) NA_real_
  )
  c(approx_entropy = result)
}
