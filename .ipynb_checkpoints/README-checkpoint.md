
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# rollNonlinear

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/rollNonlinear)](https://CRAN.R-project.org/package=rollNonlinear)
[![R-CMD-check](https://github.com/rollnonlinear/rollNonlinear/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rollnonlinear/rollNonlinear/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/rollnonlinear/rollNonlinear/branch/main/graph/badge.svg)](https://app.codecov.io/gh/rollnonlinear/rollNonlinear?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`rollNonlinear` computes rolling-window nonlinear time-series metrics —
Hurst exponent, DFA, sample/approximate entropy, Lyapunov exponent, and
correlation dimension — on OHLC data for one or more symbols. It
provides an extensible metric registry, built-in series transforms, and
optional parallel execution via [`mirai`](https://mirai.r-lib.org/).

## Installation

``` r
# CRAN (once released):
install.packages("rollNonlinear")

# Development version from GitHub:
# install.packages("remotes")
remotes::install_github("rollnonlinear/rollNonlinear")
```

## Quick start

``` r
if (requireNamespace("rollNonlinear", quietly = TRUE)) {
  library(rollNonlinear)
} else {
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
}
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
library(data.table)

set.seed(1L)
n  <- 300L
dt <- data.table(
  symbol = rep(c("AAPL", "MSFT"), each = n),
  time   = rep(
    seq(as.POSIXct("2024-01-02 09:30:00", tz = "UTC"), by = 60, length.out = n),
    2L
  ),
  close  = c(cumsum(rnorm(n)) + 150, cumsum(rnorm(n)) + 370)
)

out <- roll_nonlinear(
  dt,
  metrics     = c("hurst", "dfa"),
  window_size = 128L,
  stride      = 32L
)
#> ℹ Rolling 2 metrics across 2 symbols (sequential mode).

head(out)
#> Key: <symbol, time>
#>    symbol                time window_start_idx window_end_idx     hurst
#>    <char>              <POSc>            <int>          <int>     <num>
#> 1:   AAPL 2024-01-02 11:37:00                1            128 0.2707119
#> 2:   AAPL 2024-01-02 12:09:00               33            160 0.5185714
#> 3:   AAPL 2024-01-02 12:41:00               65            192 0.6034170
#> 4:   AAPL 2024-01-02 13:13:00               97            224 0.8362714
#> 5:   AAPL 2024-01-02 13:45:00              129            256 0.8115970
#> 6:   AAPL 2024-01-02 14:17:00              161            288 0.7208838
#>    dfa_alpha
#>        <num>
#> 1:  1.270712
#> 2:  1.518571
#> 3:  1.603417
#> 4:  1.836271
#> 5:  1.811597
#> 6:  1.720884
```

## Parallel execution

``` r
out_par <- roll_nonlinear(
  dt,
  metrics     = c("hurst", "dfa"),
  window_size = 128L,
  stride      = 32L,
  parallel    = TRUE,   # uses mirai daemons
  n_workers   = 2L      # optional: override core count
)
```

## Yahoo Finance CLI utility

The repository also ships a small `Rscript` utility that downloads Yahoo
Finance OHLC data and runs the package engine end to end:

``` sh
Rscript inst/scripts/roll-nonlinear-yahoo.R \
  --symbols AAPL MSFT \
  --start 2024-01-01 \
  --end 2024-12-31 \
  --window-size 128 \
  --stride 16 \
  --metrics hurst dfa \
  --series log_returns \
  --output output/rolling-metrics.csv
```

Use repeated `--metric-arg` flags for metric-specific overrides, for
example:

``` sh
Rscript inst/scripts/roll-nonlinear-yahoo.R \
  --symbols AAPL \
  --metrics sample_entropy \
  --metric-arg sample_entropy.dimension=3 \
  --metric-arg sample_entropy.r_frac=0.25
```

## Learn more

- **Vignette**:
  `vignette("rolling-nonlinear", package = "rollNonlinear")`
- **Reference**: `?roll_nonlinear`, `?register_metric`, `?rn_daemons`
- **pkgdown site**: <https://rollnonlinear.github.io/rollNonlinear/>
