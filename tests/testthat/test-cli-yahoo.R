make_yahoo_like_df <- function(symbol = "AAPL") {
  out <- data.frame(
    open_col = c(100, 101, 102),
    high_col = c(101, 102, 103),
    low_col = c(99, 100, 101),
    close_col = c(100.5, 101.5, 102.5),
    volume_col = c(1000, 1100, 1200),
    adjusted_col = c(100.2, 101.2, 102.2),
    check.names = FALSE
  )
  names(out) <- paste0(
    symbol,
    c(".Open", ".High", ".Low", ".Close", ".Volume", ".Adjusted")
  )
  rownames(out) <- c("2024-01-02", "2024-01-03", "2024-01-04")
  out
}

test_that(".parse_metric_args_cli builds typed per-metric overrides", {
  args <- c(
    "sample_entropy.dimension=3",
    "sample_entropy.r_frac=0.25",
    "corr_dim.use_embedded=true",
    "hurst.label='demo'"
  )

  out <- rollNonlinear:::.parse_metric_args_cli(args)

  expect_equal(out$sample_entropy$dimension, 3L)
  expect_equal(out$sample_entropy$r_frac, 0.25)
  expect_true(out$corr_dim$use_embedded)
  expect_equal(out$hurst$label, "demo")
})

test_that(".parse_metric_args_cli rejects malformed entries", {
  expect_error(
    rollNonlinear:::.parse_metric_args_cli("sample_entropy=3"),
    "metric_arg"
  )
})

test_that(".resolve_cli_series maps names to package series specs", {
  expect_equal(rollNonlinear:::.resolve_cli_series("close"), "close")

  dt <- data.table::data.table(
    high = c(11, 12),
    low = c(9, 10),
    close = c(10, 11)
  )

  expect_equal(
    rollNonlinear:::.resolve_cli_series("typical")(dt),
    c(10, 11)
  )
})

test_that(".normalize_yahoo_ohlc standardizes Yahoo-style column names", {
  out <- rollNonlinear:::.normalize_yahoo_ohlc(make_yahoo_like_df(), "AAPL")

  expect_true(data.table::is.data.table(out))
  expect_equal(names(out), c("symbol", "time", "open", "high", "low", "close", "volume", "adjusted"))
  expect_equal(unique(out$symbol), "AAPL")
  expect_s3_class(out$time, "Date")
  expect_equal(out$close, c(100.5, 101.5, 102.5))
  expect_equal(data.table::key(out), c("symbol", "time"))
})

test_that(".download_yahoo_ohlc downloads and binds multiple symbols", {
  lookup <- list(
    AAPL = make_yahoo_like_df("AAPL"),
    MSFT = make_yahoo_like_df("MSFT")
  )

  downloader <- function(Symbols, src, from, to, auto.assign, warnings) {
    expect_equal(src, "yahoo")
    expect_false(auto.assign)
    lookup[[Symbols]]
  }

  out <- rollNonlinear:::.download_yahoo_ohlc(
    symbols = c("AAPL", "MSFT"),
    start = as.Date("2024-01-01"),
    end = as.Date("2024-01-31"),
    downloader = downloader
  )

  expect_equal(unique(out$symbol), c("AAPL", "MSFT"))
  expect_equal(nrow(out), 6L)
  expect_equal(data.table::key(out), c("symbol", "time"))
})

test_that(".download_yahoo_ohlc validates date range", {
  expect_error(
    rollNonlinear:::.download_yahoo_ohlc(
      symbols = "AAPL",
      start = as.Date("2024-02-01"),
      end = as.Date("2024-01-01"),
      downloader = function(...) make_yahoo_like_df()
    ),
    "on or after"
  )
})
