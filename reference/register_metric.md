# Register a custom rolling metric

Adds a new metric (or replaces an existing one) in the package metric
registry. Once registered, the metric name is available for use in
[`roll_nonlinear_one()`](https://rollnonlinear.github.io/rollNonlinear/reference/roll_nonlinear_one.md)
and
[`roll_nonlinear()`](https://rollnonlinear.github.io/rollNonlinear/reference/roll_nonlinear.md).

## Usage

``` r
register_metric(name, fn, outputs, defaults = list(), min_len = NULL)
```

## Arguments

- name:

  A single string. The metric name (key in the registry).

- fn:

  A function with formals `(x, .args = list())` that accepts a numeric
  vector `x` and a list of named arguments `.args`, and returns a named
  numeric vector with names matching `outputs`.

- outputs:

  Non-empty, unique character vector of output column names that `fn`
  returns.

- defaults:

  Named list of default arguments passed to `fn` as `.args`.
  User-supplied `metric_args` override these via
  [`utils::modifyList()`](https://rdrr.io/r/utils/modifyList.html).

- min_len:

  A function `function(args) -> integer` that returns the advisory
  minimum window size for the given resolved args. If `NULL`, defaults
  to `function(a) 2L`.

## Value

Invisibly `NULL`. Called for side effects.

## Examples

``` r
my_fn <- function(x, .args = list()) c(my_mean = mean(x, na.rm = TRUE))
register_metric("my_mean", my_fn, outputs = "my_mean")
available_metrics()
#> [1] "approx_entropy" "corr_dim"       "dfa"            "hurst"         
#> [5] "lyapunov"       "my_mean"        "sample_entropy"
# cleanup
rm("my_mean", envir = rollNonlinear:::.metric_registry)
```
