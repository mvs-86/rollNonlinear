# Query or set mirai daemons

Convenience wrapper around
[`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html) and
[`mirai::status()`](https://mirai.r-lib.org/reference/status.html). With
no arguments, returns the current daemon status. With an integer
argument, sets the number of daemons and returns `n` invisibly.

## Usage

``` r
rn_daemons(n = NULL)
```

## Arguments

- n:

  `NULL` (default) to query status, or a positive integer to set the
  number of local daemons.

## Value

When `n = NULL`, the list returned by
[`mirai::status()`](https://mirai.r-lib.org/reference/status.html).
Otherwise, `n` (invisibly).

## Details

Query or set mirai daemons

## Examples

``` r
# \donttest{
if (requireNamespace("mirai", quietly = TRUE)) {
  rn_daemons(2L)   # start 2 daemons
  rn_daemons()     # query status
  rn_stop_daemons()
}
# }
```
