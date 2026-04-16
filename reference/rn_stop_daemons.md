# Stop mirai daemons

Calls `mirai::daemons(0L)` to stop all currently running local daemons.
Safe to call even when no daemons are running.

## Usage

``` r
rn_stop_daemons()
```

## Value

`NULL` invisibly.

## Details

Stop mirai daemons

## Examples

``` r
# \donttest{
if (requireNamespace("mirai", quietly = TRUE)) {
  rn_daemons(2L)
  rn_stop_daemons()
}
# }
```
