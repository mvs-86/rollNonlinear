# CRAN submission comments — rollNonlinear 0.1.0

## R CMD check results

0 errors | 0 warnings | 0 notes

(A "New submission" note may appear on CRAN's side; that is expected.)

## Platform results

Tested on:

| Platform | R version | Result |
|----------|-----------|--------|
| Ubuntu 24.04 | R release | ✅ |
| Ubuntu 24.04 | R devel   | ✅ |
| macOS 14      | R release | ✅ |
| macOS 14      | R devel   | ✅ |
| Windows Server 2022 | R release | ✅ |
| Windows Server 2022 | R devel   | ✅ |

## Notes

* This is a new submission (no previous CRAN version).
* The `mirai` and `parallelly` packages are in Imports because parallel
  execution is a first-class feature. Both packages are on CRAN.
* All examples that call Lyapunov / corr_dim metrics are wrapped in
  `\donttest{}` due to their computational cost (> 5 s per example).
* Parallel examples are wrapped in `\donttest{}` and guarded with
  `requireNamespace("mirai", quietly = TRUE)` per CRAN policy.
