if (requireNamespace("mirai", quietly = TRUE)) {
  try(mirai::daemons(0L), silent = TRUE)
}
