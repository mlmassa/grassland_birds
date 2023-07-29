sample_int <- function(n, size, replace = FALSE, wt = NULL, call = caller_env()) {
  if (!replace && n < size) {
   size <- n
  }

  if (size == 0L) {
    integer(0)
  } else {
    sample.int(n, size, prob = wt, replace = replace)
  }
}

assignInNamespace('sample_int', sample_int, 'dplyr')