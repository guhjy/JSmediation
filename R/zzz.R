is_contrast <- function(x) {
  length(unique(x)) == 2L & sum(unique(x)) == 0
}

is_centered <- function(x) {
  # implementation to deal for floating number rounding error
  isTRUE(all.equal(mean(x), 0))
}
