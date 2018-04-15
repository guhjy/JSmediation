#' @title build a contrast code from character vector
#'
#' @description
#'
#' @param vector a character vector.
#' @param cond_a a character string to be recoded -0.5
#' @param cond_b a character string to be recoded  0.5
#'
#' @export
build_contrast <- function(vector, cond_a, cond_b) {
  UseMethod("build_contrast")
}

#' @export
build_contrast.character <- function(vector, cond_a, cond_b) {
  # recode
  vector[vector == cond_a] <- -.5
  vector[vector == cond_b] <-  .5

  # return
  as.numeric(vector)
}
