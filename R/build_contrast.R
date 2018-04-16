#' @title build a contrast code from character vector
#'
#' @description This helper function helps you build a contrast code from a
#'   character variable.
#'
#' @param vector A character vector.
#' @param cond_a A character string to be coded -0.5
#' @param cond_b A character string to be coded  0.5
#'
#' @return A numeric vector.
#'
#' @examples
#'   data(linkedfate)
#'
#'   linkedfate$condition_contrast <- build_contrast(linkedfate$condition,
#'                                                   "Low discrimination",
#'                                                   "High discrimination")
#'
#'  head(linkedfate)
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
