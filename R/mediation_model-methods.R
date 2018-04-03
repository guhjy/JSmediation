#' Print a summary for a mediation model represented by a \code{mediation_model} object
#'
#' @author Cédric Batailler \email{cedric.batailler@gmail.com}
#'
#' @export
print.mediation_model <- function(x, ...) {
  cat(glue::glue("Test of mediation ({x$type}):"))
  cat("\n\n")
  cat("----------------------------------------------\n")
  cat("Test of X -> M \n")
  print(summary(x$js_models[[1]]))
  cat("----------------------------------------------\n")
  cat("Test of X + M -> Y\n")
  print(summary(x$js_models[[2]]))
}

