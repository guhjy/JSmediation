#' Print a summary for an indirect effect index created with \code{add_index()}
#'   method.
#'
#' @author Cédric Batailler \email{cedric.batailler@gmail.com}
#'
#' @export
print.indirect_index <- function(x, digits = 3) {
  cat("- type:", x$type, "\n")
  cat("- point estimate:", format(x$estimate, digits = digits), "\n" )
  cat("- confidence interval:\n")
  cat("  - method: ", x$method, " (", x$iter, " iterations)\n", sep = "")
  cat("  - alpha:", x$alpha, "\n")
  cat("  - CI: [",
      format(x$CI[[1]], digits = digits),
      "; ",
      format(x$CI[[2]], digits = digits),
      "]\n",
      sep = "")
}
