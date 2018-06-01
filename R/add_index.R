#' @title Add mediation index to a fitted mediation model
#'
#' @description Add mediation index to a fitted mediation model.
#'
#' @param mediation_model A mediation model fitted with \code{mediation_model}
#' method.
#' @param iter number of iteration for the Monte Carlo CI method.
#' @param alpha Alpha threshold to use when computing Monte Carlo CI.
#' @param stage Stage on which you want to compute conditional index for
#' moderated mediation models.
#'
#' @return An object of the same class as \code{mediation_model}, but with
#' confidence interval for the indirect effect estimation added for later
#' use.
#'
#' @examples
#' \dontrun{
#' my_model <- add_index(my_model)
#' }
#' @export
add_index <- function(mediation_model, iter = 5000, alpha = .05, ...) {
  UseMethod("add_index")
}
