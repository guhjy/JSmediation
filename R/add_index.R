#' @title Add an indirect effect index to a fitted mediation model
#'
#' @description \code{\link{add_index}} is a generic function that adds indirect
#'   effect index to a fitted mediation model. This function invokes particular
#'   methods depending of the class of the mediation model. For example, with a
#'   model fitted with \code{\link{mdt_simple}}, \code{\link{add_index}} will
#'   invoke \code{\link{add_index.simple_mediation}}.
#'
#' @param mediation_model A mediation model fitted with and \code{mdt_*} family
#'   function.
#' @param iter Number of simulation to use to compute Monte Carlo indirect
#'   effect confidence interval.
#' @param alpha Alpha threshold to use with the confidence interval.
#' @param ... Further arguments to be passed to further methods.
#'
#' @return An object of the same class as \code{mediation_model}, but with
#'   confidence interval for the indirect effect estimation added for later use.
#'
#' @export
add_index <- function(mediation_model, iter = 5000, alpha = .05, ...) {
  UseMethod("add_index")
}
