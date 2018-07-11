#' @title add_index method for within-participant mediation
#'
#' @description Adds confidence interval for the inderct effect to a
#'   within-participant mediation model fitted with \code{\link{mdt_within}} or
#'   \code{\link{mdt_within_w}}.
#'
#' @param mediation_model A mediation model of class
#'   \code{"within_participant_mediation"}.
#' @param iter Number of simulation to use to compute Monte Carlo indirect
#'   effect confidence interval.
#' @param alpha Alpha threshold to use with the confidence interval.
#' @param ... Further arguments passed to or from other methods.
#'
#' @examples
#' ## getting an indirect effect index
#' within_model <- mdt_within(data = dohle_siegrist,
#'                            IV = name, 
#'                            DV = willingness, 
#'                            M = hazardousness, 
#'                            grouping = participant) 
#' add_index(within_model)
#'
#' @export
add_index.within_participant_mediation <- function(mediation_model, iter = 5000, alpha = .05, ...) {

  a   <- purrr::pluck(mediation_model, "paths", "a", "point_estimate")
  sea <- purrr::pluck(mediation_model, "paths", "a", "se")
  b   <- purrr::pluck(mediation_model, "paths", "b", "point_estimate")
  seb <- purrr::pluck(mediation_model, "paths", "b", "se")

  ab_sampling <-
    MASS::mvrnorm(n  = iter,
                  mu = c(a, b),
                  Sigma =
                    matrix(
                      c(sea^2, 0,
                        0, seb^2),
                      nrow = 2
                    ))

  indirect_sampling <- ab_sampling[ , 1] * ab_sampling[ , 2]
  CI <- stats::quantile(indirect_sampling, c(alpha / 2, 1 - alpha / 2))
  contains_zero <- (CI[[1]] < 0 & CI[[2]] > 0)

  indirect_index_infos <-
    list(type          = "Within-participant indirect effect",
         method        = "Monte Carlo",
         estimate      = a * b,
         CI            = CI,
         alpha         = alpha,
         iterations    = iter,
         contains_zero = contains_zero,
         sampling      = indirect_sampling)

  mediation_model$indirect_index <- TRUE
  mediation_model$indirect_index_infos <-
    as_indirect_index(indirect_index_infos)

  mediation_model
}
