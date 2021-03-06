#' @title JSmediation
#'
#' @name JSmediation-package
#' @aliases JSmediation
#'
#' @description A set of helper functions to conduct joint-significance test for
#'   mediation analysis.
#'
#' @details The main functions of the \pkg{JSmediation} package follow an
#'   \code{mdt_*} pattern. \code{mdt_*} family functions and allow you to
#'   conduct joint-significance tests for various mediation models.
#'
#'   The syntax for \code{mdt_*} family functions is usually the same. The first
#'   argument is always a data frame (\code{data})  which is followed by the
#'   variable names involved in the model (e.g., DV, IV). Because \code{mdt_*}
#'   family functions use non-standard evaluation, these variable names must
#'   generaly be specified unquoted.
#'
#'   \code{mdt_*} family functions allow you to create an object of class
#'   \code{"mediation_model"} for which various methods are implemented.
#'
#'   See \code{vignette("jsmediation")} for a general introduction and overview
#'   of \pkg{JSmediation}.
NULL
