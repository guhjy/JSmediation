#' @title Shape a data frame from wide to long format.
#'
#' @description An helper to reshape a wide formatted data.frame to a long one.
#' \code{\link{mdt_within}} requiere a \code{data.frame} with one line by IV
#' condition. \code{reshape_wide_to_long} helps you to go from a
#' \code{data.frame} where DV and M score would be stored in different columns
#' for the different IV condition to a \code{data.frame} that is supported by
#' \code{\link{mdt_within}}.
#'
#'
#' @param data A \code{data.frame}
#' @param DV_A An unquoted variable name in data containing DV value for A
#' condition of IV.
#' @param DV_B An unquoted variable name in data containing DV value for B
#' condition of IV.
#' @param M_A An unquoted variable name in data containing DV value for A
#' condition of mediator.
#' @param M_B An unquoted variable name in data containing DV value for B
#' condition of mediator.
#'
#' @return data A \code{data.frame} in a long format.
#'
#' @examples
#' \dontrun{
#' }
#' @export
shape_wide_to_long <- function(data, DV_A, DV_B, M_A, M_B) {
  UseMethod("reshape_wide_to_long")
}

#' @export
shape_wide_to_long.data.frame <- function(data, DV_A, DV_B, M_A, M_B) {
warning("not yet implemented.")
}
