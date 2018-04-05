#' Create an APA formated report from the test of a specific term in a
#' linear model
#'
#' @param model A linear model created using \code{lm()}.
#' @param term A character string representing a term in the linear model
#'
#' @return Ana APA formated character string.
#'
#' @export
apastylr <- function(model, term) {
  UseMethod("apastylr")
}

#' @export
apastylr.lm <- function(model, term) {

  summary_statistics <-
    broom::tidy(model)

  if(nrow(summary_statistics[summary_statistics$term == term, ]) == 0)
     warning(glue::glue("Warning:\n\tCould not find term \"{term}\" in model."))

  summary_statistics <- summary_statistics[summary_statistics$term == term, ]

  t <-
    format(abs(round(summary_statistics$statistic[1], 2)), nsmall = 2)

  df <-
    model$df.residual

  pvalue <-
    summary_statistics$p.value[1]

  glue::glue("t({df}) = {t}, p {p}",
             p = ifelse(pvalue < .001,
                        "< .001",
                        sub(".", "= ", format(round(summary_statistics$p.value, 3), nsmall = 3))
             ))

}