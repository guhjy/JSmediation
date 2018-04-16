#' Print a summary for a mediation model represented by a \code{mediation_model}
#'   object.
#'
#' @author Cédric Batailler \email{cedric.batailler@gmail.com}
#'
#' @export
print.mediation_model <- function(x, digits = 3) {

  # object  -------------------------------------------------------------------
  type <- x$type

  params <- x %>% purrr::pluck("params")
  paths  <- x %>% purrr::pluck("paths")
  models <- x %>% purrr::pluck("js_models")

  # summary -------------------------------------------------------------------
  cat(glue::glue("Test of mediation ({type})\n\n"))
  cat("==============================================\n")

  cat("\nVariables:\n")

  map2(params,
       names(params),
       ~ cat(glue::glue("{.x} ({.y})\n\n")))

  cat("\nPaths:\n")

  map2(paths,
       names(paths),
       ~ cat(.y, ": ",
             format(pluck(.x, "point_estimate"), digits = digits),
             "(", format(pluck(.x, "se"), digits = digits), ")",
             pluck(.x, "APA"),
             "\n"))

  cat("---\n* estimate (standard error), signficance test")

  cat("\nIndirect effect index:\n")

  cat("\nModels' summaries\n")
  cat("==============================================\n")

  for(i in 1:length(models)) {
    cat(names(models)[[i]])
    cat("\n----------------------------------------------\n")
    print(summary(models[[i]]))
  }
}

