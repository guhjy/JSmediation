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

  purrr::map2(params,
              names(params),
              ~ cat(glue::glue("{.x} ({.y})\n\n")))

  cat("\nPaths:\n")

  purrr::map2(paths,
              names(paths),
              ~ cat(.y, ": ",
                    format(purrr::pluck(.x, "point_estimate"), digits = digits),
                    " (", format(purrr::pluck(.x, "se"), digits = digits), "), ",
                    purrr::pluck(.x, "APA"),
                    "\n",
                    sep = ""))

  cat("---\n* estimate (standard error), significance test\n")

  cat("\nIndirect effect index:\n")
  if(! x$indirect_index)
    cat("Indirect effect index is not computed by default.",
        "Please use add_index() to compute it.",
        sep = "\n")
  else
    print(x$indirect_index_infos, digits = digits)

  cat("\nModels' summaries\n")
  cat("==============================================\n")

  for(i in 1:length(models)) {
    cat(names(models)[[i]])
    cat("\n----------------------------------------------\n")
    print(summary(models[[i]]))
  }
}

