#' Print a summary for a mediation model represented by a \code{mediation_model} object
#'
#' @author Cédric Batailler \email{cedric.batailler@gmail.com}
#'
#' @export
print.mediation_model <- function(x, ...) {

  type <- x$type
  IV   <- x$model$IV
  DV   <- x$model$DV
  M    <- x$model$M

  cat(glue::glue("Test of mediation ({type})\n\n"))

  cat("\nPaths\n")
  cat("==============================================\n")

  if(type == "simple mediation") {
    cat(glue::glue("a:  {a_test}\n\n",
                   a_test = apastylr(x[["js_models"]][["X -> M"]], IV)))
    cat(glue::glue("b:  {b_test}\n\n",
                   b_test = apastylr(x[["js_models"]][[3]], M)))
    cat(glue::glue("c:  {c_test}\n\n",
                   c_test = apastylr(x[["js_models"]][["X -> Y"]], IV)))
    cat(glue::glue("c': {cprime_test}\n\n",
                   cprime_test = apastylr(x[["js_models"]][[3]], IV)))
  }

  cat("\nPoint estimates\n")
  cat("==============================================\n")
  cat(glue::glue("a:  {a}\n\n",
                 model = x[["js_models_summary"]][["X -> M"]],
                 a = round(model[model$term == IV, "estimate"], 3)))
  cat(glue::glue("b:  {b}\n\n",
                 model = x[["js_models_summary"]][["X + M -> Y"]],
                 b = round(model[model$term == M, "estimate"], 3)))
  cat(glue::glue("c:  {c}\n\n",
                 model = x[["js_models_summary"]][["X -> Y"]],
                 c = round(model[model$term == IV, "estimate"], 3)))
  cat(glue::glue("c': {cprime}\n\n",
                 model = x[["js_models_summary"]][["X + M -> Y"]],
                 cprime = round(model[model$term == IV, "estimate"], 3)))

  cat(glue::glue("\n\nab: {ab}\n\n",
                 model1 = x[["js_models_summary"]][["X -> M"]],
                 a = model1[model1$term == IV, "estimate"],
                 model2 = x[["js_models_summary"]][["X + M -> Y"]],
                 b = model2[model2$term == M, "estimate"],
                 ab = round(a * b, 3)))

  cat("\nCI\n")
  cat("==============================================\n")
  if(!x$CI)
    cat("You have to compute CI using add_ci() method.\n")
  if(x$CI) {
    cat(glue::glue("{method} {percentage}% CI ({iterations} iterations):
                    [{CIL}; {CIU}]",
                   method     = x[["CI_infos"]][["method"]],
                   percentage = 100 - 100 * x[["CI_infos"]][["alpha"]],
                   iterations = x[["CI_infos"]][["iterations"]],
                   CIL = round(x[["CI_infos"]][["CI"]][[1]], 3),
                   CIU = round(x[["CI_infos"]][["CI"]][[2]], 3)))
    cat("\n")
  }


  cat("\nModels' summaries\n")
  cat("==============================================\n")

  for(i in 1:length(x$js_models)) {
    cat(names(x$js_models)[[i]])
    cat("\n----------------------------------------------\n")
    print(summary(x$js_models[[i]]))
  }
}

