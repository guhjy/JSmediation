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

  if(type == "moderated mediation") {
    Mod <- x$model$Mod
    IVInter <- glue::glue("{IV}:{Mod}")
    MInter  <- glue::glue("{M}:{Mod}")
  }

  cat(glue::glue("Test of mediation ({type})\n\n"))

  cat("\nPaths\n")
  cat("==============================================\n")

  if(type == "simple mediation") {
    cat(
      glue::glue(
        "

        a:  {a_test}
        b:  {b_test}
        c:  {c_test}
        c': {cprime_test}

        ",
        a_test = apastylr(x[["js_models"]][["X -> M"]], IV),
        b_test = apastylr(x[["js_models"]][["X + M -> Y"]], M),
        c_test = apastylr(x[["js_models"]][["X -> Y"]], IV),
        cprime_test = apastylr(x[["js_models"]][["X + M -> Y"]], IV)
      )
    )
  }

  if(type == "moderated mediation") {

    cat(
      glue::glue(
        "

        a:       {a_test}
        b * Mod: {bmod_test}

        a * Mod: {amod_test}
        b:       {b_test}

        c:        {c_test}
        c * Mod:  {cmod_test}

        c:       {cprime_test}
        c'* Mod: {cprimemod_test}

        ",
        a_test    = apastylr(x[["js_models"]][["X * Mod -> M"]], IV),
        bmod_test = apastylr(x[["js_models"]][["(X + M) * Mod -> Y"]], MInter),

        amod_test = apastylr(x[["js_models"]][["X * Mod -> M"]], IVInter),
        b_test    = apastylr(x[["js_models"]][["(X + M) * Mod -> Y"]], M),

        c_test      = apastylr(x[["js_models"]][["X * Mod -> Y"]], IV),
        cmod_test   = apastylr(x[["js_models"]][["X * Mod -> Y"]], IVInter),

        cprime_test    = apastylr(x[["js_models"]][["(X + M) * Mod -> Y"]], IV),
        cprimemod_test = apastylr(x[["js_models"]][["(X + M) * Mod -> Y"]], IVInter)
      )
    )
  }
  cat("\nPoint estimates\n")
  cat("==============================================\n")
  if(type == "simple mediation") {
    cat(glue::glue(
      "

      a:  {a} ({sea})
      b:  {b} ({seb})
      c:  {c} ({sec})
      c': {cprime} ({secprime})

      ab: {ab}

      ",
      model1 = x[["js_models_summary"]][["X -> M"]],
      model2 = x[["js_models_summary"]][["X + M -> Y"]],
      model3 = x[["js_models_summary"]][["X -> Y"]],

      a   = round(model1[model1$term == IV, "estimate"], 3),
      sea = round(model1[model1$term == IV, "std.error"], 3),
      b   = round(model2[model2$term == M, "estimate"], 3),
      seb = round(model2[model2$term == M, "std.error"], 3),

      c   = round(model3[model3$term == IV, "estimate"], 3),
      sec = round(model3[model3$term == IV, "std.error"], 3),

      cprime   = round(model2[model2$term == IV, "estimate"], 3),
      secprime = round(model2[model2$term == IV, "std.error"], 3),


      ab = round(a, 3)))
  }

  cat("\nCI\n")
  cat("==============================================\n")
  if(!x$CI)
    cat("You have to compute CI using add_ci() method.\n")
  if(x$CI) {
    if(type == "simple mediation") {
      cat(glue::glue(
        "{method} {percentage}% CI ({iterations} iterations):
                    [{CIL}; {CIU}]",
        method     = x[["CI_infos"]][["method"]],
        percentage = 100 - 100 * x[["CI_infos"]][["alpha"]],
        iterations = x[["CI_infos"]][["iterations"]],
        CIL = round(x[["CI_infos"]][["CI"]][[1]], 3),
        CIU = round(x[["CI_infos"]][["CI"]][[2]], 3))
      )
      cat("\n")
    }
    if(type == "moderated mediation") {
      cat(glue::glue(
        "{method} stage {stage} {percentage}% Conditional Index ({iterations} iterations):
                    [{CIL}; {CIU}]",
        method     = x[["CI_infos"]][["method"]],
        stage      = x[["CI_infos"]][["stage"]],
        percentage = 100 - 100 * x[["CI_infos"]][["alpha"]],
        iterations = x[["CI_infos"]][["iterations"]],
        CIL = round(x[["CI_infos"]][["CI"]][[1]], 3),
        CIU = round(x[["CI_infos"]][["CI"]][[2]], 3))
      )
      cat("\n")
    }
  }

  cat("\nModels' summaries\n")
  cat("==============================================\n")

  for(i in 1:length(x$js_models)) {
    cat(names(x$js_models)[[i]])
    cat("\n----------------------------------------------\n")
    print(summary(x$js_models[[i]]))
  }
}

