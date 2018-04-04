#' Fit a moderated mediation model
#'
#' Given a dataframe, an IV, a DV, a mediator and a moderator, fit a moderated
#'   mediation model.
#'
#' @param data dataframe
#' @param IV IV
#' @param M Mediator
#' @param DV DV
#' @param Moderator Moderator
#'
#' @references Muller, D., Judd, C. M., & Yzerbyt, V. Y. (2005). When moderation
#'   is mediated and mediation is moderated. \textit{Journal of Personality and
#'   Social Psychology}, 89(6), 852‑863. doi: 10.1037/0022-3514.89.6.852
#'
#' @export

mdt_moderated <- function(...) {
  UseMethod("mdt_moderated")
}

#' @export
mdt_moderated.data.frame <- function(data,
                                     IV,
                                     DV,
                                     M,
                                     Moderator) {

  IV_var        <- enquo(IV)
  DV_var        <- enquo(DV)
  M_var         <- enquo(M)
  Moderator_var <- enquo(Moderator)


  model1 <-
    stats::as.formula(glue::glue("{DV} ~ {IV} * {Mod}",
                                 IV  = rlang::f_text(IV_var),
                                 DV  = rlang::f_text(DV_var),
                                 Mod = rlang::f_text(Moderator_var)))

  model2 <-
    stats::as.formula(glue::glue("{M} ~ {IV} * {Mod}",
                                 IV  = rlang::f_text(IV_var),
                                 M   = rlang::f_text(M_var),
                                 Mod = rlang::f_text(Moderator_var)))

  model3 <-
    stats::as.formula(glue::glue("{DV} ~ ({IV} + {M}) * {Mod}",
                                 DV  = rlang::f_text(DV_var),
                                 IV  = rlang::f_text(IV_var),
                                 M   = rlang::f_text(M_var),
                                 Mod = rlang::f_text(Moderator_var)))

  mediation_model <-
    tibble::lst(
      type      = "moderated mediation",
      method    = "Joint significant",
      model     = list("IV" = rlang::f_text(IV_var),
                       "DV" = rlang::f_text(DV_var),
                       "M"  = rlang::f_text(M_var),
                       "Moderator" = rlang::f_text(Moderator_var)),
      CI        = FALSE,
      js_models =
        list("X * Mod -> Y"       = model1,
             "X * Mod -> M"       = model2,
             "(X + M) * Mod -> Y" = model3) %>%
        purrr::map(~lm(.x, data)),
      js_models_summary =
        purrr::map(js_models, ~broom::tidy(.x)),
      data =
        data
    )

  as_mediation_model(mediation_model)
}
