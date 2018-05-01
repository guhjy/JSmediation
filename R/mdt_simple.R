#' @title Simple mediation model
#'
#' @description
#' Given a dataframe, an IV (\code{IV}), a DV (\code{DV}) and a mediator
#' (\code{M}), \code{mdt_simple} fits a simple mediation model using joint
#' significance method (Judd, Yzerbyt, & Muller, 2014).
#'
#' @details
#' In a simple mediation analysis, researchers are intrested in finding if the
#' effect of \eqn{X} on \eqn{Y} involve a third variable \eqn{M}. The basic
#' hypothesis behind this test is that \eqn{X} has an effect on \eqn{M} which
#' has an effect on \eqn{Y}: meaning that \eqn{X} has an indrect effect on
#' \eqn{Y} through \eqn{M}.
#'
#' Total effect of \eqn{X} on \eqn{Y} can be described the following way:
#'
#' \deqn{c = c' + ab}
#'
#' where \eqn{c} is the total effect of \eqn{X} on \eqn{Y}, \eqn{c'} is the
#' direct of \eqn{X} on \eqn{Y}, and \eqn{ab} is the indirect effect of \eqn{X}
#' on \eqn{Y} through {M}.
#'
#' To assess if the indirect effect is different from the null, one has to
#' assess the significance against the null for both \eqn{a} (the effect of
#' \eqn{X} on \eqn{M}) for and \eqn{b} (effect of \eqn{M} on \eqn{Y} controlling
#' for the effect of \eqn{X}) through the following set of linear models:
#'
#' \deqn{Y_i = \beta_{01} + c X_i}{Yi = \beta_01 + c*Xi}
#' \deqn{M_i = \beta_{02} + a X_i}{Mi = \beta_02 + a*Xi}
#' \deqn{Y_i = \beta_{03} + c' X_i + b M_i}{Yi = \beta_03 + c'*Xi + b*Mi}
#'
#' In joint significance mediation analysis approach, bot \eqn{a} and \eqn{b}
#' must be significantly different from the null to conclude on the
#' significance of the indirect effect.
#'
#' @family mediation models
#'
#' @references
#' Judd, C. M., Yzerbyt, V. Y., & Muller, D. (2014). Mediation
#' and moderation. Handbook of research methods in
#' social and personality psychology, 2, 653–676.
#'
#' @param data a dataframe containing the variables in the model.
#' @param IV an unquoted numeric variable in the data frame which will be used
#'             as independant variable.
#' @param M an unquoted numeric variable in the data frame which will be used
#'             as mediator.
#' @param DV an unquoted numeric variable in the data frame which will be used
#'             as dependant variable.
#'
#' @template mediation_model
#'
#' @examples
#' \dontrun{
#' my_model <- mdt_simple(data = dataset, IV = X, DV = Y, M = Mediator)
#' }
#' @export

mdt_simple <- function(data, IV, DV, M) {
  UseMethod("mdt_simple")
}

#' @export
mdt_simple.data.frame <- function(data, IV, DV, M) {

  # nse -----------------------------------------------------------------------
  IV_var <- enquo(IV)
  DV_var <- enquo(DV)
  M_var  <- enquo(M)

  IV_name <- rlang::quo_name(IV_var)
  DV_name <- rlang::quo_name(DV_var)
  M_name  <- rlang::quo_name(M_var)

  IV_data <- data %>% dplyr::pull( !! IV_var )
  DV_data <- data %>% dplyr::pull( !! DV_var )
  M_data  <- data %>% dplyr::pull( !! M_var )

  # type check ----------------------------------------------------------------
  if(!is.numeric(IV_data))
    stop(glue::glue("Warning:
                    IV ({IV_name}) must be numeric (see build_contrast() to
                    convert a character vector to a contrast code)."))

  if(!is.numeric(M_data))
    stop(glue::glue("Warning:
                    Mediator ({M_name}) must be numeric."))

  if(!is.numeric(DV_data))
    stop(glue::glue("Warning:
                    DV ({DV_name}) must be numeric."))

  # building models -----------------------------------------------------------
  model1 <-
    stats::as.formula(glue::glue("{DV} ~ {IV}",
                                 IV = IV_name,
                                 DV = DV_name))

  model2 <-
    stats::as.formula(glue::glue("{M} ~ {IV}",
                                 IV = IV_name,
                                 M  = M_name))

  model3 <-
    stats::as.formula(glue::glue("{DV} ~ {IV} + {M}",
                                 DV = DV_name,
                                 IV = IV_name,
                                 M  = M_name))

  # models fitting and cleaning -----------------------------------------------
  js_models <-
    list("X -> Y"     = model1,
         "X -> M"     = model2,
         "X + M -> Y" = model3) %>%
    purrr::map(~lm(.x, data))

  # paths ---------------------------------------------------------------------
  paths <-
    list("a" = create_path(js_models, "X -> M", IV_name),
         "b" = create_path(js_models, "X + M -> Y", M_name),
         "c" = create_path(js_models, "X -> Y", IV_name),
         "c'"= create_path(js_models, "X + M -> Y", IV_name))

  # bulding mediation model object --------------------------------------------
  mediation_model <-
    structure(
      list(
        type           = "simple mediation",
        method         = "joint significant",
        params         = list("IV" = IV_name,
                              "DV" = DV_name,
                              "M"  = M_name),
        paths          = paths,
        indirect_index = FALSE,
        js_models      = js_models,
        data           = data
        ),
      class = c("simple_mediation", "mediation_model")
    )

  mediation_model
}

}

