#' @title Joint-significance test for simple mediation
#'
#' @description Given a data frame, a predictor (\code{IV}), an outcome
#'   (\code{DV}), and a mediator (\code{M}), conducts a joint-significant test 
#'   for simple mediation (see Yzerbyt, Muller, Batailler, & Judd, 2018).
#'
#' @param data A data frame containing the variables to be used in the model.
#' @param IV An unquoted numeric variable in the data frame which will be used
#'   as independant variable.
#' @param M An unquoted numeric variable in the data frame which will be used as
#'   mediator.
#' @param DV An unquoted numeric variable in the data frame which will be used
#'   as dependant variable.
#'
#' @template mediation_model
#'
#' @family mediation models
#'
#' @details With simple mediation analysis, one is intrested in finding if the
#'   effect of \eqn{X} on \eqn{Y} goes through a third variable \eqn{M}. The
#'   hypothesis behind this test is that \eqn{X} has an effect on \eqn{M}
#'   (\eqn{a}) which has an effect on \eqn{Y} (\eqn{b}), meaning that \eqn{X}
#'   has an indrect effect on \eqn{Y} through \eqn{M}.
#'
#'   Total effect of \eqn{X} on \eqn{Y} can be described the following way:
#'
#'   \deqn{c = c' + ab}
#'
#'   with \eqn{c} the total effect of \eqn{X} on \eqn{Y}, \eqn{c'} the direct of
#'   \eqn{X} on \eqn{Y}, and \eqn{ab} the indirect effect of \eqn{X} on \eqn{Y}
#'   through {M} (see Models section).
#'
#'   To assess if the indirect effect is different from the null, one has to
#'   assess the significance against the null for both \eqn{a} (the effect of
#'   \eqn{X} on \eqn{M}) for and \eqn{b} (effect of \eqn{M} on \eqn{Y}
#'   controlling for the effect of \eqn{X}). Both \eqn{a} and \eqn{b} needs to  
#'   be simultaneously significant for an indirect effect to be claimed (Baron &
#'   Kenny, 1986; Judd & Kenny, 1981).
#'
#' @section Models: In a simple mediation model, three models will be used:
#'
#'   - \eqn{Y_i = b_{10} + \mathbf{c_{11}} X_i}{Yi = b_10 + c_11*Xi} 
#'   - \eqn{M_i = b_{20} + \mathbf{a_{21}} X_i}{Mi = b_20 + a_21*Xi} 
#'   - \eqn{Y_i = b_{30} + \mathbf{c'_{31}} X_i + \mathbf{b_{32}} M_i}{Yi =
#'     b_30 + c'_31*Xi + b_32*Mi}
#'
#'   with \eqn{Y_i}{Yi}, outcome value for the \emph{i}th observation,
#'   \eqn{X_i}{Xi}, predictor value for the \emph{i}th observation, and
#'   \eqn{M_i}{Mi}, mediator value for the \emph{i}th observation (Baron &
#'   Kenny, 1986; Judd & Kenny, 1981).
#'   
#'   Coefficients associated with \eqn{a}, \eqn{aMod} \eqn{b}, \eqn{c}, and
#'   \eqn{c'} paths are respectively \eqn{a_{21}}{a_21}, \eqn{b_{32}}{b_32},
#'   \eqn{c_{11}}{c_11}, and \eqn{c'_{31}}{c'_31}.
#'   
#' @section Variable coding: Because joint-significance tests uses linear models
#'   uneder the scene, variables involved in the model have to be numeric.
#'   \code{mdt_simple} will throw an error if non-numeric variables are
#'   specified in the model.
#'
#'   If you need to convert a dichotomous categorical variable to a numeric one,
#'   please refer to \code{\link{build_contrast}} function.
#'
#' @references Baron, R. M., & Kenny, D. A. (1986). The moderator-mediator
#'   variable distinction in social psychological research: Conceptual,
#'   strategic, and statistical considerations. \emph{Journal of Personality and
#'   Social Psychology}, \emph{51}(6), 1173‑1182.doi:10.1037/0022-3514.51.6.1173
#'
#'   Judd, C. M., & Kenny, D. A. (1981). Process Analysis: Estimating Mediation
#'   in Treatment Evaluations. \emph{Evaluation Review}, \emph{5}(5), 602‑619.
#'   doi: 10.1177/0193841X8100500502
#'
#'   Yzerbyt, V., Muller, D., Batailler, C., & Judd, C. M. (in press). New
#'   recommendations for testing indirect effects in mediational models: The
#'   need to report and test component paths. \emph{Journal of Personality and
#'   Social Psychology}.
#' 
#' @examples
#' ## fit a simple mediation model
#' data(ho_et_al)
#' ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
#'                                        "Low discrimination",
#'                                        "High discrimination")
#' mdt_simple(data = ho_et_al,
#'            IV = condition_c,
#'            DV = hypodescent,
#'            M = linkedfate)
#'
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
  mediation_model(
    type      = "simple mediation",
    params    = list("IV" = IV_name,
                     "DV" = DV_name,
                     "M"  = M_name),
    paths     = paths,
    js_models = js_models,
    data      = data,
    subclass  = "simple_mediation"
  )
}

