#' @title Joint-significance test for within-participant mediation
#'
#' @description Given a data frame, a predictor (\code{IV}), an outcome
#'   (\code{DV}), a mediator (\code{M}), and a grouping variable (\code{group})
#'   conducts a joint-significant test for within-participant mediation (see
#'   Yzerbyt, Muller, Batailler, & Judd, 2018).
#'

#'
#' @param data a data frame containing the variables in the model.
#' @param IV an unquoted variable in the data frame which will be used as
#'   independant variable.
#' @param M an unquoted variable in the data frame which will be used as
#'   mediator.
#' @param DV an unquoted variable in the data frame which will be used as
#'   dependant variable.
#' @param grouping an unquoted variable in the data frame which will be used as
#'   grouping variable.
#' @param default_coding should the variable coding be the default? Defaults to
#'   \code{TRUE}.
#'
#' @template mediation_model
#'
#' @section Models: For within-participant mediation, three models will be used:
#'
#'   - \eqn{Y_{2i} - Y_{1i} = c_{11}}{Y2i - Y1i = c11}
#'   - \eqn{M_{2i} - M_{1i} = a_{21}}{M2i - M1i = a21}
#'   - \eqn{Y_{2i} - Y_{1i} = c'_{31} + b_{32}(M_{2i} - M_{1i}) +
#'   d_{33}[0.5(M_{1i} + M_{2i}) - 0.5(\overline{M_{1} + M_{2}})]}{Y2i - Y1i =
#'   c'31 + b32 * (M2i + M1i) + d33 * [0.5 * (M1i + M2i) - 0.5 * mean(M1 + M2)]}
#'
#'   with \eqn{Y_{2i} - Y_{1i}}{Y2i - Y1i} the difference score beetween DV
#'   conditions for the outcome variable for the \emph{i}th observation,
#'   \eqn{M_{2i} - M_{1i}}{M2i - M1i} the difference score between DV conditions
#'   for the mediator variable for the \emph{i}th observation, \eqn{M_{1i} +
#'   M_{2i}}{M1i + M2i} the sum of mediator variables values for DV conditions
#'   for the \emph{i}th observation, and \eqn{\overline{M_{1} + M_{2}}}{mean(M1i
#'   + M2i)} the mean sum of mediator variables values for DV conditions across
#'   observations (see Montoya and Hayes, 2011).
#'   
#'   Coefficients associated with \eqn{a}, \eqn{b}, \eqn{c}, and \eqn{c'} paths
#'   are respectively \eqn{a_{21}}{a21}, \eqn{b_{32}}{b32}, \eqn{c_{11}}{c11},
#'   and \eqn{c'_{31}}{c'31}.
#'   
#' @section Data formatting: To be consistant with other \code{mdt_*} family
#'   functions, \code{mdt_within} takes a long-format data frame as \code{data}
#'   argument. With this kind of format, each sampled unit has two rows, one for
#'   the first within-participant condition and one for the second
#'   within-participant condition. In addition, each row has one observation for
#'   the outcome and one observation for the mediator (see
#'   \code{\link{dohle_siegrist}} for an example.
#'
#'   Because such formatting is not the most common among social scientists
#'   intrested in within-participant mediation, \pkg{JSmediation} contains the
#'   \code{\link{mdt_within_wide}} function which handles long-formatted data
#'   input (but is syntax-inconsistant with other \code{mdt_*} family
#'   functions).
#'
#' @section Variable coding: Within-participant mediation model's underlying
#'   models uses difference scores as DV (see Models section). Because the
#'   function input does not allow the user to specify how the difference scores
#'   should be computed, \code{mdt_within} has a default coding.
#'
#'   \code{mdt_within}'s default behavior is to compute a difference score so
#'   the total effect (the effect of \eqn{X} on \eqn{Y}) will be positive and
#'   compute other difference scores accordingly.
#'
#'   User can change choose to have a negative total effect by using the
#'   \code{default_coding} argument.
#'
#'   Note that \code{DV} and \code{M} have to be numeric.
#'
#' @references Montoya, A. K., & Hayes, A. F. (2017). Two-condition
#'   within-participant statistical mediation analysis: A path-analytic
#'   framework. \emph{Psychological Methods}, \emph{22}(1), 6‑27. doi:
#'   10.1037/met0000086
#'   
#' @family mediation models
#'
#' @export
mdt_within <- function(data, IV, DV, M, grouping, default_coding = TRUE) {
  UseMethod("mdt_within")
}

#' @export
mdt_within.data.frame <- function(data, IV, DV, M, grouping, default_coding = TRUE) {

  # nse -----------------------------------------------------------------------
  IV_var       <- enquo(IV)
  DV_var       <- enquo(DV)
  M_var        <- enquo(M)
  grouping_var <- enquo(grouping)

  IV_name       <- rlang::quo_name(IV_var)
  DV_name       <- rlang::quo_name(DV_var)
  M_name        <- rlang::quo_name(M_var)
  grouping_name <- rlang::quo_name(grouping_var)

  IV_data <- data %>% dplyr::pull( !! IV_var )
  DV_data <- data %>% dplyr::pull( !! DV_var )
  M_data  <- data %>% dplyr::pull( !! M_var )

  # type check ----------------------------------------------------------------
  if(!is.character(IV_data))
    stop(glue::glue("Warning:
                    IV ({IV_name}) must be character."))

  if(!is.numeric(M_data))
    stop(glue::glue("Warning:
                    Mediator ({M_name}) must be numeric."))

  if(!is.numeric(M_data))
    stop(glue::glue("Warning:
                    DV ({DV_name}) must be numeric."))

  # data wrangling ------------------------------------------------------------
  # naming
  IV_cond <- data %>% dplyr::pull( !! IV_var ) %>% unique()

  M_cond_1_name <-
    glue::glue("{M_name}_mean_{IV_cond[[1]]}")
  M_cond_2_name <-
    glue::glue("{M_name}_mean_{IV_cond[[2]]}")

  M_mean_name <-
    glue::glue("{M_name}_mean")

  DV_cond_1_name <-
    glue::glue("{DV_name}_mean_{IV_cond[[1]]}")
  DV_cond_2_name <-
    glue::glue("{DV_name}_mean_{IV_cond[[2]]}")

  # wrangling
  wrangling_formula <-
    glue::glue("{grouping} ~ {IV}",
               grouping = grouping_name,
               IV       = IV_name) %>%
    stats::as.formula()

  data_long <-
    data.table::dcast(formula = wrangling_formula,
                      data = data.table::as.data.table(data),
                      fun.aggregate = list(mean, mean),
                      value.var = list(DV_name, M_name)) %>%
    tibble::as_tibble()

  DV_A_sup_B <- rlang::is_true(
    data_long %>% dplyr::pull(DV_cond_1_name) %>%  mean() >
      data_long %>% dplyr::pull(DV_cond_2_name) %>%  mean()
  )

  # little bit hacky:
  # if A > B and default_coding is true, set A - B, if
  # B < A and defaults coding is false, set A - B,
  # else, set B - A.
  if(DV_A_sup_B == default_coding) {
    DV_diff_name <-
      glue::glue("DV_{IV_cond[[1]]}_{IV_cond[[2]]}")
    M_diff_name <-
      glue::glue("IV_{IV_cond[[1]]}_{IV_cond[[2]]}")

    data_long <-
      data_long %>%
      dplyr::mutate( !! sym(DV_diff_name) := !! sym(DV_cond_1_name) - !! sym(DV_cond_2_name),
                     !! sym(M_diff_name)  := !! sym(M_cond_1_name)  - !! sym(M_cond_2_name))
  } else {
    DV_diff_name <-
      glue::glue("DV_{IV_cond[[2]]}_{IV_cond[[1]]}")

    M_diff_name <-
      glue::glue("M_{IV_cond[[2]]}_{IV_cond[[1]]}")

    data_long <-
      data_long %>%
      dplyr::mutate( !! sym(DV_diff_name) := !! sym(DV_cond_2_name) - !! sym(DV_cond_1_name),
                     !! sym(M_diff_name)  := !! sym(M_cond_2_name)  - !! sym(M_cond_1_name))
  }

  data_long <-
    data_long %>%
    dplyr::mutate(!! M_mean_name :=
                    scale((!! sym(M_cond_1_name) + !! sym(M_cond_2_name))/2, scale = FALSE))

  # bulding models ------------------------------------------------------------
  model1 <-
    stats::as.formula(glue::glue("{DV} ~ 1",
                                 DV = DV_diff_name))

  model2 <-
    stats::as.formula(glue::glue("{M}  ~ 1",
                                 M = M_diff_name))

  model3 <-
    stats::as.formula(glue::glue("{DV}  ~ 1 + {M} + {M_mean}",
                                 DV     = DV_diff_name,
                                 M      = M_diff_name,
                                 M_mean = M_mean_name))

  # model fitting and cleaning ------------------------------------------------
  js_models <-
    list("1 -> DV_diff"                   = model1,
         "1 -> M_diff"                    = model2,
         "1 + M_diff + M_mean -> DV_diff" = model3) %>%
    purrr::map(~lm(.x, data_long))

  # paths ---------------------------------------------------------------------
  paths <-
    list("a"  = create_path(js_models, "1 -> M_diff", "(Intercept)"),
         "b"  = create_path(js_models, "1 + M_diff + M_mean -> DV_diff", M_diff_name),
         "c"  = create_path(js_models, "1 -> DV_diff", "(Intercept)"),
         "c'" = create_path(js_models, "1 + M_diff + M_mean -> DV_diff", "(Intercept)"))

  # bulding mediation model object --------------------------------------------
  mediation_model(
    type      = "within-participant_mediation",
    params    = list("IV" = glue::glue("{IV_name} (difference: {score})",
                                       score = ifelse(DV_A_sup_B == default_coding,
                                                      paste0(IV_cond[[1]], " - ", IV_cond[[2]]),
                                                      paste0(IV_cond[[2]], " - ", IV_cond[[1]]))),
                     "DV" = DV_name,
                     "M"  = M_name),
    paths     = paths,
    js_models = js_models,
    data      = data_long,
    subclass  = "within_participant_mediation"
  )
}
