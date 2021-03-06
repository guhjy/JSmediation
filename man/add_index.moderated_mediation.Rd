% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mdt_moderated_index.R
\name{add_index.moderated_mediation}
\alias{add_index.moderated_mediation}
\title{add_index method for moderated mediation}
\usage{
\method{add_index}{moderated_mediation}(mediation_model, iter = 5000,
  alpha = 0.05, stage = NULL, ...)
}
\arguments{
\item{mediation_model}{A mediation model of class
\code{"moderated_mediation"}.}

\item{iter}{Number of simulation to use to compute Monte Carlo indirect
effect confidence interval.}

\item{alpha}{Alpha threshold to use with the confidence interval.}

\item{stage}{Moderated indirect effect's stage on which to compute the
confidence interval. Can be either \code{1} (or \code{"first"}) or \code{2}
(or \code{"second"}). To compute total indirect effect moderation index,
use \code{"total"}.}

\item{...}{Further arguments passed to or from other methods.}
}
\description{
Adds confidence interval for the inderct effect's moderation to
a moderated mediation model fitted with \code{\link{mdt_moderated}}.
}
\details{
Indirect effect moderation index for within-participant mediation
uses \eqn{a}, \eqn{a \times Mod}{a * Mod}, \eqn{b} and \eqn{b \times Mod}{b
  * Mod} estimates and their standard error to compute the appropriate index
product distribution using Monte Carlo methods (see Muller, Judd, &
Yzerbyt, 2005).

\pkg{JSmediation} supports different type of mediated indirect effect
index:
\itemize{
\item{Stage 1:}{compute the product beetween \eqn{a \times Mod}{a
    * Mod} and \eqn{b}.}
\item{Stage 2:}{compute the product beetween \eqn{a} and \eqn{b \times
    Mod}{b * Mod}.}
\item{Total:}{compute the sum of Stage 1 and Stage 2 distribution.}
}
}
\examples{
## getting a stage 1 moderated indirect effect index
ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
                                       "Low discrimination",
                                       "High discrimination")
ho_et_al$linkedfate_c <- scale(ho_et_al$linkedfate, scale = FALSE)
ho_et_al$sdo_c <- scale(ho_et_al$sdo, scale = FALSE)
moderated_model <- mdt_moderated(data = ho_et_al,
                                 IV = condition_c,
                                 DV = hypodescent,
                                 M = linkedfate_c,
                                 Mod = sdo_c)
add_index(moderated_model, stage = 1)

}
\references{
Muller, D., Judd, C. M., & Yzerbyt, V. Y. (2005). When moderation
is mediated and mediation is moderated. \emph{Journal of Personality and
Social Psychology}, 89(6), 852‑863. doi: 10.1037/0022-3514.89.6.852
}
