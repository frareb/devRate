% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{logan10_76}
\alias{logan10_76}
\title{Logan et al. equation 10 of development rate as a function of temperature.}
\format{
A list of eight elements describing the equation.
\describe{
  \item{eq}{The equation (formula object).}
  \item{eqAlt}{The equation (string).}
  \item{name}{The name of the equation.}
  \item{ref}{The equation reference.}
  \item{refShort}{The equation reference shortened.}
  \item{startVal}{The parameters found in the literature with their references.}
  \item{com}{An optional comment about the equation use.}
  \item{id}{An id to identify the equation.}
}
}
\usage{
logan10_76
}
\description{
Logan, J. A., Wollkind, D. J., Hoyt, S. C., and Tanigoshi, L. K. (1976). An analytic model
for description of temperature dependent rate phenomena in arthropods. Environmental
Entomology, 5(6), 1133-1140.
}
\details{
Equation:
\deqn{rT = alpha * (\frac{1}{1 + cc * e^{- bb * T}} - e^{-\frac{Tmax - T}{deltaT}})}{%
      rT = alpha * (1/(1 + cc * exp(- bb * T)) - exp(-((Tmax - T)/deltaT)))}

where rT is the development rate, T the temperature, Tmax the maximum temperature,
deltaT the width of the high temperature boundary layer, and alpha and bb constants.
}
\references{
\doi{10.1093/ee/5.6.1133}
}
\keyword{datasets}
