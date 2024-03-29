% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{regniere_12}
\alias{regniere_12}
\title{Regniere equation of development rate as a function of temperature.}
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
regniere_12
}
\description{
Regniere, J., Powell, J., Bentz, B., and Nealis, V. (2012) Effects of temperature on
development, survival and reproduction of insects: experimental design, data analysis
and modeling. Journal of Insect Physiology 58(5): 634-47.
}
\details{
Equation:
\deqn{rT = phi * (e^{bb * (T - Tb)} - \frac{Tm - T}{Tm - Tb} * e^{-bb * \frac{T - Tb}{deltab}} - \frac{T - Tb}{Tm - Tb} * e^{\frac{bb * (Tm - Tb) - (Tm - T)}{deltam}})}{%
      rT = phi * (exp(bb * (T - Tb)) - ((Tm - T) / (Tm - Tb)) * exp(-bb * (T - Tb) / deltab) - ((T - Tb)/(Tm - Tb)) * exp(bb * (Tm - Tb) - (Tm - T)/deltam))}

where rT is the development rate, T the temperature, Tb the minimum
temperature, Tm the maximum temperature and phi, bb, deltab,
and deltam constants (see source for more details).
}
\references{
\doi{10.1016/j.jinsphys.2012.01.010}
}
\keyword{datasets}
