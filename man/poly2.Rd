% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{poly2}
\alias{poly2}
\title{Second-order polynomial equation of development rate as a function of temperature.}
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
poly2
}
\description{
A simple second-order polynomial equation.
}
\details{
Equation:
\deqn{rT = a0 + a1 * T + a2 * T^{2}}{%
      rT = a0 + a1 * T + a2 * T^2}

where rT is the development rate, T the temperature, and a0, a1, and a2 are
constants.
}
\keyword{datasets}
