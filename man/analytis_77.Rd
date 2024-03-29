% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{analytis_77}
\alias{analytis_77}
\title{Analytis equation of development rate as a function of temperature.}
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
analytis_77
}
\description{
Analytis, S. (1977) Uber die Relation zwischen biologischer Entwicklung und Temperatur bei
phytopathogenen Pilzen. Journal of Phytopathology 90(1): 64-76.
}
\details{
Equation:
\deqn{rT = aa * (T - Tmin)^{bb} * (Tmax - T)^{cc}}{%
      rT = aa * (T - Tmin)^bb * (Tmax - T)^cc}

where rT is the development rate, T the temperature, Tmin the minimum
temperature, Tmax the maximum temperature, and aa, bb, and cc constants.
}
\references{
\doi{10.1111/j.1439-0434.1977.tb02886.x}
}
\keyword{datasets}
