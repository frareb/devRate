% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{bieri1_83}
\alias{bieri1_83}
\title{Bieri equation 1 of development rate as a function of temperature.}
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
bieri1_83
}
\description{
Bieri, M., Baumgartner, J., Bianchi, G., Delucchi, V., Arx, R. von. (1983)
Development and fecundity of pea aphid (Acyrthosiphon pisum Harris) as
affected by constant temperatures and by pea varieties. Mitteilungen der
Schweizerischen Entomologischen Gesellschaft, 56, 163-171.

Kumar, S., and Kontodimas, D.C. (2012). Temperature-dependent
development of Phenacoccus solenopsis under laboratory conditions.
Entomologia Hellenica, 21, 25-38.
}
\details{
Equation:
\deqn{rT = aa * (T - Tmin) - (bb * e^{T - Tm})}{%
      rT = aa * (T - Tmin) - (bb * exp(T - Tm))}

where rT is the development rate, T the temperature, Tmin the minimum
temperature, and aa, bb, and Tm fitted coefficients.
}
\references{
\url{http://www.e-periodica.ch}
}
\keyword{datasets}
