% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{rootsq_82}
\alias{rootsq_82}
\title{Root square equation of development rate as a function of temperature.}
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
rootsq_82
}
\description{
Ratkowsky, D.A., Olley, J., McMeekin, T.A., and Ball, A. (1982) Relationship between
temperature and growth rate of bacterial cultures. Journal of Bacteriology 149(1): 1-5.
}
\details{
Equation:
\deqn{rT = (bb * (T - Tb))^{2}}{%
      rT = (bb * (T - Tb))^2}

where rT is the development rate, T the temperature, bb the slope of the
regression line, and Tb a conceptual temperature of no metabolic significance.
}
\references{
\doi{10.1128/jb.149.1.1-5.1982}
}
\keyword{datasets}
