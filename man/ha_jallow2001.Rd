% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/devRate_harmigera.R
\name{ha_jallow2001}
\alias{ha_jallow2001}
\title{Jallow and Matsumura linear thermal performance curve for the
  development of Helicoverpa armigera}
\usage{
ha_jallow2001(plotfig = TRUE)
}
\arguments{
\item{plotfig}{A Boolean used to return the experimental points and the
equation fitted in the article.}
}
\value{
A list with the equation used, and a list of model parameters for
  the different life stages considered in the article.
}
\description{
Linear development performance curve for eggs, larvae and
  pupae from nine experimental temperatures (10, 13.3, 16.4, 20, 22.5, 25,
  27.9, 30.5, and 32.5 degrees Celsius).
}
\details{
This work is part of the ACOMPLI project. The ACOMPLI project
  is part of the Strategic Action Plan for the anticipation of the potential
  European withdrawal of active substances and the development of
  alternative crop protection techniques (PARSADA). It is financed by
  ecological planning funds. The French Ministry of Agriculture cannot be
  held responsible for the content of this package.
}
\examples{
  mymodel <- ha_jallow2001(plotfig = FALSE)
}
\seealso{
Jallow, M. F., and Matsumura, M. (2001). Influence of temperature
  on the rate of development of Helicoverpa armigera (Hubner)(Lepidoptera:
  Noctuidae). Applied Entomology and Zoology, 36(4), 427-430.
  https://doi.org/10.1303/aez.2001.427
}
