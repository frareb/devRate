devRateModel <- function(eq, temp, devRate, startValues = 0){
  nls_devRate <- nls(eq[[1]], data = data.frame(rT = devRate, T = temp), start = startValues)
  # nls_devRate <- minpack.lm::nlsLM(eq[[1]], data = data.frame(rT = devRate, T = temp), start = startValues)
  return(nls_devRate)
}
