#' Predict developmental rate from a a map of temperatures
#'
#' Create a map from a temperature matrix and the developmental rate curve
#'
#' @param nlsDR The result returned by the \code{devRateModel} function.
#' @param tempMap A matrix containing temperatures in degrees.
#' @return A matrix with developmental rates predicted from the model.
#' @examples
#' myT <- 5:15
#' myDev <- -0.05 + rnorm(n = length(myT), mean = myT, sd = 1) * 0.01
#' myNLS <- devRateModel(eq = campbell_74, temp = myT, devRate = myDev,
#'   startValues = list(aa = 0, bb = 0))
#' myMap <- devRateMap(nlsDR =  myNLS, tempMap = matrix(rnorm(100, mean = 12, sd = 2), ncol=10))
#' @export
devRateMap <- function(nlsDR, tempMap){
  mapDev <- matrix(stats::predict(nlsDR, newdata = list(T = c(tempMap))), ncol = ncol(tempMap))
  mapDev[mapDev < 0] <- 0
  return(mapDev)
}
