#' Lower thermal limit of performance
#'
#' @param nlsDR The result returned by the \code{devRateModel} function.
#' @param prec The precision for the temperature (default = 0.1 degree celsius).
#' @param lowTempLim The minimum temperature for the metrics (default = -30 degree celsius).
#' @param highTempLimit The maximum temperature for the metrics (default = +60 degree celsius).
#' @param devLimit The development rate considered as null (default = 0.001).
#' @param printOut A logical to print the result (default = FALSE).
#' @return A matrix with one column and one row for each metric. The metrics names
#'   are the names of each row.
#' @examples
#' rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
#'    13, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
#'    25, 0.200, 30, 0.200, 30, 0.180, 35, 0.001), ncol = 2, byrow = TRUE)
#' mEggs <- devRateModel(eq = taylor_81, temp = rawDevEggs[,1], devRate = rawDevEggs[,2],
#'    startValues = list(Rm = 0.05, Tm = 30, To = 5))
#' myMetrics <- dRGetMetrics(nlsDR = mEggs, printOut = TRUE)
#' @export
dRGetMetrics <- function(nlsDR, prec = 0.1, lowTempLim = -30, highTempLimit = 60, devLimit = 0.001, printOut = FALSE){
  devT <- seq(from = lowTempLim, to = highTempLimit, by = prec)
  devRT <- predict(nlsDR, newdata = list(T = devT))
  devRT[devRT < devLimit] <- 0

  ### TO DO ###
  ## make exception for Taylor using To parameter
  ## make exception for Campbell (linear model)
  ## ...

  topt <- devT[devRT == max(devRT)]
  cTmin <- max(devT[devRT < devLimit & devT < topt])
  cTmax <- min(devT[devRT < devLimit & devT > topt])
  tolRange <- cTmax - cTmin
  tbMin <- max(devT[devRT <= max(devRT)/2 & devT < topt])
  tbMax <- min(devT[devRT <= max(devRT)/2 & devT > topt])
  tbr <- tbMax - tbMin
  tsm <- cTmax - topt

  dfMetrics <- t(data.frame("Lower thermal limit of performance" = cTmin,
               "Upper theral limit of performance" = cTmax,
               "Temperature at which rT is maximized" = topt,
               "Temperature range between cTmax and cTmin" = tolRange,
               "Temperature min at 0.5 rT" = tbMin,
               "Temperature max at 0.5 rT" = tbMax,
               "Temperature range at 0.5 rT" = tbr,
               "Thermal safety margin" = tsm))
  if(printOut == TRUE){print(dfMetrics)}
  return(dfMetrics)
}
