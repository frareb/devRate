#' Lower thermal limit of performance
#'
#'
#' @examples
#' rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
#'    13, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
#'    25, 0.200, 30, 0.200, 30, 0.180, 35, 0.001), ncol = 2, byrow = TRUE)
#' mEggs <- devRateModel(eq = taylor_81, temp = rawDevEggs[,1], devRate = rawDevEggs[,2],
#'    startValues = list(Rm = 0.05, Tm = 30, To = 5))
#' dRGetMetrics(myNLS = mEggs)
dRGetMetrics <- function(myNLS, prec = 0.1, devLimit = 0.001, printOut = TRUE){
  devT <- seq(from = -30, to = 60, by = prec)
  devRT <- predict(mEggs, newdata = list(T = devT))
  devRT[devRT < devLimit] <- 0

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
}
