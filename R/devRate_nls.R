#' Compute non-linear regression
#'
#' Determine the nonlinear least-squares estimates of the parameters of a nonlinear model, on
#'   the basis of the \code{nls} function from package \code{stats}.
#'
#' @param eq The name of the equation.
#' @param temp The temperature.
#' @param devRate The developmental rate \code{(days)^-1}
#' @param startValues Starting values for the regression.
#' @param ... Additional arguments for the \code{nls} function.
#' @return An object of class \code{nls} (except for Stinner et al. 1974 and
#'   Lamb 1992 where the function returns a list of two objects of class \code{nls}).
#' @details \code{startValues} for equations by Stinner et al. 1974 and Lamb 1992 are composed of
#'   two equations: one for the temperatures below the optimal temperature and another for the
#'   tamperatures above the optimal temperature. For these equations, \code{startValues} should
#'   be a list of two lists, where the second element only contain starting estimates not
#'   specified in the first element, e.g., for Stinner et al.:
#'   \code{startValues <- list(list(C = 0.05, k1 = 5, k2 = -0.3), list(Topt = 30))},
#'   and for Lamb 1992:
#'   \code{startValues <- list(list(Rm = 0.05, Tmax = 35, To = 15), list(T1 = 4))}
#' @examples
#' myT <- 5:15
#' myDev <- -0.05 + rnorm(n = length(myT), mean = myT, sd = 1) * 0.01
#' myNLS <- devRateModel(eq = campbell_74, temp = myT, devRate = myDev,
#'   startValues = list(aa = 0, bb = 0))
#' myT <- seq(from = 0, to = 50, by = 10)
#' myDev <- c(0.001, 0.008, 0.02, 0.03, 0.018, 0.004)
#' myNLS <- devRateModel(eq = stinner_74, temp = myT, devRate = myDev,
#'   startValues = list(list(C = 0.05, k1 = 5, k2 = -0.3), list(Topt = 30)))
#' @export
devRateModel <- function(eq, temp, devRate, startValues, ...){
  ### handling exception for <stinner_74> and <lamb_92>
  if(eq$id == "eq040" | eq$id == "eq150"){
    tTh <- temp[devRate == max(devRate, na.rm = TRUE)][1]
    part1_temp <- temp[temp <= tTh]
    part2_temp <- temp[temp >= tTh]
    part1_devRate <- devRate[temp <= tTh]
    part2_devRate <- devRate[temp >= tTh]
    if(eq$id == "eq040") {
      nls_devRate1 <- stats::nls(
        formula = eq[[1]][[1]],
        data = data.frame(rT = part1_devRate, T = part1_temp),
        start = startValues[[1]],
        ...)
      newEq <- gsub("C", stats::coef(nls_devRate1)[1], eq$eqAlt[2])
      newEq <- gsub("k1", stats::coef(nls_devRate1)[2], newEq)
      newEq <- gsub("k2", stats::coef(nls_devRate1)[3], newEq)
      newEq <- paste0("rT ~ ", newEq)
      nls_devRate2 <- stats::nls(
        formula = newEq,
        data = data.frame(rT = part2_devRate, x = part2_temp),
        start = startValues[[2]])
      nls_devRate <- list(nls_devRate1, nls_devRate2)
    }
    if(eq$id == "eq150") {
      nls_devRate1 <- stats::nls(
        formula = eq[[1]][[1]],
        data = data.frame(rT = part1_devRate, T = part1_temp),
        start = startValues[[1]],
        ...)
      newEq <- gsub("Rm", stats::coef(nls_devRate1)[1], eq$eqAlt[2])
      newEq <- gsub("Tmax", stats::coef(nls_devRate1)[2], newEq)
      newEq <- gsub("To", stats::coef(nls_devRate1)[3], newEq)
      newEq <- paste0("rT ~ ", newEq)
      nls_devRate2 <- stats::nls(
        formula = newEq,
        data = data.frame(rT = part2_devRate, x = part2_temp),
        start = startValues[[2]],
        ...)
      nls_devRate <- list(nls_devRate1, nls_devRate2)
    }
  } else {
    nls_devRate <- stats::nls(formula = eq[[1]], data = data.frame(rT = devRate, T = temp), start = startValues, ...)
  }
  return(nls_devRate)
}
