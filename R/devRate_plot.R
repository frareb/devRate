#' Plot the empirical points and the regression
#'
#' @param eq The name of the equation.
#' @param nlsDR The result returned by the \code{devRateModel} function
#' @param temp The temperature.
#' @param devRate The developmental rate \code{(days)^-1}
#' @param rangeT The range of temperatures over which the regression is plotted.
#'   This argument may be overwritten depending on the equation.
#' @param optText A logical indcating wether the name of the eqaution should be written
#'   in the topright corner of the plot.
#'
#' @export
devRatePlot <- function(eq, nlsDR, temp, devRate, rangeT = 10, optText = TRUE, spe = TRUE, ...){
  minX <- -100
  maxX <- 100
  if(spe == TRUE){
    switch(EXPR = eq$id,
      "eq010" = { # janisch_32
        s <- seq(from = min(temp, na.rm = TRUE) - rangeT, to = max(temp, na.rm = TRUE) + rangeT, length = 100)
        plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Developmental rate", ...)
        lines(s, predict(nlsDR, newdata = list(T = s)), ...)
      },
      "eq020" = { # davidson_44
        maxX <- temp[devRate == max(devRate, na.rm = TRUE)][!is.na(temp[devRate == max(devRate, na.rm = TRUE)])][1]
        s <- seq(from = min(temp, na.rm = TRUE) - rangeT, to = max(temp, na.rm = TRUE) + rangeT, length = 100)
        plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Developmental rate", xlim = c(0, maxX),  ...)
        lines(s, predict(nlsDR, newdata = list(T = s)), ...)
      },
      "eq030" = { # campbell_74
        minX <- -coef(nlsDR)[1]/coef(nlsDR)[2]
        maxX <- max(temp, na.rm = TRUE)
        s1 <- seq(from = min(temp, na.rm = TRUE), to = min(max(temp, na.rm = TRUE), maxX), length = 100)
        s2 <- seq(from = minX, to = min(temp, na.rm = TRUE), length = 100)
        plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Developmental rate", xlim = range(c(s1, s2)), ...)
        lines(s1, predict(nlsDR, newdata = list(T = s1)), ...)
        lines(s2, predict(nlsDR, newdata = list(T = s2)), lty = 2, ...)
      },
      "eq040" = {},
      "eq050" = {},
      "eq060" = {},
      "eq070" = {},
      "eq080" = {},
      "eq090" = {},
      "eq100" = {},
      "eq110" = {},
      "eq120" = {},
      "eq130" = {},
      "eq140" = {},
      "eq150" = {},
      "eq160" = {},
      "eq170" = {},
      "eq180" = {},
      "eq190" = {},
      "eq200" = {},
      "eq210" = {},
      "eq220" = {},
      { # otherwise:
        s <- seq(from = min(temp, na.rm = TRUE) - rangeT, to = max(temp, na.rm = TRUE) + rangeT, length = 100)
        plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Developmental rate", ...)
        lines(s, predict(nlsDR, newdata = list(T = s)), ...)
      }
    )
  } else {
    s <- seq(from = min(temp, na.rm = TRUE) - rangeT, to = max(temp, na.rm = TRUE) + rangeT, length = 100)
    plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Developmental rate", ...)
    lines(s, predict(nlsDR, newdata = list(T = s)), ...)
  }

  if(optText == TRUE){
    text(x = par("xaxp")[2], y = par("yaxp")[2], pos = 2, paste0(eq$name, " (", eq$refShort, ")"), ...)
  }
}














devRatePlot(eq = campbell_74, nlsDR = tcampbell_74, temp = sampleDataset$T, devRate = sampleDataset$rT, ylim=c(0, 0.25))
