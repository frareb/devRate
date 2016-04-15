devRatePlot <- function(eq, nlsDR, temp, devRate, rangeT = 5, optText = TRUE, ...){
  plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Developmental rate", ...)
  s <- seq(from = min(temp, na.rm = TRUE) - rangeT, to = max(temp, na.rm = TRUE) + rangeT, length = 100)
  lines(s, predict(nlsDR, newdata = list(T = s)), ...)
  if(optText == TRUE){
    text(x = par("xaxp")[2], y = par("yaxp")[2], pos = 2, paste0(eq$name, " (", eq$refShort, ")"), ...)
  }
}

### ne marche pas pour toutes les équations, eg Taylor 1981 truncated at To...
