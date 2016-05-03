#' Display information about an equation
#'
#' @param eq The name of the equation.
#' @return Nothing.
#' @examples
#' devRateInfo(eq = campbell_74)
#' @export
devRateInfo <- function(eq){
  cat("----------------------------------------\n")
  cat(as.character(eq$name), "\n----------------------------------------\n")
  cat(strwrap(x = eq$ref, width = 80), sep = "\n")
  cat("\n")
  print(eq$eq)
  if(nrow(eq$startVal) > 0){
    cat("\nParameter estimates from the literature (eq$startVal): \n\n")
    print(eq$startVal)
  }
  if(length(eq$com) > 0){cat("\nComments: ", strwrap(x = eq$com, width = 80), sep = "\n")}
}


#' Plot parameter estimates from the literature
#'
#' @param eq The name of the equation.
#' @param sortBy The filter to seperate species ("ordersp", "family"sp, "sp").
#' @return Nothing.
#' @export
devRatePlotInfo <- function(eq, sortBy = "sp", ...){
  listPlot <- split(eq$startVal, eq$startVal[sortBy])
  plot(0, type = "n", xlab = "Temperature", ylab = "Developmental rate", ...)
  for(i in 1:length(listPlot)){
    for(j in 1:nrow(listPlot[[i]])){
      switch(EXPR = eq$id,
             "eq030" = {
               aa <- listPlot[[i]][j, 5]
               bb <- listPlot[[i]][j, 6]
               x <- seq(from = 0, to = 50, length.out = 100)
               # rT <- aa + bb * T
               # lines(x = T, y = rT, col = i)
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             {}
      )
    }
  }
  legend("topleft", legend = names(listPlot), col = 1:length(listPlot), lwd = 1)
}

# for(i in names(devRateEqList)){devRateInfo(eq = get(i))}


devRatePlotInfo(eq = campbell_74, sortBy = "ordersp", xlim = c(0,30), ylim = c(0,0.2))
devRatePlotInfo(eq = campbell_74, sortBy = "familysp", xlim = c(0,30), ylim = c(0,0.2))





