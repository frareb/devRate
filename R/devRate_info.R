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
#' @examples
#' devRatePlotInfo(eq = campbell_74)
#' @export
devRatePlotInfo <- function(eq, sortBy = "sp", ...){
  listPlot <- split(eq$startVal, eq$startVal[sortBy])
  plot(0, type = "n", xlab = "Temperature", ylab = "Developmental rate", ...)
  x <- seq(from = 0, to = 50, length.out = 100)
  for(i in 1:length(listPlot)){
    for(j in 1:nrow(listPlot[[i]])){
      switch(EXPR = eq$id,
             "eq010" = {
               Dmin <- listPlot[[i]][j, 5]
               Topt <- listPlot[[i]][j, 6]
               aa <- listPlot[[i]][j, 7]
               bb <- listPlot[[i]][j, 8]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq020" = {
               aa <- listPlot[[i]][j, 5]
               bb <- listPlot[[i]][j, 6]
               K <- listPlot[[i]][j, 7]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq030" = {
               aa <- listPlot[[i]][j, 5]
               bb <- listPlot[[i]][j, 6]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq040" = {
               C <- listPlot[[i]][j, 5]
               k1 <- listPlot[[i]][j, 6]
               k2 <- listPlot[[i]][j, 7]
               Topt <- listPlot[[i]][j, 8]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt[1]))))
               curve(fx, add = TRUE, col = i, from = 0, to = Topt)
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt[2]))))
               curve(fx, add = TRUE, col = i, from = Topt, to = 60)
             },
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
             {
              # otherwise
             }
      )
    }
  }
  legend("topleft", legend = names(listPlot), col = 1:length(listPlot), lwd = 1)
}



### testing area
# for(i in names(devRateEqList)){devRateInfo(eq = get(i))}
# for(i in names(devRateEqList)){devRatePlotInfo(eq = get(i), sortBy = "ordersp", xlim = c(0,50), ylim = c(0,0.2))}
#
# devRatePlotInfo(eq = campbell_74, sortBy = "ordersp", xlim = c(0,30), ylim = c(0,0.2))
# devRatePlotInfo(eq = campbell_74, sortBy = "familysp", xlim = c(0,30), ylim = c(0,0.2))
devRateInfo(eq = janisch_32)
devRateInfo(eq = davidson_44)
devRateInfo(eq = campbell_74)
devRateInfo(eq = stinner_74)

devRatePlotInfo(eq = janisch_32, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.05))
devRatePlotInfo(eq = davidson_44, sortBy = "sp", xlim = c(0,40), ylim = c(0,0.05))
devRatePlotInfo(eq = campbell_74, sortBy = "sp", xlim = c(0,30), ylim = c(0,0.05))
devRatePlotInfo(eq = stinner_74, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.05))



