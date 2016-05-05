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
             "eq050" = {
               phi <- listPlot[[i]][j, 5]
               bb <- listPlot[[i]][j, 6]
               Tmax <- listPlot[[i]][j, 7]
               deltaT <- listPlot[[i]][j, 8]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq060" = {
               alpha <- listPlot[[i]][j, 5]
               bb <- listPlot[[i]][j, 6]
               cc <- listPlot[[i]][j, 7]
               Tmax <- listPlot[[i]][j, 8]
               deltaT <- listPlot[[i]][j, 9]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq070" = {
               aa <- listPlot[[i]][j, 5]
               bb <- listPlot[[i]][j, 6]
               cc <- listPlot[[i]][j, 7]
               dd <- listPlot[[i]][j, 8]
               ff <- listPlot[[i]][j, 9]
               gg <- listPlot[[i]][j, 10]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq080" = {
               aa <- listPlot[[i]][j, 5]
               bb <- listPlot[[i]][j, 6]
               cc <- listPlot[[i]][j, 7]
               Tmin <- listPlot[[i]][j, 8]
               Tmax <- listPlot[[i]][j, 9]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq090" = {
               p25 <- listPlot[[i]][j, 5]
               aa <- listPlot[[i]][j, 6]
               bb <- listPlot[[i]][j, 7]
               cc <- listPlot[[i]][j, 8]
               dd <- listPlot[[i]][j, 9]
               ee <- listPlot[[i]][j, 10]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq100" = {
               Rm <- listPlot[[i]][j, 5]
               Tm <- listPlot[[i]][j, 6]
               To <- listPlot[[i]][j, 7]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq110" = {
               a0 <- listPlot[[i]][j, 5]
               a1 <- listPlot[[i]][j, 6]
               a2 <- listPlot[[i]][j, 7]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq120" = {
               a0 <- listPlot[[i]][j, 5]
               a1 <- listPlot[[i]][j, 6]
               a2 <- listPlot[[i]][j, 7]
               a3 <- listPlot[[i]][j, 8]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq130" = {
               a0 <- listPlot[[i]][j, 5]
               a1 <- listPlot[[i]][j, 6]
               a2 <- listPlot[[i]][j, 7]
               a3 <- listPlot[[i]][j, 8]
               a4 <- listPlot[[i]][j, 9]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq140" = {
               phi <- listPlot[[i]][j, 5]
               aa <- listPlot[[i]][j, 6]
               Tmax <- listPlot[[i]][j, 7]
               deltaT <- listPlot[[i]][j, 8]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq150" = {
               Rm <- listPlot[[i]][j, 5]
               Tmax <- listPlot[[i]][j, 6]
               To <- listPlot[[i]][j, 7]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq160" = {
               aa <- listPlot[[i]][j, 5]
               Tmax <- listPlot[[i]][j, 6]
               deltaT <- listPlot[[i]][j, 7]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq170" = {
               aa <- listPlot[[i]][j, 5]
               Tmax <- listPlot[[i]][j, 6]
               deltaT <- listPlot[[i]][j, 7]
               bb <- listPlot[[i]][j, 8]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq180" = {
               aa <- listPlot[[i]][j, 5]
               Tmin <- listPlot[[i]][j, 6]
               Tmax <- listPlot[[i]][j, 7]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq190" = {
               aa <- listPlot[[i]][j, 5]
               Tmax <- listPlot[[i]][j, 6]
               Tmin <- listPlot[[i]][j, 7]
               bb <- listPlot[[i]][j, 8]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq200" = {
               aa <- listPlot[[i]][j, 5]
               Tmin <- listPlot[[i]][j, 6]
               Tmax <- listPlot[[i]][j, 7]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq210" = {
               aa <- listPlot[[i]][j, 5]
               bb <- listPlot[[i]][j, 6]
               cc <- listPlot[[i]][j, 7]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
             "eq220" = {
               aa <- listPlot[[i]][j, 5]
               bb <- listPlot[[i]][j, 6]
               cc <- listPlot[[i]][j, 7]
               fx <- as.function(alist(x=, eval(parse(text=eq$eqAlt))))
               curve(fx, add = TRUE, col = i)
             },
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
devRateInfo(eq = logan6_76)
devRateInfo(eq = logan10_76)
devRateInfo(eq = sharpeDeMichele_77)
devRateInfo(eq = analytis_77)
devRateInfo(eq = schoolfield_81)
devRateInfo(eq = taylor_81)
devRateInfo(eq = poly2)
devRateInfo(eq = harcourtYee_82)
devRateInfo(eq = poly4)
devRateInfo(eq = hilbertLogan_83)
devRateInfo(eq = lamb_92)
devRateInfo(eq = lactin1_95)
devRateInfo(eq = lactin2_95)
devRateInfo(eq = briere1_99)
devRateInfo(eq = briere2_99)
devRateInfo(eq = kontodimas_04)
devRateInfo(eq = damos_08)
devRateInfo(eq = damos_11)

devRatePlotInfo(eq = janisch_32, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.05))
devRatePlotInfo(eq = davidson_44, sortBy = "sp", xlim = c(0,40), ylim = c(0,0.05))
devRatePlotInfo(eq = campbell_74, sortBy = "sp", xlim = c(0,30), ylim = c(0,0.05))
devRatePlotInfo(eq = stinner_74, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.05))
devRatePlotInfo(eq = logan6_76, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.1))
devRatePlotInfo(eq = logan10_76, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.1))
devRatePlotInfo(eq = sharpeDeMichele_77, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.2))


devRatePlotInfo(eq = analytis_77, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.05))
devRatePlotInfo(eq = schoolfield_81, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.05))
devRatePlotInfo(eq = taylor_81, sortBy = "ordersp", xlim = c(-20,80), ylim = c(0,20))
devRatePlotInfo(eq = poly2, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.05))
devRatePlotInfo(eq = harcourtYee_82, sortBy = "sp", xlim = c(0,60), ylim = c(0,1))
devRatePlotInfo(eq = poly4, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.5))
# devRatePlotInfo(eq = hilbertLogan_83, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.5))
# devRatePlotInfo(eq = lamb_92, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.05))
devRatePlotInfo(eq = lactin1_95, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.8))
devRatePlotInfo(eq = lactin2_95, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.8))
# devRatePlotInfo(eq = briere1_99, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.05))
devRatePlotInfo(eq = briere2_99, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.05))
devRatePlotInfo(eq = kontodimas_04, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.8))
devRatePlotInfo(eq = damos_08, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.08))
devRatePlotInfo(eq = damos_11, sortBy = "sp", xlim = c(0,60), ylim = c(0,0.05))

