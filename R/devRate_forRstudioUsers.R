rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
                       16, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
                       25, 0.200, 30, 0.200, 30, 0.180), ncol = 2, byrow = TRUE)
temp <- rawDevEggs[, 1]
devRate <- rawDevEggs[, 2]
# eq <- analytis_77


devRateFindStartNls <- function(temp, devRate, eq){
  library(manipulate)
  switch(EXPR = eq$id,
       "eq010" = {
        manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, max(devRate, na.rm = TRUE)),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
           } +
           points(x = xx, y = (Dmin/2 * (exp(aa*(xx - Topt)) + exp(-bb*(xx - Topt))))^(-1),
                  type = 'l', lwd = 3),
           Dmin = slider(0, 50, step = 0.005, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           Topt = slider(0, 50, step = 0.005, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           aa = slider(0, 1, step = 0.005, initial = mean(eq$startVal[, 9], na.rm = TRUE)),
           bb = slider(0, 1, step = 0.005, initial = mean(eq$startVal[, 10], na.rm = TRUE))
        )
       },
       "eq020" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, max(devRate, na.rm = TRUE)),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
           } +
             points(x = xx, y = K / (1 + exp(aa + bb * xx)),
                    type = 'l', lwd = 3),
          aa = slider(0, 10, step = 0.005, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
          bb = slider(-0.5, 0, step = 0.005, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
          K = slider(0, 1, step = 0.005, initial = mean(eq$startVal[, 9], na.rm = TRUE))
         )
       },
       "eq030" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, max(devRate, na.rm = TRUE)),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
           } +
             points(x = xx, y = aa + bb * xx,
                    type = 'l', lwd = 3),
           aa = slider(-1, 0, step = 0.005, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           bb = slider(0, 0.1, step = 0.0005, initial = mean(eq$startVal[, 8], na.rm = TRUE))
         )
       },
       "eq040" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, max(devRate, na.rm = TRUE)),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
           } +
           points(x = xx, y = C / (1 + exp(k1 + k2 * xx)),
                    type = 'l', lwd = 3) +
           points(x = xx, y = C / (1 + exp(k1 + k2 * (2 * Topt - xx))),
                  type = 'l', lwd = 3),
           C = slider(0, 1, step = 0.005, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           k1 = slider(0, 15, step = 0.0005, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           k2 = slider(-1, 0, step = 0.0005, initial = mean(eq$startVal[, 9], na.rm = TRUE)),
           Topt = slider(0, 50, step = 0.0005, initial = mean(eq$startVal[, 10], na.rm = TRUE))
         )
       },
       "eq050" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, max(devRate, na.rm = TRUE)),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
           } +
             points(x = xx, y = phi * (exp(bb * xx) - exp(bb * Tmax - (Tmax - xx)/deltaT)),
                    type = 'l', lwd = 3),
           phi = slider(-0.5, 1, step = 0.005, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           bb = slider(0, 0.5, step = 0.0005, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           Tmax = slider(0, 50, step = 0.0005, initial = mean(eq$startVal[, 9], na.rm = TRUE)),
           deltaT = slider(0, 10, step = 0.0005, initial = mean(eq$startVal[, 10], na.rm = TRUE))
         )
       },
       "eq060" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, max(devRate, na.rm = TRUE)),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
           } +
             points(x = xx, y = alpha * (1/(1 + cc * exp(- bb * xx)) - exp(-((Tmax - xx)/deltaT))),
                    type = 'l', lwd = 3),
           alpha = slider(0, 1, step = 0.005, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           bb = slider(-0.5, 0.5, step = 0.0005, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           cc = slider(0, 500, step = 10, initial = mean(eq$startVal[, 9], na.rm = TRUE)),
           Tmax = slider(0, 50, step = 0.05, initial = mean(eq$startVal[, 10], na.rm = TRUE)),
           deltaT = slider(0, 10, step = 0.0005, initial = mean(eq$startVal[, 11], na.rm = TRUE))
         )
       },
       "eq070" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, max(devRate, na.rm = TRUE)),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
           } +
             points(x = xx, y = ((xx + 273.16) * exp((aa - bb/(xx + 273.16))/1.987)) / (1 + exp((cc - dd/(xx + 273.16))/1.987) + exp((ff - gg/(xx + 273.16))/1.987)),
                    type = 'l', lwd = 3),
           aa = slider(0, 100, step = 0.1, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           bb = slider(0, 100000, step = 10, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           cc = slider(-1000, 0, step = 1, initial = mean(eq$startVal[, 9], na.rm = TRUE)),
           dd = slider(-500000, 0, step = 10, initial = mean(eq$startVal[, 10], na.rm = TRUE)),
           ff = slider(0, 1000, step = 1, initial = mean(eq$startVal[, 11], na.rm = TRUE)),
           gg = slider(0, 100000, step = 1, initial = mean(eq$startVal[, 12], na.rm = TRUE))
         )
       },
       "eq080" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, max(devRate, na.rm = TRUE)),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
           } +
             points(x = xx, y = aa * (xx - Tmin)^bb * (Tmax - xx)^cc,
                    type = 'l', lwd = 3),
           aa = slider(0, 0.01, step = 0.0000001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           bb = slider(0, 10, step = 0.001, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           cc = slider(0, 1, step = 0.001, initial = mean(eq$startVal[, 9], na.rm = TRUE)),
           Tmin = slider(-15, 15, step = 0.1, initial = mean(eq$startVal[, 10], na.rm = TRUE)),
           Tmax = slider(15, 50, step = 0.1, initial = mean(eq$startVal[, 11], na.rm = TRUE))
         )
       },
       "eq090" = {
         p25 <- listPlot[[i]][j, colparam]
         aa <- listPlot[[i]][j, colparam + 1]
         bb <- listPlot[[i]][j, colparam + 2]
         cc <- listPlot[[i]][j, colparam + 3]
         dd <- listPlot[[i]][j, colparam + 4]
         ee <- listPlot[[i]][j, colparam + 5]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq091" = {
         p25 <- listPlot[[i]][j, colparam]
         aa <- listPlot[[i]][j, colparam + 1]
         dd <- listPlot[[i]][j, colparam + 2]
         ee <- listPlot[[i]][j, colparam + 3]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq092" = {
         p25 <- listPlot[[i]][j, colparam]
         aa <- listPlot[[i]][j, colparam + 1]
         bb <- listPlot[[i]][j, colparam + 2]
         cc <- listPlot[[i]][j, colparam + 3]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq100" = {
         Rm <- listPlot[[i]][j, colparam]
         Tm <- listPlot[[i]][j, colparam + 1]
         To <- listPlot[[i]][j, colparam + 2]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i, to = Tm + To)
       },
       "eq110" = {
         a0 <- listPlot[[i]][j, colparam]
         a1 <- listPlot[[i]][j, colparam + 1]
         a2 <- listPlot[[i]][j, colparam + 2]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq120" = {
         a0 <- listPlot[[i]][j, colparam]
         a1 <- listPlot[[i]][j, colparam + 1]
         a2 <- listPlot[[i]][j, colparam + 2]
         a3 <- listPlot[[i]][j, colparam + 3]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq130" = {
         a0 <- listPlot[[i]][j, colparam]
         a1 <- listPlot[[i]][j, colparam + 1]
         a2 <- listPlot[[i]][j, colparam + 2]
         a3 <- listPlot[[i]][j, colparam + 3]
         a4 <- listPlot[[i]][j, colparam + 4]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq140" = {
         phi <- listPlot[[i]][j, colparam]
         aa <- listPlot[[i]][j, colparam + 1]
         Tb <- listPlot[[i]][j, colparam + 2]
         Tmax <- listPlot[[i]][j, colparam + 3]
         deltaT <- listPlot[[i]][j, colparam + 4]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq150" = {
         Rm <- listPlot[[i]][j, colparam]
         Tmax <- listPlot[[i]][j, colparam + 1]
         To <- listPlot[[i]][j, colparam + 2]
         T1 <- listPlot[[i]][j, colparam + 3]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt[1]))))
         graphics::curve(fx, add = TRUE, col = i, from = 0, to = Tmax)
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt[2]))))
         graphics::curve(fx, add = TRUE, col = i, from = Tmax, to = 60)
       },
       "eq160" = {
         aa <- listPlot[[i]][j, colparam]
         Tmax <- listPlot[[i]][j, colparam + 1]
         deltaT <- listPlot[[i]][j, colparam + 2]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq170" = {
         aa <- listPlot[[i]][j, colparam]
         Tmax <- listPlot[[i]][j, colparam + 1]
         deltaT <- listPlot[[i]][j, colparam + 2]
         bb <- listPlot[[i]][j, colparam + 3]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq180" = {
         aa <- listPlot[[i]][j, colparam]
         Tmin <- listPlot[[i]][j, colparam + 1]
         Tmax <- listPlot[[i]][j, colparam + 2]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq190" = {
         aa <- listPlot[[i]][j, colparam]
         Tmax <- listPlot[[i]][j, colparam + 1]
         Tmin <- listPlot[[i]][j, colparam + 2]
         bb <- listPlot[[i]][j, colparam + 3]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq200" = {
         aa <- listPlot[[i]][j, colparam]
         Tmin <- listPlot[[i]][j, colparam + 1]
         Tmax <- listPlot[[i]][j, colparam + 2]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq210" = {
         aa <- listPlot[[i]][j, colparam]
         bb <- listPlot[[i]][j, colparam + 1]
         cc <- listPlot[[i]][j, colparam + 2]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq220" = {
         aa <- listPlot[[i]][j, colparam]
         bb <- listPlot[[i]][j, colparam + 1]
         cc <- listPlot[[i]][j, colparam + 2]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq230" = {
         K <- listPlot[[i]][j, colparam]
         r <- listPlot[[i]][j, colparam + 1]
         T0 <- listPlot[[i]][j, colparam + 2]
         TL <- listPlot[[i]][j, colparam + 3]
         TH <- listPlot[[i]][j, colparam + 4]
         aa <- listPlot[[i]][j, colparam + 5]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq240" = {
         p1 <- listPlot[[i]][j, colparam]
         p2 <- listPlot[[i]][j, colparam + 1]
         p3 <- listPlot[[i]][j, colparam + 2]
         p4 <- listPlot[[i]][j, colparam + 3]
         p5 <- listPlot[[i]][j, colparam + 4]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq250" = {
         cc <- listPlot[[i]][j, colparam]
         k1 <- listPlot[[i]][j, colparam + 1]
         k2 <- listPlot[[i]][j, colparam + 2]
         T1 <- listPlot[[i]][j, colparam + 3]
         T2 <- listPlot[[i]][j, colparam + 4]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq260" = {
         phi <- listPlot[[i]][j, colparam]
         bb <- listPlot[[i]][j, colparam + 1]
         Tb <- listPlot[[i]][j, colparam + 2]
         Tm <- listPlot[[i]][j, colparam + 3]
         deltab <- listPlot[[i]][j, colparam + 4]
         deltam <- listPlot[[i]][j, colparam + 5]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq270" = {
         cc <- listPlot[[i]][j, colparam]
         k1 <- listPlot[[i]][j, colparam + 1]
         T1 <- listPlot[[i]][j, colparam + 2]
         k2 <- listPlot[[i]][j, colparam + 3]
         T2 <- listPlot[[i]][j, colparam + 4]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq280" = {
         Tmin <- listPlot[[i]][j, colparam]
         aa <- listPlot[[i]][j, colparam + 1]
         Topt <- listPlot[[i]][j, colparam + 2]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq290" = {
         bb <- listPlot[[i]][j, colparam]
         Tb <- listPlot[[i]][j, colparam + 1]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq300" = {
         cc <- listPlot[[i]][j, colparam]
         T1 <- listPlot[[i]][j, colparam + 1]
         k <- listPlot[[i]][j, colparam + 2]
         T2 <- listPlot[[i]][j, colparam + 3]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq310" = {
         mu <- listPlot[[i]][j, colparam]
         Tb <- listPlot[[i]][j, colparam + 1]
         aa <- listPlot[[i]][j, colparam + 2]
         Tc <- listPlot[[i]][j, colparam + 3]
         bb <- listPlot[[i]][j, colparam + 4]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq320" = {
         aa <- listPlot[[i]][j, colparam]
         bb <- listPlot[[i]][j, colparam + 1]
         cc <- listPlot[[i]][j, colparam + 2]
         dd <- listPlot[[i]][j, colparam + 3]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq330" = {
         aa <- listPlot[[i]][j, colparam]
         bb <- listPlot[[i]][j, colparam + 1]
         cc <- listPlot[[i]][j, colparam + 2]
         dd <- listPlot[[i]][j, colparam + 3]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       "eq340" = {
         aa <- listPlot[[i]][j, colparam]
         bb <- listPlot[[i]][j, colparam + 1]
         Tm <- listPlot[[i]][j, colparam + 2]
         Tmin <- listPlot[[i]][j, colparam + 3]
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt))))
         graphics::curve(fx, add = TRUE, col = i)
       },
       {
         # otherwise nothing
       }
  )

}





