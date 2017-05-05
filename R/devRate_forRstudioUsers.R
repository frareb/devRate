# A wrapper for the manipulate function to get parameter estimates for NLS
#
# This is a wrapper for manipulate function using sliders for all equations
# in the devRate package in order to have good first guess estimates of the
# parameters in the NLS process (devRateModel function).
# It requiers the manipulate package to be installed in order to work:
# install.packages("manipulate")
#
# devRateFindStartNls is not part of the R CRAN package because it depends
# on the manipulate package which works only for Rstudio. This function is
# available online in the GitHub page of the devRate package.
#
# There are three parameters:
# - temp: the dataset temperatures
# - devRate: the dataset development rate
# - eq: the equation to be fitted to the dataset
#
# example:
# rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
#   16, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
#   25, 0.200, 30, 0.200, 30, 0.180, 50, 0.01), ncol = 2, byrow = TRUE)
# devRateFindStartNls(temp = rawDevEggs[, 1], devRate = rawDevEggs[, 2], eq = taylor_81)
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
       }, # linear model: no need for starting estimates
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
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, max(devRate, na.rm = TRUE)),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
           } +
             points(x = xx, y = (p25 * (xx + 273.16)/298 * exp(aa/1.987 * (1/298 - 1/(xx + 273.16)))) / (1 + exp(bb/1.987 * (1/cc - 1/(xx + 273.16))) + exp(dd/1.987 * (1/ee - 1/(xx + 273.16)))),
                    type = 'l', lwd = 3),
           p25 = slider(0, 1, step = 0.001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           aa = slider(0, 100000, step = 10, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           bb = slider(-50000, 0, step = 1, initial = mean(eq$startVal[, 9], na.rm = TRUE)),
           cc = slider(0, 1000, step = 0.1, initial = mean(eq$startVal[, 10], na.rm = TRUE)),
           dd = slider(0, 100000, step = 10, initial = mean(eq$startVal[, 11], na.rm = TRUE)),
           ee = slider(0, 10000, step = 1, initial = mean(eq$startVal[, 12], na.rm = TRUE))
         )
       },
       "eq091" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, max(devRate, na.rm = TRUE)),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
           } +
             points(x = xx, y = (p25 * (xx + 273.16)/298 * exp(aa/1.987 * (1/298 - 1/(xx + 273.16)))) / (1 + exp(dd/1.987 * (1/ee - 1/(xx + 273.16)))),
                    type = 'l', lwd = 3),
           p25 = slider(0, 1, step = 0.001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           aa = slider(0, 100000, step = 10, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           dd = slider(0, 100000, step = 10, initial = mean(eq$startVal[, 11], na.rm = TRUE)),
           ee = slider(0, 10000, step = 1, initial = mean(eq$startVal[, 12], na.rm = TRUE))
         )
       },
       "eq092" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, max(devRate, na.rm = TRUE)),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
           } +
             points(x = xx, y = (p25 * (xx + 273.16)/298 * exp(aa/1.987 * (1/298 - 1/(xx + 273.16)))) / (1 + exp(bb/1.987 * (1/cc - 1/(xx + 273.16)))),
                    type = 'l', lwd = 3),
           p25 = slider(0, 1, step = 0.001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           aa = slider(0, 100000, step = 10, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           bb = slider(-50000, 0, step = 1, initial = mean(eq$startVal[, 9], na.rm = TRUE)),
           cc = slider(0, 1000, step = 0.1, initial = mean(eq$startVal[, 10], na.rm = TRUE))
         )
       },
       "eq100" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, max(devRate, na.rm = TRUE)),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
           } +
             points(x = xx, y = Rm * exp(-1/2 * ((xx - Tm)/To)^2),
                    type = 'l', lwd = 3),
           Rm = slider(0, 1, step = 0.001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           Tm = slider(0, 50, step = 0.01, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           To = slider(-10, 15, step = 0.01, initial = mean(eq$startVal[, 9], na.rm = TRUE))
         )
       },
       "eq110" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, max(devRate, na.rm = TRUE)),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
           } +
             points(x = xx, y = a0 + a1 * xx + a2 * xx^2,
                    type = 'l', lwd = 3),
           a0 = slider(-0.2, 0, step = 0.001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           a1 = slider(0, 0.1, step = 0.0001, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           a2 = slider(-0.005, 0, step = 0.00001, initial = mean(eq$startVal[, 9], na.rm = TRUE))
         )
       }, # linear model: no need for starting estimates
       "eq120" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, max(devRate, na.rm = TRUE)),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
           } +
             points(x = xx, y = a0 + a1 * xx + a2 * xx^2 + a3 * xx^3,
                    type = 'l', lwd = 3),
           a0 = slider(-0.2, 1, step = 0.001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           a1 = slider(-0.2, 0.2, step = 0.0001, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           a2 = slider(-2, 0.1, step = 0.00001, initial = mean(eq$startVal[, 9], na.rm = TRUE)),
           a3 = slider(-0.1, 15, step = 0.00001, initial = mean(eq$startVal[, 10], na.rm = TRUE))
         )
       }, # linear model: no need for starting estimates
       "eq130" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, max(devRate, na.rm = TRUE)),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
           } +
             points(x = xx, y = a0 + a1 * xx + a2 * xx^2 + a3 * xx^3 + a4 * xx^4,
                    type = 'l', lwd = 3),
           a0 = slider(-1, 1, step = 0.001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           a1 = slider(-0.2, 0.5, step = 0.0001, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           a2 = slider(-0.1, 0.1, step = 0.00001, initial = mean(eq$startVal[, 9], na.rm = TRUE)),
           a3 = slider(-0.001, 0.001, step = 0.00001, initial = mean(eq$startVal[, 10], na.rm = TRUE)),
           a4 = slider(-0.00001, 0.00001, step = 0.0000001, initial = mean(eq$startVal[, 11], na.rm = TRUE))
         )
       }, # linear model: no need for starting estimates
       "eq140" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, max(devRate, na.rm = TRUE)),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
           } +
             points(x = xx, y = phi * (((xx-Tb)^2 / ((xx-Tb)^2 + aa^2)) - exp(-(Tmax - (xx-Tb))/deltaT)),
                    type = 'l', lwd = 3),
           phi = slider(0, 2, step = 0.0001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           aa = slider(0, 100, step = 0.0001, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           Tb = slider(0, 20, step = 0.0001, initial = mean(eq$startVal[, 9], na.rm = TRUE)),
           Tmax = slider(10, 50, step = 0.0001, initial = mean(eq$startVal[, 10], na.rm = TRUE)),
           deltaT = slider(0, 10, step = 0.0001, initial = mean(eq$startVal[, 11], na.rm = TRUE))
         )
       },
       "eq150" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, max(devRate, na.rm = TRUE)),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
           } +
             points(x = xx, y = Rm * exp(-1/2 * ((xx - Tmax)/To)^2),
                    type = 'l', lwd = 3, col = 4) +
             points(x = xx, y = Rm * exp(-1/2 * ((xx - Tmax)/T1)^2),
                    type = 'l', lwd = 3, col = 2),
           Rm = slider(0, 1, step = 0.0001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           Tmax = slider(0, 50, step = 0.0001, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           To = slider(0, 20, step = 0.0001, initial = mean(eq$startVal[, 9], na.rm = TRUE)),
           T1 = slider(-1, 5, step = 0.0001, initial = mean(eq$startVal[, 10], na.rm = TRUE))
         )
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt[1]))))
         graphics::curve(fx, add = TRUE, col = i, from = 0, to = Tmax)
         fx <- as.function(alist(x =, eval(parse(text = eq$eqAlt[2]))))
         graphics::curve(fx, add = TRUE, col = i, from = Tmax, to = 60)
       },
       "eq160" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, defYlim),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
             points(x = xx, y = exp(aa * xx) - exp(aa * Tmax - (Tmax - xx)/deltaT),
                    type = 'l', lwd = 3)
           },
           defYlim = slider(0, 10, step = 0.01, initial = 1),
           aa = slider(0, 1, step = 0.0001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           Tmax = slider(0, 100, step = 0.0001, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           deltaT = slider(0, 10, step = 0.0001, initial = mean(eq$startVal[, 9], na.rm = TRUE))
         )
       },
       "eq170" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, defYlim),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
             points(x = xx, y = exp(aa * xx) - exp(aa * Tmax - (Tmax - xx)/deltaT) + bb,
                    type = 'l', lwd = 3)
           },
           defYlim = slider(0, 10, step = 0.01, initial = 1),
           aa = slider(0, 1, step = 0.0001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           Tmax = slider(0, 100, step = 0.0001, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           deltaT = slider(0, 10, step = 0.0001, initial = mean(eq$startVal[, 9], na.rm = TRUE)),
           bb = slider(-2, 0, step = 0.0001, initial = mean(eq$startVal[, 10], na.rm = TRUE))
         )
       },
       "eq180" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, defYlim),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
             points(x = xx, y = aa * xx * (xx - Tmin) * (Tmax - xx)^(1 / 2),
                    type = 'l', lwd = 3)
           },
           defYlim = slider(0, 10, step = 0.01, initial = 1),
           aa = slider(0, 1, step = 0.0001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           Tmin = slider(0, 20, step = 0.0001, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           Tmax = slider(0, 100, step = 0.0001, initial = mean(eq$startVal[, 9], na.rm = TRUE))
         )
       },
       "eq190" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, defYlim),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
             points(x = xx, y = aa * xx * (xx - Tmin) * (Tmax - xx)^(1 / bb),
                    type = 'l', lwd = 3)
           },
           defYlim = slider(0, 5, step = 0.0001, initial = 1),
           aa = slider(0, 5, step = 0.0001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           Tmax = slider(0, 100, step = 0.0001, initial = mean(eq$startVal[, 9], na.rm = TRUE)),
           Tmin = slider(0, 20, step = 0.0001, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           bb = slider(0, 10, step = 0.0001, initial = mean(eq$startVal[, 10], na.rm = TRUE))
         )
       },
       "eq200" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, defYlim),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
             points(x = xx, y = aa * (xx - Tmin)^2 * (Tmax - xx),
                    type = 'l', lwd = 3)
           },
           defYlim = slider(0, 5, step = 0.01, initial = 1),
           aa = slider(0, 0.001, step = 0.000001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           Tmin = slider(0, 20, step = 0.0001, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           Tmax = slider(0, 100, step = 0.0001, initial = mean(eq$startVal[, 9], na.rm = TRUE))
         )
       },
       "eq210" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, defYlim),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
             points(x = xx, y = aa * (bb - xx / 10) * (xx / 10)^cc,
                    type = 'l', lwd = 3)
           },
           defYlim = slider(0, 10, step = 0.0001, initial = 1),
           aa = slider(0, 0.001, step = 0.000001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           bb = slider(0, 20, step = 0.0001, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           cc = slider(0, 100, step = 0.0001, initial = mean(eq$startVal[, 9], na.rm = TRUE))
         )
       },
       "eq220" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, defYlim),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
             points(x = xx, y = aa / (1 + bb * xx + cc * xx^2),
                    type = 'l', lwd = 3)
           },
           defYlim = slider(0, 10, step = 0.0001, initial = 1),
           aa = slider(0, 100, step = 0.0001, initial = 0.1),
           bb = slider(0, 20, step = 0.0001, initial = 1),
           cc = slider(0, 100, step = 0.0001, initial = 1)
         )
       },
       "eq230" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, defYlim),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
             points(x = xx, y = (K / (1 + exp(-r*(xx - T0)))) * (1 - exp(-(xx - TL)/aa)) * (1-exp(-(TH - xx)/aa)),
                    type = 'l', lwd = 3)
           },
           defYlim = slider(0, 10, step = 0.0001, initial = 1),
           K = slider(0, 1, step = 0.0001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           r = slider(0, 2, step = 0.0001, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           T0 = slider(0, 100, step = 0.0001, initial = mean(eq$startVal[, 9], na.rm = TRUE)),
           TL = slider(0, 20, step = 0.0001, initial = mean(eq$startVal[, 10], na.rm = TRUE)),
           TH = slider(0, 100, step = 0.0001, initial = mean(eq$startVal[, 11], na.rm = TRUE)),
           aa = slider(0, 10, step = 0.0001, initial = mean(eq$startVal[, 12], na.rm = TRUE))
         )
       },
       "eq240" = {},
       "eq250" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, defYlim),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
             points(x = xx, y = cc * (1 - exp(-k1 * (xx - T1))) * (1 - exp(k2 * (xx - T2))),
                    type = 'l', lwd = 3)
           },
           defYlim = slider(0, 10, step = 0.0001, initial = 1),
           cc = slider(0, 1, step = 0.0001, initial = 0.5),
           k1 = slider(0, 10, step = 0.0001, initial = 1),
           k2 = slider(0, 10, step = 0.0001, initial = 1),
           T1 = slider(0, 20, step = 0.0001, initial = 15),
           T2 = slider(0, 100, step = 0.0001, initial = 35)
         )
       },
       "eq260" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, defYlim),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
             points(x = xx, y = phi * (exp(bb * (xx - Tb)) - ((Tm - xx)/(Tm - Tb)) * exp(-bb * (xx - Tb) / deltab) - ((xx - Tb)/(Tm - Tb)) * exp(bb * (Tm - Tb) - (Tm - xx)/deltam)),
                    type = 'l', lwd = 3)
           },
           defYlim = slider(0, 10, step = 0.0001, initial = 1),
           phi = slider(0, 1, step = 0.0001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           bb = slider(0, 1, step = 0.0001, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           Tb = slider(0, 100, step = 0.0001, initial = mean(eq$startVal[, 9], na.rm = TRUE)),
           Tm = slider(0, 100, step = 0.0001, initial = mean(eq$startVal[, 10], na.rm = TRUE)),
           deltab = slider(0, 10, step = 0.0001, initial = mean(eq$startVal[, 11], na.rm = TRUE)),
           deltam = slider(0, 10, step = 0.0001, initial = mean(eq$startVal[, 12], na.rm = TRUE))
         )
       },
       "eq270" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, defYlim),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
             points(x = xx, y = (sqrt(cc) * k1 * (xx - T1) * (1 - exp(k2 * (xx - T2))))^2,
                    type = 'l', lwd = 3)
           },
           defYlim = slider(0, 10, step = 0.0001, initial = 1),
           cc = slider(0, 1, step = 0.0001, initial = 0.5),
           k1 = slider(0, 1, step = 0.0001, initial = 0.5),
           T1 = slider(0, 100, step = 0.0001, initial = 10),
           k2 = slider(0, 1, step = 0.0001, initial = 0.5),
           T2 = slider(0, 100, step = 0.0001, initial = 30)
         )
       },
       "eq280" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, defYlim),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
             points(x = xx, y = (2 * (xx - Tmin)^aa * (Topt - Tmin)^aa - (xx - Tmin)^(2 * aa)) / ((Topt - Tmin)^(2 * aa)),
                    type = 'l', lwd = 3)
           },
           defYlim = slider(0, 10, step = 0.0001, initial = 1),
           Tmin = slider(0, 20, step = 0.0001, initial = 10),
           aa = slider(0, 5, step = 0.0001, initial = 0.5),
           Topt = slider(0, 100, step = 0.0001, initial = 30)
         )
       },
       "eq290" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, defYlim),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
             points(x = xx, y = (bb * (xx - Tb))^2,
                    type = 'l', lwd = 3)
           },
           defYlim = slider(0, 10, step = 0.0001, initial = 1),
           bb = slider(-1, 1, step = 0.0001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           Tb = slider(-10, 20, step = 0.0001, initial = mean(eq$startVal[, 8], na.rm = TRUE))
         )
       },
       "eq300" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, defYlim),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
             points(x = xx, y = cc * (xx - T1) * (1 - exp(k * (xx - T2))),
                    type = 'l', lwd = 3)
           },
           defYlim = slider(0, 10, step = 0.0001, initial = 1),
           cc = slider(0, 1, step = 0.0001, initial = 0.5),
           T1 = slider(-10, 20, step = 0.0001, initial = 5),
           k = slider(0, 1, step = 0.0001, initial = 0.5),
           T2 = slider(-10, 100, step = 0.0001, initial = 20)
         )
       },
       "eq310" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, defYlim),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
             points(x = xx, y = exp(mu) * (xx - Tb)^aa * (Tc - xx)^bb,
                    type = 'l', lwd = 3)
           },
           defYlim = slider(0, 10, step = 0.0001, initial = 1),
           mu = slider(-1, 1, step = 0.0001, initial = 0.5),
           Tb = slider(-10, 20, step = 0.0001, initial = 10),
           aa = slider(-1, 2, step = 0.0001, initial = 0.5),
           Tc = slider(-10, 100, step = 0.0001, initial = 35),
           bb = slider(-1, 1, step = 0.0001, initial = 0.5)
         )
       },
       "eq320" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, defYlim),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
             points(x = xx, y = aa + bb * xx + cc * exp(xx) + dd * exp(-xx),
                    type = 'l', lwd = 3)
           },
           defYlim = slider(0, 10, step = 0.0001, initial = 1),
           aa = slider(-1, 1, step = 0.0001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           bb = slider(-1, 1, step = 0.0001, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           cc = slider(-1, 1, step = 0.0000001, initial = mean(eq$startVal[, 9], na.rm = TRUE)),
           dd = slider(-500000, 0, step = 100, initial = mean(eq$startVal[, 10], na.rm = TRUE))
         )
       },
       "eq330" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, defYlim),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
             points(x = xx, y = 1/( (1 + exp((cc/1.987) * ((1/dd) - (1/(xx + 273.16))) )) / (aa*(xx + 273.16)/298.15*exp( (bb/1.987)*((1/298.15) - 1/(xx + 273.16)) ) ) ),
                    type = 'l', lwd = 3)
           },
           defYlim = slider(0, 10, step = 0.0001, initial = 1),
           aa = slider(0, 1, step = 0.0001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           bb = slider(0, 50000, step = 10, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           cc = slider(0, 500000, step = 10, initial = mean(eq$startVal[, 9], na.rm = TRUE)),
           dd = slider(0, 1000, step = 1, initial = mean(eq$startVal[, 10], na.rm = TRUE))
         )
       },
       "eq340" = {
         manipulate(
           {
             plot(x = temp, y = devRate, xlab = "Temperature", ylab = "Development rate",
                  ylim = c(0, defYlim),
                  lwd = 2, pch = 3)
             xx <- seq(from = min(temp, na.rm = TRUE) - 2, to = max(temp, na.rm = TRUE) + 2, by = 0.1)
             points(x = xx, y = aa * (xx - Tmin) - (bb * exp(xx - Tm)),
                    type = 'l', lwd = 3)
           },
           defYlim = slider(0, 10, step = 0.0001, initial = 1),
           aa = slider(0, 0.6, step = 0.0001, initial = mean(eq$startVal[, 7], na.rm = TRUE)),
           bb = slider(0, 20, step = 0.0001, initial = mean(eq$startVal[, 8], na.rm = TRUE)),
           Tm = slider(0, 100, step = 0.0001, initial = mean(eq$startVal[, 9], na.rm = TRUE)),
           Tmin = slider(-10, 20, step = 0.0001, initial = mean(eq$startVal[, 10], na.rm = TRUE))
         )
       },
       {
         # otherwise nothing
       }
  )

}





