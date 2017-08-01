#' Forecast ectotherm phenology as a function of temperature and development rate models
#'
#' @param tempTS The temperature time series (a vector).
#' @param timeStepTS The time step of the temperature time series (a numeric in days).
#' @param models The models for development rate (a list with objects of class nls).
#' @param numInd The number of individuals for the simulation (an integer).
#' @param stocha The standard deviation of a Normal distribution centered on
#'    develoment rate to create stochasticity among individuals (a numeric).
#' @param timeLayEggs The delay between emergence of adults and the time where
#'    females lay eggs in time steps (a numeric).
#' @return A list with three elements: the table of phenology for each individual,
#'    the models used (nls objects), and the time series for temperature.
#' @examples
#' data(exTropicalMoth)
#' forecastTsolanivora <- devRateIBM(
#'    tempTS = rnorm(n = 100, mean = 15, sd = 1),
#'    timeStepTS = 1,
#'    models = exTropicalMoth[[2]],
#'    numInd = 100,
#'    stocha = 0.015,
#'    timeLayEggs = 1)
#' @export
devRateIBM <- function(tempTS, timeStepTS, models, numInd = 100, stocha, timeLayEggs = 1){
  for(ind in 1:numInd){
    g <- 0
    tx <- 0
    ratioSupDev <- 0
    vectorGS <- vector()
    while(tx < length(tempTS)){
      g <- g + 1
      for(i in seq_along(models)){
        if(ratioSupDev > 0){
          add2Dev <- stats::rnorm(n = 1, mean = stats::predict(models[[i]],
              newdata = list(T = tempTS[tx])), sd = stocha) * ratioSupDev * timeStepTS
          if(add2Dev < 0){add2Dev <- 0}
          currentDev <- add2Dev
        } else {
          currentDev <- 0
        }
        while(currentDev < 1){
          tx <- tx + 1
          if(tx > length(tempTS)){break}
          addDev <- stats::rnorm(n = 1, mean = stats::predict(models[[i]],
            newdata = list(T = tempTS[tx])), sd = stocha) * timeStepTS
          if(addDev < 0){addDev <- 0}
          currentDev <- currentDev + addDev
        }
        ratioSupDev <- 0
        if(currentDev >= 1){
          # print(currentDev)
          supDev <- 0
          if(currentDev > 1){
            supDev <- currentDev - 1
            ratioSupDev <- supDev / addDev
          }
          vectorGS <- c(vectorGS, tx)
        }
      }
      tx <- tx + as.integer(timeLayEggs)
    }
    currentInd <-  vectorGS
    if(exists(x = "communityInd")){
      communityInd <- rbind(communityInd, c(currentInd, rep(NA, ncol(communityInd) - length(currentInd))))
    }else{
      communityInd <- matrix(c(currentInd, rep(NA, 10)), ncol = length(currentInd) + 10) ### assuming 10G dif.
    }
  }
  bdd <- communityInd
  bdd <- unname(bdd[, !is.na(apply(bdd, MARGIN = 2, FUN = mean, na.rm = TRUE))])
  if(!is.matrix(bdd)){bdd <- matrix(bdd)}
  if(ncol(bdd) != 0){
    colnames(bdd) <- paste0(rep(paste0("g", 1:ncol(bdd)), each = length(models)),
                            "s", 1:length(models))[1:ncol(bdd)]
  }
  rm(communityInd)
  return(list(bdd, models, tempTS))
}

#' Plot phenology table
#'
#' @param ibm The phenology model returned by devRateIBM function.
#' @param typeG The type of plot ("density" or "hist").
#' @param threshold The threshold rate of individuals for being represented
#'    in a density plot (a numeric between 0 and 1).
#' @return Nothing.
#' @examples
#' data(exTropicalMoth)
#' forecastTsolanivora <- devRateIBM(
#'    tempTS = rnorm(n = 100, mean = 15, sd = 1),
#'    timeStepTS = 1,
#'    models = exTropicalMoth[[2]],
#'    numInd = 500,
#'    stocha = 0.015,
#'    timeLayEggs = 1)
#' devRateIBMPlot(ibm = forecastTsolanivora, typeG = "density", threshold = 0.1)
#' devRateIBMPlot(ibm = forecastTsolanivora, typeG = "hist")
#' @export
devRateIBMPlot <- function(ibm, typeG = "density", threshold = 0.1){
  if(typeG == "density"){myYlim <- c(0, 0.5)}else{myYlim <- c(0, nrow(ibm[[1]]))}
  graphics::plot(0, type = 'n', xlim = c(0, length(ibm[[3]])), ylim = myYlim,
       xlab = "Time steps", ylab = "Phenology density")
  colG <- rep(1:30, each = length(ibm[[2]]))
  colGTrans <- grDevices::adjustcolor(colG, alpha.f = 0.3)
  ltyG <- rep(1:length(ibm[[2]]), 30)

  switch(EXPR = typeG,
    "density" = {
      cat(paste0("Threshold for visualization = ", threshold * 100, "% of individuals"))
      for(j in 1:ncol(ibm[[1]])){
        if(sum((!is.na(ibm[[1]][,j])) / length(ibm[[1]][,j])) >= threshold){
          graphics::points(stats::density(ibm[[1]][,j], na.rm = TRUE, adjust = 2),
                 type ='l', col = colG[j], lty = ltyG[j], lwd = 2)
        }
      }
    },
    "hist" = {
      for(j in 1:ncol(ibm[[1]])){
        graphics::hist(ibm[[1]][,j], add = TRUE, col = colGTrans[j], lty = ltyG[j])
      }
    }
  )
  graphics::legend("topleft",
         legend = colnames(ibm[[1]]),
         lty = 1:length(ibm[[2]]),
         lwd = 2,
         col = rep(1:ceiling(ncol(ibm[[1]]) / length(ibm[[2]])), each = length(ibm[[2]])),
         ncol = ceiling(ncol(ibm[[1]]) / length(ibm[[2]])))
}

#' Number of generations
#'
#' Computes the number of generations from the individual-based model fit.
#' @param ibm The phenology model returned by devRateIBM function.
#' @return The simulated number of generations.
#' @examples
#' rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
#'    13, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
#'    25, 0.200, 30, 0.200, 30, 0.180, 35, 0.001), ncol = 2, byrow = TRUE)
#' rawDevLarva <- matrix(c(10, 0.010, 10, 0.014, 10, 0.019, 13, 0.034, 15, 0.024,
#'    15.5, 0.029, 15.5, 0.034, 15.5, 0.039, 17, 0.067, 20, 0.050, 25, 0.076,
#'    25, 0.056, 30, 0.0003, 35, 0.0002), ncol = 2, byrow = TRUE)
#' rawDevPupa <- matrix(c(10, 0.001, 10, 0.008, 10, 0.012, 13, 0.044, 15, 0.017,
#'    15, 0.044, 15.5, 0.039, 16, 0.034, 15.5, 0.037, 16, 0.051, 17, 0.051,
#'    20, 0.080, 20, 0.092, 25, 0.102, 25, 0.073, 30, 0.005,
#'    35, 0.0002), ncol = 2, byrow = TRUE)
#' mEggs <- devRateModel(eq = taylor_81, temp = rawDevEggs[,1], devRate = rawDevEggs[,2],
#'    startValues = list(Rm = 0.05, Tm = 30, To = 5))
#' mLarva <- devRateModel(eq = taylor_81, temp = rawDevLarva[,1], devRate = rawDevLarva[,2],
#'    startValues = list(Rm = 0.05, Tm = 25, To = 5))
#' mPupa <- devRateModel(eq = taylor_81, temp = rawDevPupa[,1], devRate = rawDevPupa[,2],
#'    startValues = list(Rm = 0.05, Tm = 30, To = 5))
#' forecastTsolanivora <- devRateIBM(
#'    tempTS = rnorm(n = 100, mean = 15, sd = 1),
#'    timeStepTS = 1,
#'    models = list(mEggs, mLarva, mPupa),
#'    numInd = 500,
#'    stocha = 0.015,
#'    timeLayEggs = 1)
#' devRateIBMgen(ibm = forecastTsolanivora)
#' @export
devRateIBMgen <- function(ibm){
  numGen <- sum(!is.na(apply(ibm[[1]], MARGIN = 2, FUN = mean, na.rm = TRUE))) / length(ibm[[2]])
  return(numGen)
}
