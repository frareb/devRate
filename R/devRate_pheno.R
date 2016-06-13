#' Forecast ectotherm phenology as a function of temperature and developmental rate models
#' @export
devRateIBM <- function(tempTS, timeStepTS, models, numInd = 100, stocha, timeLayEggs = 1){
  for(ind in 1:numInd){
    g <- 0
    tx <- 0
    while(tx < length(tempTS)){
      g <- g + 1
      for(i in seq_along(models)){
        currentDev <- 0
        while(currentDev < 1){
          tx <- tx + 1
          if(tx > length(tempTS)){break}
          currentDev <- currentDev + rnorm(n = 1,
            mean = stats::predict(models[[i]], newdata = list(T = tempTS[tx])),
            sd = stocha) * timeStepTS
        }
        assign(paste0("g", g, "s", i), tx)
      }
      tx <- tx + as.integer(timeLayEggs)
    }
    stages <- paste0(rep(paste0("g",1:(g + 10)), each = length(models)),
                     "s", 1:length(models))
    if(exists(x = "communityInd")){
      stages <- c(stages[1:(((g - 1) * length(models) + (i - 1)) - 1)],
        rep(NA, ((ncol(communityInd) - ((g - 1) * length(models) + (i - 1))) + 1)))
    }else{
      stages <- c(stages[1:(((g - 1) * length(models) + (i - 1)) - 1)],
        rep(NA, ((length(stages) - ((g - 1) * length(models) + (i - 1))) + 1)))
    }
    currentInd <- sapply(stages, function(k){
      if(!is.na(k)){return(get(k))}else{return(NA)}
    })
    if(exists(x = "communityInd")){
      communityInd <- rbind(communityInd, currentInd)
    }else{
      communityInd <- matrix(currentInd, ncol = length(currentInd))
    }
  }
  bdd <- communityInd
  ### TO DO: enlever les colonnes avec des NA et renommer les colonnes restantes ###
  bdd <- unname(bdd[, !is.na(apply(bdd, MARGIN = 2, FUN = mean, na.rm = TRUE))])
  if(!is.matrix(bdd)){bdd <- matrix(bdd)}
  if(ncol(bdd) != 0){
    colnames(bdd) <- paste0(rep(paste0("g", 1:ncol(bdd)), each = length(models)),
      "s", 1:length(models))[1:ncol(bdd)]
  }
  rm(communityInd)

  return(list(bdd, models, tempTS))
}

devRateIBMPlot <- function(ibm, typeG = "density", threshold = 0.1){
  if(typeG == "density"){myYlim <- c(0, 0.5)}else{myYlim <- c(0, nrow(ibm[[1]]))}
  plot(0, type = 'n', xlim = c(0, length(ibm[[3]])), ylim = myYlim,
       xlab = "Time steps", ylab = "Phenology density")
  colG <- rep(1:8, each = length(ibm[[2]]))
  colGTrans <- adjustcolor(colG, alpha.f = 0.3)
  ltyG <- rep(1:length(ibm[[2]]), 8)

  switch(EXPR = typeG,
    "density" = {
      cat(paste0("Threshold for visualization = ", threshold * 100, "% of individuals"))
      for(j in 1:ncol(ibm[[1]])){
        if(sum((!is.na(ibm[[1]][,j])) / length(ibm[[1]][,j])) >= threshold){
          points(density(ibm[[1]][,j], na.rm = TRUE, adjust = 2),
                 type ='l', col = colG[j], lty = ltyG[j], lwd = 2)
        }
      }
    },
    "hist" = {
      for(j in 1:ncol(ibm[[1]])){
        hist(ibm[[1]][,j], add = TRUE, col = colGTrans[j], lty = ltyG[j])
      }
    }
  )
  legend("topleft",
         legend = colnames(ibm[[1]]),
         lty = 1:length(ibm[[2]]),
         lwd = 2,
         col = rep(1:ceiling(ncol(ibm[[1]]) / length(ibm[[2]])), each = length(ibm[[2]])),
         ncol = ceiling(ncol(ibm[[1]]) / length(ibm[[2]])))
}

### testing area ###

forecastTsolanivora <- devRateIBM(
  tempTS = rnorm(n = 100, mean = 15, sd = 1),
  timeStepTS = 1,
  models = list(mEggs, mLarva, mPupa),
  numInd = 500,
  stocha = 0.015,
  timeLayEggs = 1)

print(forecastTsolanivora[[1]])

devRateIBMPlot(ibm = forecastTsolanivora, typeG = "density", threshold = 0.1)
devRateIBMPlot(ibm = forecastTsolanivora, typeG = "hist")


numGen <- sum(!is.na(apply(forecastTsolanivora[[1]], MARGIN = 2, FUN = mean, na.rm = TRUE))) / length(forecastTsolanivora[[2]])










