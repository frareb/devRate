#' Forecast ectotherm phenology as a function of temperature and development rate models
#' available in the package database
#'
#' @param tempTS The temperature time series (a vector).
#' @param timeStepTS The time step of the temperature time series (a numeric with 1 = one day).
#' @param eq The name of the equation (e.g., lactin2_95).
#' @param species The species for the model (e.g., "Sesamia nonagrioides").
#' @param lifeStages The life stages available for the species and the model.
#' @param numInd The number of individuals for the simulation (an integer).
#' @param stocha The standard deviation of a Normal distribution centered on
#'    development rate to create stochasticity among individuals (a numeric).
#' @param timeLayEggs The delay between emergence of adults and the time where
#'    females lay eggs in time steps (a numeric).
#' @return A list with three elements: the table of phenology for each individual,
#'    the models used (nls objects), and the time series for temperature.
#' @examples
#' forecastLactin2_95 <- devRateIBMdataBase(
#'   tempTS = rnorm(n = 20, mean = 20, sd = 1),
#'   timeStepTS = 10,
#'   eq = lactin2_95,
#'   species = "Sesamia nonagrioides",
#'   lifeStages = c("eggs", "larva", "pupa"),
#'   numInd = 10,
#'   stocha = 0.015,
#'   timeLayEggs = 1
#' )
#' @export
devRateIBMdataBase <- function(tempTS, timeStepTS, eq, species, lifeStages, numInd = 10, stocha, timeLayEggs = 1){
  models <- lapply(seq_along(lifeStages), function(z){ # get list of parameters for each model
    eq$startVal[ , grepl( "param." , names( eq$startVal ) ) ][(eq$startVal[ , "genSp"] == species & eq$startVal[ , "stage"] == lifeStages[z]), ]
  })
  modelVar <- sapply(models, function(i){ # get parameters' names for each model
    sapply(strsplit(names(i), split = "\\."), "[[", 2)
  })
  for(ind in 1:numInd){
    g <- 0 # generation
    tx <- 0 # time step number in temperature time series
    ratioSupDev <- 0
    vectorGS <- vector()
    while(tx < length(tempTS)){
      g <- g + 1
      for(i in seq_along(models)){
        if(ratioSupDev > 0){

          for(k in 1:nrow(modelVar)){assign(x = modelVar[k, i], value = models[[i]][k])}
          x <- tempTS[tx]
          devRT <- unlist(eval(parse(text = eq$eqAlt)))
          add2Dev <- stats::rnorm(n = 1, mean = devRT, sd = stocha) * ratioSupDev * timeStepTS

          if(add2Dev < 0){add2Dev <- 0}
          currentDev <- add2Dev
        } else {
          currentDev <- 0
        }
        while(currentDev < 1){
          tx <- tx + 1
          if(tx > length(tempTS)){break}

          for(k in 1:nrow(modelVar)){assign(x = modelVar[k, i], value = models[[i]][k])}
          x <- tempTS[tx]
          devRT <- unlist(eval(parse(text = eq$eqAlt)))
          addDev <- stats::rnorm(n = 1, mean = devRT, sd = stocha) * timeStepTS

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



#' Forecast ectotherm phenology as a function of temperature and development
#' rate models using known parameters
#'
#' @param tempTS The temperature time series (a vector).
#' @param timeStepTS The time step of the temperature time series (a numeric
#'  with 1 = one day).
#' @param eq The name of the equation (e.g., lactin2_95).
#' @param myParam The known parameters for the equation (a list of list for
#'   each life stage).
#' @param numInd The number of individuals for the simulation (an integer).
#' @param stocha The standard deviation of a Normal distribution centered on
#'    development rate to create stochasticity among individuals (a numeric).
#' @param timeLayEggs The delay between emergence of adults and the time where
#'    females lay eggs in time steps (a numeric).
#' @return A list with three elements: the table of phenology for each individual,
#'    the models used (nls objects), and the time series for temperature.
#' @details Please note that this function is experimental and only works for
#'   the briere2_99 equation.
#' @examples
#' # with only one life stage
#' forecastX <- devRateIBMparam(
#'   tempTS = rnorm(n = 20, mean = 20, sd = 1),
#'   timeStepTS = 10,
#'   eq = briere2_99,
#'   myParam = list(
#'     list(
#'       aa = 0.0002,
#'       Tmin = 10,
#'       Tmax = 36.1,
#'       bb = 2.84)
#'   ),
#'   numInd = 10,
#'   stocha = 0.015,
#'   timeLayEggs = 1
#' )
#' # with two life stages
#' forecastXX <- devRateIBMparam(
#'   tempTS = rnorm(n = 20, mean = 20, sd = 1),
#'   timeStepTS = 10,
#'   eq = briere2_99,
#'   myParam = list(
#'     lifeStage01 = list(
#'       aa = 0.0002,
#'       Tmin = 10,
#'       Tmax = 36.1,
#'       bb = 2.84),
#'     lifeStage02 = list(
#'       aa = 0.0004,
#'       Tmin = 8,
#'       Tmax = 35,
#'       bb = 2.8)
#'   ),
#'   numInd = 10,
#'   stocha = 0.015,
#'   timeLayEggs = 1
#' )
#' @export
devRateIBMparam <- function(
  tempTS,
  timeStepTS,
  eq,
  myParam,
  numInd = 10,
  stocha,
  timeLayEggs = 1){

  # !!! exceptions for equations with positive values for low temp !!!
  exceptDevRate <- function(temp){
    i <- devRT
    if(eq$name == "Briere-2"){
      Tmin <- Tmin
      if(x <= Tmin){
        i <- 0
      }
    }
    return(i)
  }

  models <- lapply(seq_along(myParam), function(z){
    as.data.frame(myParam[[z]])
  })
  modelVar <- sapply(models, function(i){
    names(i)
  })
  for(ind in 1:numInd){
    g <- 0
    tx <- 0
    ratioSupDev <- 0
    vectorGS <- vector()
    while(tx < length(tempTS)){
      g <- g + 1
      for(i in seq_along(models)){
        if(ratioSupDev > 0){

          for(k in 1:nrow(modelVar)){
            assign(x = modelVar[k, i], value = models[[i]][k])
          }
          x <- tempTS[tx]
          devRT <- unlist(eval(parse(text = eq$eqAlt)))
          # !!! exceptions for equations with positive values for low temp !!!
          devRT <- exceptDevRate(x)
          devRT[is.na(devRT)] <- 0

          add2Dev <- stats::rnorm(n = 1, mean = devRT, sd = devRT*stocha) *
            ratioSupDev * timeStepTS

          if(add2Dev < 0){add2Dev <- 0}
          currentDev <- add2Dev
        } else {
          currentDev <- 0
        }
        while(currentDev < 1){
          tx <- tx + 1
          if(tx > length(tempTS)){break}

          for(k in 1:nrow(modelVar)){
            assign(x = modelVar[k, i], value = models[[i]][k])
          }
          x <- tempTS[tx]
          devRT <- unlist(eval(parse(text = eq$eqAlt)))
          # !!! exceptions for equations with positive values for low temp !!!
          devRT <- exceptDevRate(x)
          devRT[is.na(devRT)] <- 0

          addDev <- stats::rnorm(n = 1, mean = devRT, sd = devRT*stocha) *
            timeStepTS

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
    if(exists(x = "communityInd") && length(currentInd) < ncol(communityInd)){
        communityInd <- rbind(
          communityInd,
          c(currentInd, rep(NA, ncol(communityInd) - length(currentInd))))
    }else{
      communityInd <- matrix(
        c(currentInd, rep(NA, 10)), ncol = length(currentInd) + 10
      ) ### assuming 10G dif.
    }
  }
  bdd <- communityInd
  bdd <- unname(bdd[, !is.na(apply(bdd, MARGIN = 2, FUN = mean, na.rm = TRUE))])
  if(!is.matrix(bdd)){bdd <- matrix(bdd)}
  if(ncol(bdd) != 0){
    colnames(bdd) <- paste0(
      rep(paste0("g", 1:ncol(bdd)), each = length(models)),
      "s", 1:length(models))[1:ncol(bdd)]
  }
  rm(communityInd)
  return(list(bdd, models, tempTS))
}
