

#' @param tempTS The temperature time series (a vector).
#' @param timeStepTS The time step of the temperature time series (a numeric with 1 = one day).
#' @param eq
#' @param species
#' @param lifeStages
#' @param numInd The number of individuals for the simulation (an integer).
#' @param stocha The standard deviation of a Normal distribution centered on
#'    develomental rate to create stochasticity among individuals (a numeric).
#' @param timeLayEggs The delay between emergence of adults and the time where
#'    females lay eggs in time steps (a numeric).
devRateIBMdataBase <- function(tempTS, timeStepTS, eq, species, lifeStages, numInd = 100, stocha, timeLayEggs = 1){
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



devRateFind(species = "Cydia pomonella")
forecastCpomonella_logan6_76 <- devRateIBMdataBase(
  tempTS = rnorm(n = 60, mean = 25, sd = 1),
  timeStepTS = 1,
  eq = logan6_76,
  species = "Cydia pomonella",
  lifeStages = c("egg", "larva", "pupa"),
  numInd = 100,
  stocha = 0.015,
  timeLayEggs = 1
)
forecastCpomonella_logan10_76 <- devRateIBMdataBase(
  tempTS = rnorm(n = 60, mean = 25, sd = 1),
  timeStepTS = 1,
  eq = logan10_76,
  species = "Cydia pomonella",
  lifeStages = c("egg", "larva", "pupa"),
  numInd = 100,
  stocha = 0.015,
  timeLayEggs = 1
)
forecastCpomonella_analytis_77 <- devRateIBMdataBase(
  tempTS = rnorm(n = 60, mean = 25, sd = 1),
  timeStepTS = 1,
  eq = analytis_77,
  species = "Cydia pomonella",
  lifeStages = c("egg", "larva", "pupa"),
  numInd = 100,
  stocha = 0.015,
  timeLayEggs = 1
)
forecastCpomonella_taylor_81 <- devRateIBMdataBase(
  tempTS = rnorm(n = 60, mean = 25, sd = 1),
  timeStepTS = 1,
  eq = taylor_81,
  species = "Cydia pomonella",
  lifeStages = c("egg", "larva", "pupa"),
  numInd = 100,
  stocha = 0.015,
  timeLayEggs = 1
)
forecastCpomonella_harcourtYee_82 <- devRateIBMdataBase(
  tempTS = rnorm(n = 60, mean = 25, sd = 1),
  timeStepTS = 1,
  eq = harcourtYee_82,
  species = "Cydia pomonella",
  lifeStages = c("egg", "larva", "pupa"),
  numInd = 100,
  stocha = 0.015,
  timeLayEggs = 1
)
forecastCpomonella_hilbertLogan_83 <- devRateIBMdataBase(
  tempTS = rnorm(n = 60, mean = 25, sd = 1),
  timeStepTS = 1,
  eq = hilbertLogan_83,
  species = "Cydia pomonella",
  lifeStages = c("eggs", "larva", "pupa"),
  numInd = 100,
  stocha = 0.015,
  timeLayEggs = 1
)
forecastCpomonella_lactin2_95 <- devRateIBMdataBase(
  tempTS = rnorm(n = 60, mean = 25, sd = 1),
  timeStepTS = 1,
  eq = lactin2_95,
  species = "Cydia pomonella",
  lifeStages = c("egg", "larva", "pupa"),
  numInd = 100,
  stocha = 0.015,
  timeLayEggs = 1
)
forecastCpomonella_kontodimas_04 <- devRateIBMdataBase(
  tempTS = rnorm(n = 60, mean = 25, sd = 1),
  timeStepTS = 1,
  eq = kontodimas_04,
  species = "Cydia pomonella",
  lifeStages = c("egg", "larva", "pupa"),
  numInd = 100,
  stocha = 0.015,
  timeLayEggs = 1
)
forecastCpomonella_Campbell <- devRateIBMdataBase(
  tempTS = rnorm(n = 60, mean = 25, sd = 1),
  timeStepTS = 1,
  eq = campbell_74,
  species = "Cydia pomonella",
  lifeStages = c("eggs", "larva", "pupa"),
  numInd = 100,
  stocha = 0.015,
  timeLayEggs = 1
)
forecastCpomonella_Davidson <- devRateIBMdataBase(
  tempTS = rnorm(n = 60, mean = 25, sd = 1),
  timeStepTS = 1,
  eq = davidson_44,
  species = "Cydia pomonella",
  lifeStages = c("eggs", "larva", "pupa"),
  numInd = 100,
  stocha = 0.015,
  timeLayEggs = 1
)
forecastCpomonella_Briere1 <- devRateIBMdataBase(
  tempTS = rnorm(n = 60, mean = 25, sd = 1),
  timeStepTS = 1,
  eq = briere1_99,
  species = "Cydia pomonella",
  lifeStages = c("egg", "larva", "pupa"),
  numInd = 100,
  stocha = 0.015,
  timeLayEggs = 1
)
devRateIBMPlot(ibm = forecastCydiaPomonellaBriere1, typeG = "hist")
forecastCpomonella_Briere2 <- devRateIBMdataBase(
  tempTS = rnorm(n = 60, mean = 25, sd = 1),
  timeStepTS = 1,
  eq = briere2_99,
  species = "Cydia pomonella",
  lifeStages = c("egg", "larva", "pupa"),
  numInd = 100,
  stocha = 0.015,
  timeLayEggs = 1
)
devRateIBMPlot(ibm = forecastCpomonella_kontodimas_04, typeG = "hist")
devRateIBMPlot(ibm = forecastCpomonella_logan6_76, typeG = "hist")
devRateIBMPlot(ibm = forecastCpomonella_Campbell, typeG = "hist")
devRateIBMPlot(ibm = forecastCpomonella_Davidson, typeG = "hist")
devRateIBMPlot(ibm = forecastCpomonella_logan10_76, typeG = "hist")
devRateIBMPlot(ibm = forecastCpomonella_analytis_77, typeG = "hist")
devRateIBMPlot(ibm = forecastCpomonella_taylor_81, typeG = "hist")
devRateIBMPlot(ibm = forecastCpomonella_harcourtYee_82, typeG = "hist")
# devRateIBMPlot(ibm = forecastCpomonella_hilbertLogan_83, typeG = "hist")
devRateIBMPlot(ibm = forecastCpomonella_lactin2_95, typeG = "hist")
devRateIBMPlot(ibm = forecastCpomonella_Briere1, typeG = "hist")
devRateIBMPlot(ibm = forecastCpomonella_Briere2, typeG = "hist")
