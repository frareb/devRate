#' Fitting all models listed in devRateEqList to a development rate dataset
#'
#' This function fits all models listed in devRateEqList to a development
#' rate dataset and then calculates a series of indices of goodness of fit for
#' each fitted model.
#' @param dfData A data.frame with the temperature in the first column and the
#'   development rate in the second column.
#' @param eqList A list of models that can be retrieved from the object devRateEqList.
#' The default value is the object devRateEqList.
#' @param eqStartVal A list of sarting values for each model. The default value is
#' the object devRateEqStartVal.
#' @param propThresh The proportion of maximal development rate used as a
#'   threshold for estimating CTmin and CTmax when asymptotic equations are used
#'   (default value is 0.01)
#' @param interval A vector containing the lower and upper boundaries of the
#'   interval of temperatures in which metrics are searched.
#' @param ... Additional arguments for the \code{devRateModel} function.
#' @return An object of class \code{list} with two elements. The first
#' element is a \code{list} with all the nls objects resulting from the fitting
#' of the models. The second element is a \code{data.frame}.
#' The first column corresponds to models' names. The columns 2 to 4 corresponds
#' to the results of the function \code{devRateQlStat}. The columns 5 to 7
#' corresponds to the results of the function \code{devRateQlBio}. The column 8
#' shows the AIC of each model, the column 9 shows the rank of each model
#' according to its AIC, the column 10 shows the deltaAIC (the difference
#' between the AIC of the ith model and the minimal AIC). The rest of the columns
#' corresponds to the same but with BIC instead of AIC.
#' @details
#' Equations stinner_74 and lamb_92 are fitted and the resulting nls objects
#' are showed in the first element of the returned list, however indices of goodness
#' of fit are not provided as these equations return a list of two nls objects.
#' Equation campbell_74 is not fitted as it is a linear model.
#' @examples
#' myDf <- exTropicalMoth$raw$egg
#' devRateModelAll(dfData = myDf)
#' @export

devRateModelAll <- function(
  dfData,
  eqList = devRate::devRateEqList,
  eqStartVal = devRate::devRateEqStartVal,
  propThresh = 0.01,
  interval = c(0, 50),
  ...){
  modL <- lapply(seq_along(eqList), function(i){
    if(eqList[[i]]$id == "eq270"){
      modX <- try(
        devRateModel(
          dfData = dfData,
          eq = eqList[[i]],
          startValues = eqStartVal[[i]],
          lower = c(0, -Inf, -Inf, -Inf, -Inf),
          ...),
        silent = TRUE)
    }else{
      if(eqList[[i]]$id == "eq030"){
        return(NULL)
      }else{
        modX <- try(
          devRateModel(
            dfData = dfData,
            eq = eqList[[i]],
            startValues = eqStartVal[[i]],
            ...),
          silent = TRUE)
      }
    }
    if(class(modX) == "try-error"){
      return(NULL)
    }else{
      return(modX)
    }
  })
  names(modL) <- names(eqList)

  IC <- lapply(seq_along(modL), function(i){
    if(eqList[[i]]$id == "eq040" | eqList[[i]]$id == "eq150"){
      return(c(NA, NA))
    }else{
      if(!is.null(modL[[i]])){
        return(c(stats::AIC(modL[[i]]), stats::BIC(modL[[i]])))
      }else{
        return(c(NA, NA))
      }
    }
  })
  IC <- do.call(rbind, IC)
  rankAIC <- rank(IC[, 1])
  rankBIC <- rank(IC[, 2])
  deltaAIC <- vector()
  deltaBIC <- vector()
  for(i in 1:length(modL)){
    deltaAIC[i] <- IC[, 1][i] - min(IC[, 1], na.rm = TRUE)
    deltaBIC[i] <- IC[, 2][i] - min(IC[, 2], na.rm = TRUE)
  }

  ICdf <- data.frame(
    AIC = IC[, 1],
    rankAIC,
    deltaAIC,
    BIC = IC[, 2],
    rankBIC,
    deltaBIC)

  qlStat <- devRateQlStat(
    eq = eqList,
    nlsDR = modL,
    dfDataList = list(dfData))

  qlBio <- devRateQlBio(
    nlsDR = modL,
    eq = eqList,
    propThresh = propThresh,
    interval = interval)

  ql <- data.frame(
    eqName = names(eqList),
    qlStat,
    qlBio,
    ICdf)

  rownames(ql) <- NULL
  return(list(nlsList = modL, gofTable = ql))
}
