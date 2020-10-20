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
#'   threshold for estimating XTmin and XTmax for asymptotic equations
#'   (default value is 0.01)
#' @param interval A vector containing the lower and upper boundaries of the
#'   interval of temperatures in which metrics are searched.
#' @param ... Additional arguments for the \code{devRateModel} function.
#' @return An object of class \code{list} with two elements. The first
#' element is a \code{list} with all the nls objects resulting from the fitting
#' of the models. The second element is a \code{data.frame}.
#' The first column corresponds to models' names and the second column corresponds
#' to the number of parameters of the models. The columns 3 and 4 correspond
#' to the results of the function \code{devRateQlStat}. The columns 5 to 9
#' correspond to the results of the function \code{devRateQlBio}. The column 10
#' shows the AIC of each model, the column 11 shows BIC of each model.
#' @details
#' Equations stinner_74 and lamb_92 are fitted and the resulting nls objects
#' are showed in the first element of the returned list, however indices of goodness
#' of fit are not provided as these equations return a list of two nls objects.
#' Equation campbell_74 is not fitted.
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
        return(c(stats::AIC(modL[[i]]),
                 stats::BIC(modL[[i]])))
      }else{
        return(c(NA, NA))
      }
    }
  })
  IC <- do.call(rbind, IC)

  ICdf <- data.frame(
    AIC = IC[, 1],
    BIC = IC[, 2])

  qlStat <- devRateQlStat(
    eq = eqList,
    nlsDR = modL,
    dfDataList = list(dfData))

  qlBio <- devRateQlBio(
    nlsDR = modL,
    eq = eqList,
    propThresh = propThresh,
    interval = interval)

  nParam <- lapply(seq_along(modL), function(i){
    if(length(stats::coef(modL[[i]])) == 0){
      return(NA)
    }else{
      return(length(stats::coef(modL[[i]])))
    }
  })

  nParam <- unlist(nParam)

  ql <- data.frame(
    eqName = names(eqList),
    nParam = nParam,
    qlStat,
    qlBio,
    ICdf)

  rownames(ql) <- NULL
  return(list(nlsList = modL, gofTable = ql))
}
