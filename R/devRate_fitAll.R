#' Fitting all models listed in devRateEqList to a development rate dataset
#'
#' This function fits all models listed in devRateEqList to a development
#' rate dataset and then performs ...
#' @param df A data.frame with the temperature in the first column and the
#'   development rate in the second column.
#' @param eqList A list of models that can be retrieved from the object devRateEqList.
#' The default value is the object devRateEqList.
#' @param eqStartVal A list of sarting values for each model. The default value is
#' the object devRateEqStartVal.
#' @param ... Additional arguments for the \code{devRateModel} function and
#' the \code{devRateQlBio} function.
#' @return  An object of class \code{list} with two elements. The first
#' element is a \code{list} with all the nls objects resulting from the fitting
#' of the models. The second element is a \code{data.frame} with xxx columns.
#' The first column corresponds to models' names. The second column shows the AIC
#' of each model, the third shows the rank of each model according to its
#' AIC, the fourth shows the deltaAIC (the difference between the AIC of the
#' ith model and the minimal AIC). The columns 5 to 7 corresponds to the same
#' but with BIC instead of AIC. The rest of the columns corresponds to the
#' results of the function \code{devRateQlStat} and \code{devRateQlBio}.
#' @details
#' Equations stinner_74 and lamb_92 are fitted and the resulting nls objects
#' are showed in the first element of the returned list, however indices of quality
#' of fits are not yet provided as these equations result in two nls objects.
#' @examples
#' myDf <- exTropicalMoth$raw$egg
#' devRateModelAll(df = myDf)
#' @export

devRateModelAll <- function(df,
                            eqList = devRateEqList,
                            eqStartVal = devRateEqStartVal,
                            ...){
  modL <- lapply(seq_along(eqList), function(i){
    modX <- try(devRateModel(df = df,
                             eq = eqList[[i]],
                             startValues = eqStartVal[[i]],
                             ...),
                silent = TRUE)
    if(class(modX) == "try-error"){
      return(NULL)
    }else{
      return(modX)
    }
  })
  IC <- lapply(seq_along(modL), function(i){
    if(eqList[[i]]$id == "eq040" | eqList[[i]]$id == "eq150"){
      return(c(NA, NA))
    }else{
      if(!is.null(modL[[i]])){
        return(c(AIC(modL[[i]]), BIC(modL[[i]])))
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
  ICdf <- data.frame(AIC = IC[, 1], rankAIC, deltaAIC,
                     BIC = IC[, 2], rankBIC, deltaBIC)
  qlStat <- devRateQlStat(eq = devRateEqList, nlsDR = modL, df = list(df))
  qlBio <- devRateQlBio(nlsDR = modL, eq = devRateEqList, ...)
  ql <- data.frame(eqName = names(devRateEqList), ICdf, qlStat, qlBio)
  rownames(ql) <- NULL
  return(list(modL, ql))
}
