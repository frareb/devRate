#' Fitting of every models and indices of quality of fits
#'
#' Return a list composed of two elements: first a list of every fitted models,
#' second a data.frame showing every models used and their associated indices
#' of goodness-of-fit.
#'
#' @param df A data.frame with the temperature in the first column and the
#'   development rate in the second column.
#' @return  An object of class \code{list} with first the list of the nls objects
#' and second a data.frame with indices of fits qualityin columns and model used
#' in rows
#' @details
#' @examples
#' @export

devRateModelAll <- function(df, ...){
  modL <- lapply(seq_along(devRateEqList), function(i){
    modX <- try(devRateModel(
      df = df,
      eq = devRateEqList[[i]],
      startValues = devRateEqStartVal[[i]]), silent = TRUE)
    if(class(modX) == "try-error"){
      return(NULL)
    }else{
      return(modX)
    }
  })

  AIC <- lapply(seq_along(modL), function(i){
    if(i == 4 | i == 23){
      return(NA)
    }else{
      if(!is.null(modL[[i]])){
        return(AIC(modL[[i]]))
      }else{
        return(NA)
      }
    }
  })
  AIC <- do.call(c, AIC)
  rankAIC <- rank(AIC)
  deltaAIC <- vector()
  for(i in 1:37){
    deltaAIC[i] <- AIC[i] - min(AIC, na.rm = TRUE)
  }
  AICdf <- data.frame(AIC, rankAIC, deltaAIC)

  BIC <- lapply(seq_along(modL), function(i){
    if(i == 4 | i == 23){
      return(NA)
    }else{
      if(!is.null(modL[[i]])){
        return(BIC(modL[[i]]))
      }else{
        return(NA)
      }
    }
  })
  BIC <- do.call(c, BIC)
  rankBIC <- rank(BIC)
  deltaBIC <- vector()
  for(i in 1:37){
    deltaBIC[i] <- BIC[i] - min(BIC, na.rm = TRUE)
  }
  BICdf <- data.frame(BIC, rankBIC, deltaBIC)

  qlStat <- devRateQlStat(eq = devRateEqList, nlsDR = modL, df = list(df))

  qlBio <- devRateQlBio(nlsDR = modL, propThresh = 0.01, eq = devRateEqList)

  ql <- data.frame(eqName = names(devRateEqList), AICdf, BICdf, qlStat, qlBio)
  rownames(ql) <- NULL

  return(list(modL, ql))
}
