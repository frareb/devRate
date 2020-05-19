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
