#' Statistical characterization of the nls goodness-of-fit
#'
#' Return a table of multiple statistical indexes of goodness of fit
#'
#' @param nlsDR A list of nls objects.
#' @param eq A list of equations used for nls fitting.
#' @param dfDataList A list of data.frame with the temperature in the first column and
#'   the development rate in the second column.
#' @return An object of class \code{data.frame} with statistical indexes
#'   in columns and nls objects in rows.
#' @details NULL is returned when nlsDR or dfDataList are not of type list.
#' @examples
#' myDf <- data.frame(
#'   temp = seq(from = 0, to = 50, by = 10),
#'   rT = c(0.001, 0.008, 0.02, 0.03, 0.018, 0.004))
#' myNLS <- list(
#'   devRateModel(
#'     eq = damos_08,
#'     dfData = myDf,
#'     startValues = list(aa = 1, bb = 1, cc = 1),
#'     algo = "LM"),
#'   devRateModel(
#'     eq = kontodimas_04,
#'     dfData = myDf,
#'     startValues = list(aa = 1, Tmin = 7, Tmax = 40),
#'     algo = "LM"),
#'   devRateModel(
#'     eq = poly2,
#'     dfData = myDf,
#'     startValues = list(a0 = 1, a1 = 1, a2 = 1),
#'     algo = "LM"))
#' devRateQlStat(
#'   eq = list(damos_08, kontodimas_04, poly2),
#'   nlsDR = myNLS,
#'   dfDataList = list(myDf))
#' @export
devRateQlStat <- function(eq, nlsDR, dfDataList){
  if(class(nlsDR) == "list" &
     class(dfDataList) == "list" &
     length(eq) == length(nlsDR)){
    if(length(dfDataList) < length(nlsDR)){
      dfDataList <- rep(dfDataList, length(nlsDR))
    }
    temp <- lapply(seq_along(dfDataList), function(i){
      return(dfDataList[[i]][, 1])
    })
    devRate <- lapply(seq_along(dfDataList), function(i){
      return(dfDataList[[i]][, 2])
    })
    stats <- lapply(seq_along(nlsDR), function(i){
      # stinner_74 and lamb_92 exception
      if(eq[[i]]$id == "eq040" | eq[[i]]$id == "eq150"){
        # warning("stinner_74 and lamb_92 not implemented")
        dfStats <- data.frame(RSS = NA, RMSE = NA)
        return(dfStats)
      }else{
        if(!is.null(nlsDR[[i]])){
          N <- length(temp[[i]])
          res <- stats::residuals(nlsDR[[i]])
          RSS <- sum(res^2)
          RMSE <- sqrt(RSS / N)
          dfStats <- data.frame(RSS = RSS, RMSE = RMSE)
          return(dfStats)
        }else{
          dfStats <- data.frame(RSS = NA, RMSE = NA)
          return(dfStats)
        }
      }
    })
    stats <- do.call(rbind, stats)
    row.names(stats) <- lapply(seq_along(nlsDR), function(i){paste0("nls#", i)})
    return(stats)
  }else{
    # warning("nlsDR or dfDataList is not a list")
    return(NULL)
  }
}

#' Biological likelihood of nls fits
#'
#' Return a table of 5 metrics of development (CTmin, CTmax, Topt, XTmin, XTmax)
#'
#' @param nlsDR A list of nls objects.
#' @param propThresh The proportion of maximal development rate used as a
#'   threshold for estimating XTmin and XTmax for asymptotic equations
#'   (default value is 0.01)
#' @param eq A list of equations used for nls fitting.
#' @param interval A vector containing the lower and upper boundaries of the
#' interval of temperatures in which metrics are searched.
#' @return An object of class \code{data.frame} with development metrics (CTmin,
#'   Ctmax, Topt, XTmin, XTmax) in columns and nls objects in rows.
#' @details NULL is returned when nlsDR or eq are not a list.
#' @examples
#' myDf <- data.frame(temp = seq(from = 0, to = 50, by = 10),
#'  rT = c(0.001, 0.008, 0.02, 0.03, 0.018, 0.004))
#' myNLS <- list(
#'  devRateModel(
#'    eq = janisch_32,
#'    df = myDf,
#'    startValues = list(aa = 0.2, bb = 0.1, Dmin = 10, Topt = 30),
#'    algo = "LM"),
#'  devRateModel(
#'    eq = kontodimas_04,
#'    df = myDf,
#'    startValues = list(aa = 1, Tmin = 7, Tmax = 40),
#'    algo = "LM"),
#'  devRateModel(
#'    eq = poly2,
#'    df = myDf,
#'    startValues = list(a0 = 1, a1 = 1, a2 = 1),
#'    algo = "LM"))
#' devRateQlBio(
#'   nlsDR = myNLS,
#'   eq = list(janisch_32, kontodimas_04, poly2),
#'   propThresh = 0.1)
#' @export
devRateQlBio <- function(nlsDR, propThresh = 0.01, eq, interval = c(0, 50)){
  stats <- lapply(seq_along(nlsDR), function(i){
    # stinner_74 and lamb_92 exception
    if(eq[[i]]$id == "eq040" | eq[[i]]$id == "eq150"){
      # warning("stinner_74 and lamb_92 not implemented")
      dfStats <- data.frame(CTmin = NA, CTmax = NA, Topt = NA, XTmin = NA, XTmax = NA)
      return(dfStats)
    }else{
      if(!is.null(nlsDR[[i]])){
        if(eq[[i]]$id == "eq030"){
          a <- unname(stats::coef(nlsDR[[i]])[1])
          b <- unname(stats::coef(nlsDR[[i]])[2])
          CTmin <- -(a/b)
          return(data.frame(CTmin = CTmin, CTmax = NA, Topt = NA, XTmin = NA, XTmax = NA))
        }
        if(eq[[i]]$id == "eq020" | eq[[i]]$id == "eq290"){
          temp <- seq(from = interval[1], to = interval[2], by = 0.1)
          rT <- stats::predict(nlsDR[[i]], newdata = list(T = temp))
          rT[is.na(rT)] <- 0
          rT[rT < 0] <- 0
          CTmin <- max(temp[rT == min(rT)])
          rT[rT < propThresh*max(rT)] <- 0
          XTmin <- max(temp[rT == min(rT)])
          return(data.frame(CTmin = CTmin, CTmax = NA, Topt = NA, XTmin = XTmin, XTmax = NA))
        }
        # Topt <- stats::optimize(
        #   f = function(temp){
        #     x <- stats::predict(nlsDR[[i]], newdata = list(T = temp))
        #     x[is.na(x)] <- 0
        #     return(x)
        #   },
        #   maximum = TRUE, interval = interval)$maximum
        getTopt <- function(interval){
          Topt <- stats::optimize(
            f = function(temp){
              x <- stats::predict(nlsDR[[i]], newdata = list(T = temp))
              x[is.na(x)] <- 0
              return(x)
            },
            maximum = TRUE,
            interval = interval,
            lower = min(get("T", nlsDR[[i]]$m$getEnv())),
            upper = max(get("T", nlsDR[[i]]$m$getEnv())))$maximum
          return(Topt)
        }
        Topt <- getTopt(interval=interval)

        temp <- seq(from = interval[1], to = interval[2], by = 0.1)
        rT <- stats::predict(nlsDR[[i]], newdata = list(T = temp))
        rT[is.na(rT)] <- 0
        rT[rT < 0] <- 0
        CTmaxs <- temp[rT == min(rT) & temp > Topt]
        if(length(CTmaxs) > 0){
          CTmax <- min(CTmaxs)
        }else{
          CTmax <- NA
        }
        CTmins <- temp[rT == min(rT) & temp < Topt]
        if(length(CTmins) > 0){
          CTmin <- max(CTmins)
        }else{
          CTmin <- NA
        }
        rT[rT < propThresh*rT[round(x = temp, digits = 1) == round(x = Topt, digits = 1)]] <- 0
        XTmaxs <- temp[rT == min(rT) & temp > Topt]
        if(length(CTmaxs) > 0){
          XTmax <- min(XTmaxs)
        }else{
          XTmax <- NA
        }
        XTmins <- temp[rT == min(rT) & temp < Topt]
        if(length(XTmins) > 0){
          XTmin <- max(XTmins)
        }else{
          XTmin <- NA
        }
        return(data.frame(CTmin = CTmin, CTmax = CTmax, Topt = Topt, XTmin = XTmin, XTmax = XTmax))
      }else{
        return(data.frame(CTmin = NA, CTmax = NA, Topt = NA, XTmin = NA, XTmax = NA))
      }
    }
  })
  stats <- do.call(rbind, stats)
  row.names(stats) <- lapply(seq_along(nlsDR), function(i){
    paste0("nls#", i)
  })
  return(stats)
}
