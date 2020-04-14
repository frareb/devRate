#' Statistical characterization of the quality of nls fits
#'
#' Return a table of multiple statistical indexes of goodness of fit
#'
#' @param nlsDR A list of nls objects.
#' @param eq A list of equations used for nls fitting.
#' @param df A list of data.frame with the temperature in the first column and the
#'   development rate in the second column.
#' @return An object of class \code{data.frame} with statistical indexes
#'   in columns and nls objects in rows.
#' @details NULL is returned when nlsDR or df are not a list.
#' @examples
#' myDf <- data.frame(T = seq(from = 0, to = 50, by = 10),
#'                    rT = c(0.001, 0.008, 0.02, 0.03, 0.018, 0.004))
#' myNLS <- list(devRateModel(eq = damos_08, df = myDf,
#'                            startValues = list(aa = 1, bb = 1, cc = 1), algo = "LM"),
#'               devRateModel(eq = kontodimas_04, df = myDf,
#'                            startValues = list(aa = 1, Tmin = 7, Tmax = 40), algo = "LM"),
#'               devRateModel(eq = poly2, df = myDf,
#'                            startValues = list(a0 = 1, a1 = 1, a2 = 1), algo = "LM"))
#' devRateQlStat(eq = list(damos_08, kontodimas_04, poly2),
#'               nlsDR = myNLS,
#'               df = list(myDf))
#' @export
devRateQlStat <- function(eq, nlsDR, df){
  if(class(nlsDR) == "list" & class(df) == "list" & length(eq) == length(nlsDR)){
    if(length(df) < length(nlsDR)){
      df <- rep(df, length(nlsDR))
    }
    temp <- lapply(seq_along(df), function(i){return(df[[i]][, 1])})
    devRate <- lapply(seq_along(df), function(i){return(df[[i]][, 2])})
    stats <- lapply(seq_along(nlsDR), function(i){
      # stinner_74 and lamb_92 exception
      if(eq[[i]]$id == "eq040" | eq[[i]]$id == "eq150"){
        # warning("stinner_74 and lamb_92 not implemented yet")
        dfStats <- data.frame(RSS = NA, RMSE = NA, NRMSE = NA, R.sq = NA,
                              R.sqAdj = NA, corOP = NA, shapiroStat = NA,
                              shapiroPvalue = NA)
        return(dfStats)
      }else{
        if(!is.null(nlsDR[[i]])){
          N <- length(temp[[i]])
          fitted <- stats::predict(nlsDR[[i]])
          res <- stats::residuals(nlsDR[[i]])
          p <- length(stats::coef(nlsDR[[i]]))
          RSS <- sum((devRate[[i]] - fitted)^2)
          RMSE <- sqrt(RSS / (N - (p - 1)))
          NRMSE <- RMSE/mean(devRate[[i]])
          R.sq <- 1 - sum((devRate[[i]] - fitted)^2) / sum((devRate[[i]]
                                                            - mean(devRate[[i]]))^2)
          R.sqAdj <- 1 - (N - 1)/(N - p) * (1 - R.sq)
          corOP <- stats::cor(fitted, devRate[[i]])
          if(length(unique(res)) != 1){
            shapiroStat <- stats::shapiro.test(res)$statistic
            shapiroPvalue <- stats::shapiro.test(res)$p.value
          }else{
            shapiroStat <- NA
            shapiroPvalue <- NA
          }
          dfStats <- data.frame(RSS = RSS, RMSE = RMSE, NRMSE = NRMSE,
                                R.sq = R.sq,  R.sqAdj = R.sqAdj, corOP = corOP,
                                shapiroStat = shapiroStat,
                                shapiroPvalue = shapiroPvalue)
          return(dfStats)
        }else{
          dfStats <- data.frame(RSS = NA, RMSE = NA, NRMSE = NA, R.sq = NA,
                                R.sqAdj = NA, corOP = NA, shapiroStat = NA,
                                shapiroPvalue = NA)
          return(dfStats)
        }
      }
    })
    stats <- do.call(rbind, stats)
    row.names(stats) <- lapply(seq_along(nlsDR), function(i){paste0("nls#", i)})
    return(stats)
  }else{
    # warning("nlsDR or df is not a list")
    return(NULL)
  }
}

#' Biological likelihood of nls fits
#'
#' Return a table of three metrics of development (CTmin, CTmax, Topt)
#'
#' @param nlsDR A list of nls objects.
#' @param propThresh The proportion of maximal development rate used as a
#'   threshold for estimating CTmin and CTmax when asymptotic equations are used
#'   (default value is 0.01)
#' @param eq A list of equations used for nls fitting.
#' @return An object of class \code{data.frame} with development metrics (CTmin,
#'   Ctmax, Topt) in columns and nls objects in rows.
#' @details NULL is returned when nlsDR or df are not a list.
#' @examples
#' myDf <- data.frame(T = seq(from = 0, to = 50, by = 10),
#'                    rT = c(0.001, 0.008, 0.02, 0.03, 0.018, 0.004))
#' myNLS <- list(devRateModel(eq = janisch_32,
#'                            df = myDf,
#'                            startValues = list(aa = 0.2, bb = 0.1, Dmin = 10, Topt = 30),
#'                            algo = "LM"),
#'               devRateModel(eq = kontodimas_04,
#'                            df = myDf,
#'                            startValues = list(aa = 1, Tmin = 7, Tmax = 40),
#'                            algo = "LM"),
#'               devRateModel(eq = poly2,
#'                            df = myDf,
#'                            startValues = list(a0 = 1, a1 = 1, a2 = 1),
#'                            algo = "LM"))
#' devRateQlBio(nlsDR = myNLS,
#'              eq = list(damos_08, kontodimas_04, poly2),
#'              propThresh = 0.1)
#' @export
devRateQlBio <- function(nlsDR, propThresh = 0.01, eq){
  stats <- lapply(seq_along(nlsDR), function(i){
    if(!is.null(nlsDR[[i]])){
      if(eq[[i]]$id == "eq030"){
        a <- unname(stats::coef(nlsDR[[i]])[1])
        b <- unname(stats::coef(nlsDR[[i]])[2])
        CTmin <- -(a/b)
        return(data.frame(CTmin = CTmin, CTmax = NA, Topt = NA))
      }
      if(eq[[i]]$id == "eq020" | eq[[i]]$id == "eq290"){
        T <- seq(from = -100, to = 100, by = 0.1)
        rT <- stats::predict(nlsDR[[i]], newdata = list(T = T))
        rT[is.na(rT)] <- 0
        rT[rT < 0] <- 0
        if(eq[[i]]$id == "eq020"){
          rT[rT < propThresh*max(rT)] <- 0
        }
        CTmin <- max(T[rT == min(rT)])
        return(data.frame(CTmin = CTmin, CTmax = NA, Topt = NA))
      }
      Topt <- stats::optimize(
        f = function(T){stats::predict(nlsDR[[i]], newdata = data.frame(T))},
        interval = c(0, 50),
        maximum = TRUE)$maximum
      T <- seq(from = -100, to = 100, by = 0.1)
      rT <- stats::predict(nlsDR[[i]], newdata = list(T = T))
      rT[is.na(rT)] <- 0
      rT[rT < 0] <- 0
      rT[rT < propThresh*rT[round(x = T, digits = 1) == round(x = Topt, digits = 1)]] <- 0
      CTmax <- min(T[rT == min(rT) & T > Topt])
      CTmin <- max(T[rT == min(rT) & T < Topt])
      return(data.frame(CTmin = CTmin, CTmax = CTmax, Topt = Topt))
    }else{
      return(data.frame(CTmin = NA, CTmax = NA, Topt = NA))
    }
  })
  stats <- do.call(rbind, stats)
  row.names(stats) <- lapply(seq_along(nlsDR), function(i){paste0("nls#", i)})
  return(stats)
}
