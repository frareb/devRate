#' Compute the inverse of number of days between dates
#'
#' compDifDays computes the inverse of the difference between dates form a vector made of dates.
#'
#' @param vecDates A vector with dates.
#' @param dateFormat The format of dates (see \code{\link[base]{strptime}}).
#' @return A vector with the inverse of the difference between dates.
#' @examples
#' compDifDays(vecDates = c("28/12/15", "12/01/16", "25/01/16", "28/02/16", "15/03/16"))
#' compDifDays(vecDates = c("28/12/15", "12/01/14", "25/01/16", "28/02/16", "15/03/16"))
#' compDifDays(vecDates = c("28/12/15", "12/01/16", "25/01/16", "", ""))
#' @export
compDifDays <- function(vecDates, dateFormat = "%d/%m/%y"){
  vecDif <- sapply(2:length(vecDates), function(i){
    date1 <- as.POSIXlt(as.Date(vecDates[i - 1], dateFormat))
    date2 <- as.POSIXlt(as.Date(vecDates[i], dateFormat))
    daysDiff <- (date2 - date1)
    if(!is.na(daysDiff) && daysDiff < 0){
      msg <- paste0("Check your data for incorrect dates or tipo at position ", i, ". ",
                    vecDates[i], " is prior to ", vecDates[i - 1],". ")
      warning(msg)
    }
    return(daysDiff)
  })
  return(1 / vecDif)
}

#' Compute the inverse of number of days between dates from a data frame.
#'
#' @param dfDates A data.frame with dates (samples in columns and dates in rows).
#' @param dateFormatDf The format of dates (see \code{\link[base]{strptime}}).
#' @return A data.frame with the inverse of the difference between dates.
#' @examples
#' myDays <- data.frame(egg =  c("28/12/15", "28/12/15", "28/12/15", "28/12/15"),
#'                          larva1 = c("12/01/16", "12/01/16", "12/01/16", "13/01/16"),
#'                          larva2 = c("25/01/16", "26/01/16", "25/01/16", "29/01/16"),
#'                          pupa = c("12/02/16", "10/02/16", "14/02/16", "09/02/16"),
#'                          imago = c("28/02/16", "25/02/16", "27/02/16", "26/02/16"),
#'                          death = c("15/03/16", "12/03/16", "19/03/16", "20/03/16"))
#' compDifDaysDf(dfDates = myDays, dateFormat = "%d/%m/%y")
#' @export
compDifDaysDf <- function(dfDates, dateFormatDf = "%d/%m/%y"){
  daysDiff <- sapply(1:length(dfDates[,1]), function(j){
    compDifDays(vecDates = unlist(dfDates[j,]), dateFormat = dateFormatDf)
  })
  rownames(daysDiff) <- colnames(dfDates[,1:(ncol(dfDates)-1)])
  return(t(daysDiff))
}
