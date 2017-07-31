#' Find models for species
#'
#' @param orderSP Find models by Order.
#' @param familySP Find models by Family.
#' @param species Find models by species (Genus species).
#' @return A data.frame with the name of the equation and the number of occurrences in the database.
#' @examples
#' devRateFind(orderSP = "Lepidoptera")
#' devRateFind(familySP = "Gelechiidae")
#' devRateFind(species = "Tuta absoluta")
#' @export
devRateFind <- function(orderSP = "", familySP = "", species = ""){
  devRateEqList <- devRateEqList # avoid "no visible binding for global variable devRateEqList" Note
  vFind <- vector()
  vEq <- vector()
  for(i in names(devRateEqList)){
    eq <- get(i)
    if(orderSP != "" & familySP == "" & species == ""){
      if(orderSP %in% eq$startVal[,"ordersp"] == TRUE){
        occu <- sum(as.character(eq$startVal[,"ordersp"]) == orderSP)
        # cat("\n[", i, "]  ", strwrap(x = paste0(eq$name, ": ", eq$refShort, " (", occu, " times)"), width = 80))
        # cat("\n[", i, "]", strwrap(x = paste0(": ", occu, " times"), width = 80))
        vFind <- c(vFind, occu)
        vEq <- c(vEq, i)
      }
    }
    if(familySP != "" & species == ""){
      if(familySP %in% eq$startVal[,"familysp"] == TRUE){
        occu <- sum(as.character(eq$startVal[,"familysp"]) == familySP)
        # cat("\n[", i, "]  ", strwrap(x = paste0(eq$name, ": ", eq$refShort, " (", occu, " times)"), width = 80))
        # cat("\n[", i, "]", strwrap(x = paste0(": ", occu, " times"), width = 80))
        vFind <- c(vFind, occu)
        vEq <- c(vEq, i)
      }
    }
    if(species != ""){
      if(species %in% eq$startVal[,"genSp"] == TRUE){
        occu <- sum(as.character(eq$startVal[,"genSp"]) == species)
        # cat("\n[", i, "] ", strwrap(x = paste0(eq$name, ": ", eq$refShort, " (", occu, " times)"), width = 80))
        # cat("\n[", i, "]", strwrap(x = paste0(": ", occu, " times"), width = 80))
        vFind <- c(vFind, occu)
        vEq <- c(vEq, i)
      }
    }
  }
  dfFind <- data.frame(equation = vEq, occu = vFind)
  dfFind <- dfFind[order(dfFind[, 2], decreasing = TRUE), ]
  return(dfFind)
} ## campbell_74$startVal[campbell_74$startVal["genSp"] == "Tuta absoluta",]
