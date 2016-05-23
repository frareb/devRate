#' Find models for species
#'
#' @param orderSP Find models by Order.
#' @param familySP Find models by Family.
#' @param species Find models by species (Genus species).
#' @return Nothing.
#' @examples
#' devRateFind(orderSP = "Lepidoptera")
#' devRateFind(familySP = "Gelechiidae")
#' devRateFind(species = "Tuta absoluta")
#' @export
devRateFind <- function(orderSP = "", familySP = "", species = ""){
  # getVarName <- function (myVar, name=deparse(substitute(myVar))) {
  #   return(name)
  # }
  for(i in names(devRateEqList)){
    eq <- get(i)
    if(orderSP != "" & familySP == "" & species == ""){
      if(orderSP %in% eq$startVal[,"ordersp"] == TRUE){
        occu <- sum(as.character(eq$startVal[,"ordersp"]) == orderSP)
        cat("\n[", i, "]  ", strwrap(x = paste0(eq$name, ": ", eq$refShort, " (", occu, " times)"), width = 80))
      }
    }
    if(familySP != "" & species == ""){
      if(familySP %in% eq$startVal[,"familysp"] == TRUE){
        occu <- sum(as.character(eq$startVal[,"familysp"]) == familySP)
        cat("\n[", i, "]  ", strwrap(x = paste0(eq$name, ": ", eq$refShort, " (", occu, " times)"), width = 80))
      }
    }
    if(species != ""){
      if(species %in% eq$startVal[,"genSp"] == TRUE){
        occu <- sum(as.character(eq$startVal[,"genSp"]) == species)
        cat("\n[", i, "] ", strwrap(x = paste0(eq$name, ": ", eq$refShort, " (", occu, " times)"), width = 80))
      }
    }
  }
}
