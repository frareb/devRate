#' Find models for species
#'
devRateFind <- function(orderSP = "", familySP = "", species = ""){
  # getVarName <- function (myVar, name=deparse(substitute(myVar))) {
  #   return(name)
  # }
  for(i in names(devRateEqList)){
    eq <- get(i)
    if(orderSP != "" & familySP == "" & species == ""){
      if(orderSP %in% eq$startVal[,"ordersp"] == TRUE){
        cat("\n", strwrap(x = paste0(eq$name, ": ", eq$refShort, " [", i, "]"), width = 80))
      }
    }
    if(familySP != "" & species == ""){
      if(orderSP %in% eq$startVal[,"familysp"] == TRUE){
        cat("\n", strwrap(x = paste0(eq$name, ": ", eq$refShort, " [", i, "]"), width = 80))
      }
    }
    if(species != ""){
      if(orderSP %in% eq$startVal[,"sp"] == TRUE){
        cat("\n", strwrap(x = paste0(eq$name, ": ", eq$refShort, " [", i, "]"), width = 80))
      }
    }
  }


}

# devRateFind(orderSP = "Coleoptera")
