#' Display information about an equation
#'
#' @param eq The name of the equation.
#' @return Nothing.
#' @examples
#' devRateInfo(eq = campbell_74)
#' @export
devRateInfo <- function(eq){
  cat("----------------------------------------\n")
  cat(as.character(eq$name), "\n----------------------------------------\n")
  cat(strwrap(x = eq$ref, width = 80), sep = "\n")
  cat("\n")
  print(eq$eq)
  if(nrow(eq$startVal) > 0){
    cat("\nParameter estimates from the literature: \n\n")
    print(eq$startVal)
  }
  if(length(eq$com) > 0){cat("\nComments: ", strwrap(x = eq$com, width = 80), sep = "\n")}
}


#' Plot parameter estimates from the literature
#'
#' @param eq The name of the equation.
#' @param sortBy The filter to seperate species ("order", "family", "species", "").
#' @return Nothing.
#' @export
devRatePlotInfo <- function(eq, sortBy = ""){

}

# for(i in names(devRateEqList)){devRateInfo(eq = get(i))}
