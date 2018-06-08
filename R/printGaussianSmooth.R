#'print.GaussianSmooth
#'
#'@description print method for class "GaussianSmooth"
#'
#'@param obj an object of class "GaussianSmooth"
#'@return numeric vector consisting of Target Tenor, sigma, Target Tenor Yield, and Effective Term to Maturity
#'@export
#'

print.GaussianSmooth <- function(obj, ...){
  result <- c(obj[[3]], obj[[4]], coef(obj[[2]]), coef(obj[[1]]))
  names(result) <- c("Target Tenor", "Sigma", "Target Tenor Yield",
                     "Effective Term to Maturity")
  return(result)
}
