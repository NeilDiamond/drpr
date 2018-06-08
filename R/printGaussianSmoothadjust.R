#'print.GaussianSmoothadjust
#'
#'@description print method for class "GaussianSmoothAdjust"
#'
#'@param obj an object of class "GaussianSmoothAdjust"
#'@return numeric vector consisting of Target Tenor, sigma, Target Tenor Yield, and Effective Term to Maturity
#'@export
#'

print.GaussianSmoothAdjust <- function(obj){
  adj <- sum(as.numeric(names(table(obj[[6]])))*table(obj[[6]]))/sum(as.numeric(table(obj[[6]])))
  result <- c(obj[[3]], obj[[4]], coef(obj[[2]])+adj, coef(obj[[1]]))
  names(result) <- c("Target Tenor", "Sigma", "Target Tenor Yield",
                     "Effective Term to Maturity")
  return(result)
}

