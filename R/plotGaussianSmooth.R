#'plot.GaussianSmooth
#'
#'@description plot method for class "GaussianSmooth"
#'
#'@param obj an object of class "GaussianSmooth"
#'@return A scatter plot of Yields vs Maturites with the Gaussian Smoothed line
#'@export
#'

plot.GaussianSmooth <- function(obj, ...){
  plot(obj[[1]]$model$maturity,obj[[2]]$model$yield, xlab="Maturity", ylab="Yield",
       main="Gaussian Smoother")
  xx <- seq(0.1,max(obj[[1]]$model$maturity),0.1)
  yy <- unlist(sapply(sapply(lapply(xx,gswt1,data=obj[[5]]),"[",2),"[",1))
  invisible(lines(xx,yy))
  abline(v=10,lty=2)
}
