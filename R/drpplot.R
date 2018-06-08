#'drplot
#'
#'@description Compare Gaussian Smoothing, Nelson-Siegel, and Nelson-Siegel-Svensson fits
#'
#'@param GS10
#'@param NSmodel
#'@param NSSmodel
#'@return scatter plot of Yields vs Maturities with Gaussian Smooth, Nelson-Siegel, and Nelson-Siegel-Svensson curves
#'@export
#'

drpplot <- function(GSmodel10, NSmodel, NSSmodel){
  plot(NSmodel$model$Yield~NSmodel$model$Maturity,xlab="Maturity", ylab="Yield",
       main="Comparison of Gaussian Smoothing,\nNelson-Siegel, and Nelson-Siegel-Svensson")
  lines(seq(0.1,max(NSmodel$model$Maturity),0.1),
        predict(NSmodel, newdata=data.frame(Maturity=seq(0.1,max(NSmodel$model$Maturity),0.1))))
  lines(seq(0.1,max(NSSmodel$model$Maturity),0.1),
        predict(NSSmodel, newdata=data.frame(Maturity=seq(0.1,max(NSSmodel$model$Maturity),0.1))))
  xx <- seq(0.1,max(GSmodel10[[1]]$model$maturity),0.1)
  yy <- unlist(sapply(sapply(lapply(xx,gswt1,data=GSmodel10[[5]]),"[",2),"[",1))
  invisible(lines(xx,yy))
  abline(v=10, lty=2)
}
