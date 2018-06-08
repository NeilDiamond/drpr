#'@export

plot.NelsonSiegelSvensson <- function(obj){
  plot(obj$model$Maturity, obj$model$Yield, xlab="Maturity", ylab="Yield",
       main="Nelson-Siegel-Svennson Model",las=1)
  lines(seq(0.1,max(obj$model$Maturity),0.1),
        predict(obj, newdata=data.frame(Maturity=seq(0.1,max(obj$model$Maturity),0.1))))
  abline(v=10,lty=2)
}
