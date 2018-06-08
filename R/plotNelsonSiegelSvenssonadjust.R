#'plot.NelsonSiegelSvenssonadjust
#'
#'@description plot method for class "NelsonSiegelSvenssonAdjust"
#'
#'@param obj an object of class "NelsonSiegelSvenssonAdjust"
#'@return scatter plot of Yields vs Maturities and fitted Nelson-Siegel-Svensson curves corresponding to the three credit ratings BBB-, BBB, and BBB+
#'@export
#'

plot.NelsonSiegelSvenssonAdjust <- function(obj){
  plot(obj$model$Maturity, obj$model$Yield, xlab="Maturity", ylab="Yield",
       main="Nelson-Siegel-Svennson Model\nAdjusted for Credit Ratings",las=1,
       pch=1+obj$model$BBB+2*obj$model$BBBm)
  p1 <- predict(obj, newdata=data.frame(Maturity=seq(0.1,max(obj$model$Maturity),0.1),
                                        BBB=rep(0,length(seq(0.1,max(obj$model$Maturity),0.1))),
                                        BBBm=rep(0,length(seq(0.1,max(obj$model$Maturity),0.1)))))
  p2 <- predict(obj, newdata=data.frame(Maturity=seq(0.1,max(obj$model$Maturity),0.1),
                                        BBB=rep(1,length(seq(0.1,max(obj$model$Maturity),0.1))),
                                        BBBm=rep(0,length(seq(0.1,max(obj$model$Maturity),0.1)))))
  p3 <- predict(obj, newdata=data.frame(Maturity=seq(0.1,max(obj$model$Maturity),0.1),
                                        BBB=rep(0,length(seq(0.1,max(obj$model$Maturity),0.1))),
                                        BBBm=rep(1,length(seq(0.1,max(obj$model$Maturity),0.1)))))
  lines(seq(0.1,max(obj$model$Maturity),0.1),
        p1)
  lines(seq(0.1,max(obj$model$Maturity),0.1),
        p2)
  lines(seq(0.1,max(obj$model$Maturity),0.1),
        p3)

  n2 <- sum(obj$model$BBB, na.rm=T)
  n3 <- sum(obj$model$BBBm, na.rm=T)
  n1 <- length(obj$model$BBB[!is.na(obj$model$BBB)])-n2-n3
  w1 <- n1/(n1+n2+n3)
  w2 <- n2/(n1+n2+n3)
  w3 <- n3/(n1+n2+n3)
  p4 <- w1*p1+w2*p2+w3*p3
  lines(seq(0.1,max(obj$model$Maturity),0.1),
        p4, lty=2)
  abline(v=10,lty=2)
}
