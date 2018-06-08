#'plot.GaussianSmoothadjust
#'
#'@description plot method for class "GaussianSmoothAdjust"
#'
#'@param obj an object of class "GaussianSmoothAdjust"
#'@return scatter plot of Yields vs Credit ratings with Gaussian smoother curves corresponding to the three credit rating BBB-, BBB, and BBB+
#'@export
#'
#'
plot.GaussianSmoothAdjust <- function(obj){
  plot(obj[[1]]$model$maturity,obj[[2]]$model$yield+obj[[6]],
       xlab="Maturity", ylab="Yield",
       main="Gaussian Smoother\nAdjusted for Credit Ratings",
       pch=as.numeric(obj[[5]]$Credit.rating))
  xx <- seq(0.1,max(obj[[1]]$model$maturity),0.1)
  yy <- unlist(sapply(sapply(lapply(xx,gswt1,data=obj[[5]]),"[",2),"[",1))
  tt1 <- table(obj[6])
  nn1 <- as.numeric(names(tt1))
  invisible(lines(xx,yy))
  invisible(lines(xx,yy+nn1[1]))
  invisible(lines(xx,yy+nn1[3]))
  n2 <- tt1[2]
  n3 <- tt1[3]
  n1 <- tt1[1]
  w1 <- n1/(n1+n2+n3)
  w2 <- n2/(n1+n2+n3)
  w3 <- n3/(n1+n2+n3)
  x <- w1*(yy+nn1[1])+w2*yy+w3*(yy+nn1[3])
  invisible(lines(xx,x, lty=2))
  abline(v=10,lty=2)
}
