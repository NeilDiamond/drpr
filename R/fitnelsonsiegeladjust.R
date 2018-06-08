#' Fit the Nelson-Siegel model adjusting for Credit ratings
#
#'
#' @param data data file with at least columns named Yield and Maturity and Credit rating
#' @param lambdastartval starting value for lambda in Nelson-Siegel model
#' @param epsilon a small value to prevent -Inf when taking log of zero
#' @return A NelsonSiegelAdjust object
#' @export
fitnelsonsiegeladjust <- function(data, lambdastartval=lambdastart(2.5),
                                                 epsilon=1E-14){
  lambda <- lambdastartval
  DF <- data
  dummyvars <- model.matrix(~DF$Credit.rating)[,2:3]
  y <- DF$Yield
  Z <- DF$Maturity*lambda
  F0 <- 1-(1-exp(-Z))/Z
  F1 <- (1-exp(-Z))/Z
  F2 <- ((1-exp(-Z))/Z-exp(-Z))
  BBB <- as.numeric(as.character(DF$Credit.rating)=="BBB")
  BBBm <- as.numeric(as.character(DF$Credit.rating)=="BBB-")

  xx1 <- nnnpls(cbind(F0,F1,F2,BBB+BBBm,BBBm),DF$Yield,c(1,1,-1,1,1))
  xx2 <- nnnpls(cbind(F0,F1,F2,BBB+BBBm,BBBm),DF$Yield,c(1,1,1,1,1))


  mindeviance=c(deviance(xx1),deviance(xx2))
  eval(parse(text=paste0("svA <- coef(xx",which.min(mindeviance),")")))

  fitorig1 <- nlsLM(Yield~ exp(delta1) + (exp(delta2)-exp(delta1)) * (1-exp(-Maturity*(exp(gamma)))) /
                      (Maturity*(exp(gamma))) +
                      delta3 * ((1-exp(-Maturity*(exp(gamma)))) /
                                  (Maturity*(exp(gamma)))-
                                  exp(-Maturity*(exp(gamma))))+
                           exp(theta1)*(BBB+BBBm)+exp(theta2)*BBBm,
                    data=DF,
                    start=list(delta1=log(svA[1]+epsilon),
                               delta2=log(svA[2]+epsilon),
                               delta3=svA[3],
                               gamma=log(lambdastartval),
                               theta1=log(svA[4]+epsilon),
                               theta2=log(svA[5]+epsilon)),
                    control=nls.lm.control(maxiter=1000,maxfev=2000), trace=F,model=T)

  class(fitorig1) <- c("NelsonSiegelAdjust","nls")
  return(fitorig1)
}









