#' Fit the Nelson-Siegel model
#
#
#' @param data data file with at least columns named Yield and Maturity
#' @param lambdastartval starting value for lambda in Nelson-Siegel model
#' @param epsilon a small value to prevent -Inf when taking log of zero
#' @return A NelsonSiegel object
#' @export

fitnelsonsiegel <- function(data, lambdastartval=lambdastart(2.5),
                                     epsilon=1E-14){
  lambda <- lambdastartval
  DF <- data
  y <- DF$Yield
  Z <- DF$Maturity*lambda
  F0 <- 1-(1-exp(-Z))/Z
  F1 <- (1-exp(-Z))/Z
  F2 <- ((1-exp(-Z))/Z-exp(-Z))
  xx1 <- nnnpls(cbind(F0,F1,F2),DF$Yield,c(1,1,-1))
  xx2 <- nnnpls(cbind(F0,F1,F2),DF$Yield,c(1,1,1))
  mindeviance=c(deviance(xx1),deviance(xx2))
  eval(parse(text=paste0("svA <- coef(xx",which.min(mindeviance),")")))
  fitorig1 <- nlsLM(Yield~ exp(delta1) + (exp(delta2)-exp(delta1)) * (1-exp(-Maturity*(exp(gamma)))) /
                      (Maturity*(exp(gamma))) +
                      delta3 * ((1-exp(-Maturity*(exp(gamma)))) /
                                  (Maturity*(exp(gamma)))-
                                  exp(-Maturity*(exp(gamma)))),
                    data=DF,
                    start=list(delta1=log(svA[1]+epsilon),
                               delta2=log(svA[2]+epsilon),
                               delta3=svA[3],
                               gamma=log(lambdastartval)),
                    control=nls.lm.control(maxiter=1000,maxfev=2000), trace=F,model=T)
  class(fitorig1) <- c("NelsonSiegel","nls")
  return(fitorig1)
}



