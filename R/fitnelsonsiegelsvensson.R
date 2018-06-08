#' Fit the Nelson-Siegel-Svensson model
#
#
#' @param data data file with at least columns named Yield and Maturity
#' @param lambda1start starting value for lambda1
#' @param lambda2start starting value for lambda2; lambda2 > lambda1
#' @param epsilon a small value to prevent -Inf when taking log of zero
#' @return A NelsonSiegelSvensson object
#' @export
fitnelsonsiegelsvensson <- function(data, lambda1start=1/lambdastart(2.5),
                                    lambda2start=1/lambdastart(5), epsilon=1E-14,fixed=F){
  lambda1 <- lambda1start
  lambda2 <- lambda2start
  gamma1 <- log(lambda1start)
  gamma2 <- log(lambda2start-lambda1start)
  DF <- data
  y <- DF$Yield
  Z1 <- DF$Maturity/lambda1
  Z2 <- DF$Maturity/lambda2
  F0 <- 1-(1-exp(-Z1))/Z1
  F1 <- (1-exp(-Z1))/Z1
  F2 <- ((1-exp(-Z1))/Z1-exp(-Z1))
  F3 <-  ((1-exp(-Z2))/Z2-exp(-Z2))
  xx1 <- nnnpls(cbind(F0,F1,F2,F3),DF$Yield,c(1,1,-1,-1))
  xx2 <- nnnpls(cbind(F0,F1,F2,F3),DF$Yield,c(1,1,1,-1))
  xx3 <- nnnpls(cbind(F0,F1,F2,F3),DF$Yield,c(1,1,-1,1))
  xx4 <- nnnpls(cbind(F0,F1,F2,F3),DF$Yield,c(1,1,1,1))
  mindeviance=c(deviance(xx1),deviance(xx2),deviance(xx3),deviance(xx4))
  eval(parse(text=paste0("svA <- coef(xx",which.min(mindeviance),")")))
  print(svA)
  if(fixed){
    fitorig1 <- nlsLM(Yield~ exp(delta1) + (exp(delta2)-exp(delta1)) * (1-exp(-Maturity/(exp(gamma1)))) /
                        (Maturity/(exp(gamma1))) +
                        delta3 * ((1-exp(-Maturity/(exp(gamma1)))) /
                                    (Maturity/(exp(gamma1)))-
                                    exp(-Maturity/(exp(gamma1))))+
                        delta4 * ((1-exp(-Maturity/(exp(gamma2)+exp(gamma1)))) /
                                    (Maturity/(exp(gamma2)+exp(gamma1)))-
                                    exp(-Maturity/(exp(gamma2)+exp(gamma1)))),
                      data=data,
                      start=list(delta1=log(svA[1]+epsilon),
                                 delta2=log(svA[2]+epsilon),
                                 delta3=svA[3],
                                 delta4=svA[4]),
                      control=nls.lm.control(maxiter=1000,maxfev=2000), trace=F,model=T)

     } else {
      fitorig1 <- nlsLM(Yield~ exp(delta1) + (exp(delta2)-exp(delta1)) * (1-exp(-Maturity/(exp(gamma1)))) /
                      (Maturity/(exp(gamma1))) +
                      delta3 * ((1-exp(-Maturity/(exp(gamma1)))) /
                                  (Maturity/(exp(gamma1)))-
                                  exp(-Maturity/(exp(gamma1))))+
                      delta4 * ((1-exp(-Maturity/(exp(gamma2)+exp(gamma1)))) /
                                  (Maturity/(exp(gamma2)+exp(gamma1)))-
                                  exp(-Maturity/(exp(gamma2)+exp(gamma1)))),
                      data=data,
                      start=list(delta1=log(svA[1]+epsilon),
                                            delta2=log(svA[2]+epsilon),
                                            delta3=svA[3],delta4=svA[4],
                                            gamma1=log(lambda1start),
                                            gamma2=log(lambda2start-lambda1start)),
                    control=nls.lm.control(maxiter=1000,maxfev=2000), trace=F,model=T)
     }
  class(fitorig1) <- c("NelsonSiegelSvensson","nls")
  return(fitorig1)
}


