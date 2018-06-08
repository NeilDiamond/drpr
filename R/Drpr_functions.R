gswt1 <-
  function (tt = 10, data=Jan15, sigma = 1.5, ...){
    maturity <- data$Maturity
    issue <- data$Amount.issued
    yield <- data$Yield
    w <- issue * dnorm(maturity, tt, sigma)
    w <- w/sum(w)
    reg1 <- lm(maturity ~ 1, weights = w, model=T)
    reg2 <- lm(yield ~ 1, weights = w, model=T)
    result <- list(reg1, reg2, tt, sigma, data)
    class(result) <- "GaussianSmooth"
    return(result)
}

gkest1 <- function(sigma=1.5, data=Jan15){
  gswt0 <-
    function (tt = 10, data=data, sigma = sigma, ...){
      maturity <- data$Maturity
      issue <- data$Amount.issued
      yield <- data$Yield
      w <- issue * dnorm(maturity, tt, sigma)
      w <- w/sum(w)
      reg1 <- lm(maturity ~ 1, weights = w, model=T)
      reg2 <- lm(yield ~ 1, weights = w, model=T)
      result <- c(tt,sigma, coef(reg2), coef(reg1))
      names(result) <- c("Target Tenor","Sigma","Target Tenor Yield","Effective Term to Maturity")
      return(result)
   }
  interpmat <- t(sapply(c(3,5,7,10), gswt0,
                        sigma=sigma, data=data))
  interpmat <- as.data.frame(interpmat)
  colnames(interpmat) <- c("Tenor","Sigma","SemiAnnYield","EffectiveTenor")
  AnnYield <- ((1+interpmat[,3]/200)^2-1)*100
  Interp7yrYield <- interpmat[3,3]+
    (interpmat[4,3]-interpmat[3,3])/(interpmat[4,4]-interpmat[3,4])*(7-interpmat[3,4])
  Extrap10yrYield <- interpmat[3,3]+
    (interpmat[4,3]-interpmat[3,3])/(interpmat[4,4]-interpmat[3,4])*(10-interpmat[3,4])
  Interp7yrYield_ann <- ((1+Interp7yrYield/200)^2-1)*100
  Extrap10yrYield_ann <- ((1+Extrap10yrYield/200)^2-1)*100
  AnnYieldMat <- cbind(interpmat, AnnYield)
  results <- list(round(AnnYieldMat,4), round(Interp7yrYield,4), round(Extrap10yrYield,4),
                  round(Interp7yrYield_ann,4), round(Extrap10yrYield_ann, 4))
  names(results) <- c("Annualized Yields", "Interpolated 7 year Yield (Semi-annual basis)",
                      "Extrapolated 10 year Yield (Semi-annual basis)",
                      "Interpolated 7 year Yield (Annualized Basis)",
                      "Extrapolated 10 year Yield (Annualized Basis)")
  return(results)
}

print.GaussianSmooth <- function(obj, ...){
  result <- c(obj[[3]], obj[[4]], coef(obj[[2]]), coef(obj[[1]]))
  names(result) <- c("Target Tenor", "Sigma", "Target Tenor Yield",
                     "Effective Term to Maturity")
  return(result)
}


summary.GaussianSmooth <- function(obj, ...){
  gett.GS <- function(obj){
    c(obj[[3]],obj[[4]],coef(obj[[2]]),coef(obj[[1]]))
  }
  interpmat <- matrix(NA, 4, 4)
  interpmat[1,] <- gett.GS(gswt1(3,
                                 data=obj[[5]]))
  interpmat[2,] <- gett.GS(gswt1(5,
                                 data=obj[[5]]))
  interpmat[3,] <- gett.GS(gswt1(7,
                                 data=obj[[5]]))
  interpmat[4,] <- gett.GS(gswt1(10,
                                 data=obj[[5]]))
  interpmat <- as.data.frame(interpmat)
  colnames(interpmat) <- c("Tenor","Sigma","SemiAnnYield","EffectiveTenor")
  AnnYield <- ((1+interpmat[,3]/200)^2-1)*100
  Interp7yrYield <- interpmat[3,3]+
    (interpmat[4,3]-interpmat[3,3])/(interpmat[4,4]-interpmat[3,4])*(7-interpmat[3,4])
  Extrap10yrYield <- interpmat[3,3]+
    (interpmat[4,3]-interpmat[3,3])/(interpmat[4,4]-interpmat[3,4])*(10-interpmat[3,4])
  Interp7yrYield_ann <- ((1+Interp7yrYield/200)^2-1)*100
  Extrap10yrYield_ann <- ((1+Extrap10yrYield/200)^2-1)*100
  AnnYieldMat <- cbind(interpmat, AnnYield)
  results <- list(round(AnnYieldMat,4), round(Interp7yrYield,4), round(Extrap10yrYield,4),
                  round(Interp7yrYield_ann,4), round(Extrap10yrYield_ann, 4))
  names(results) <- c("Annualized Yields", "Interpolated 7 year Yield (Semi-annual basis)",
                      "Extrapolated 10 year Yield (Semi-annual basis)",
                      "Interpolated 7 year Yield (Annualized Basis)",
                      "Extrapolated 10 year Yield (Annualized Basis)")
  results
}



plot.GaussianSmooth <- function(obj, ...){
  plot(obj[[1]]$model$maturity,obj[[2]]$model$yield, xlab="Maturity", ylab="Yield",
       main="Gaussian Smoother")
  xx <- seq(0.1,max(obj[[1]]$model$maturity),0.1)
  yy <- unlist(sapply(sapply(lapply(xx,gswt1,data=obj[[5]]),"[",2),"[",1))
  invisible(lines(xx,yy))
  abline(v=10,lty=2)
}



gswt1adjust <- function(tt = 10, data=Jan15, sigma = 1.5){
  gswt0 <-
    function  (...){
      maturity <- data$Maturity
      issue <- data$Amount.issued
      yield <- data$Yield
      w <- issue * dnorm(maturity, tt, sigma)
      w <- w/sum(w)
      reg1 <- lm(maturity ~ 1, weights = w, model=T)
      reg2 <- lm(yield ~ 1, weights = w, model=T)
      result <- c(tt,sigma, coef(reg2), coef(reg1))
      names(result) <- c("Target Tenor","Sigma","Target Tenor Yield","Effective Term to Maturity")
      return(result)
    }
  maturity <- data$Maturity
  issue <- data$Amount.issued
  yield <- data$Yield
  w <- issue * dnorm(maturity, tt, sigma)
  w <- w/sum(w)
  adjust1 <- rep(0, nrow(data))
  newdata <- data.frame(Maturity=data$Maturity, Amount.issued=data$Amount.issued,
                        Yield=data$Yield)

  resids1 <- data$Yield - t(sapply(data$Maturity, gswt0, data=newdata))[,3]
  adjust <- with(data, model.matrix(~Credit.rating))[,-1]%*%
    coef(lm(resids1~Credit.rating, data=data))[-1]
  i <- 0
  while(sum(adjust^2)>10^(-8)){
    i <- i+1
    #cat(i, sum(adjust^2),"\n")
    adjust1 <- adjust+adjust1
    newdata <- data.frame(Maturity=data$Maturity, Amount.issued=data$Amount.issued,
                          Yield=data$Yield-adjust1)
    resids1 <- data$Yield - adjust1 - t(sapply(data$Maturity, gswt0, data=newdata))[,3]
    adjust <- with(data, model.matrix(~Credit.rating))[,-1]%*%
      coef(lm(resids1~Credit.rating, data=data))[-1]
  }

  reg1 <- lm(maturity ~ 1, weights = w, model=T)
  reg2 <- lm(yield -adjust1 ~ 1, weights = w, model=T)
  result <- list(reg1, reg2, tt, sigma, data, adjust1, with(data, model.matrix(~Credit.rating)))
  class(result) <- "GaussianSmoothAdjust"
  return(result)
}


gkest1adjust <- function(sigma=1.5, data){
  gswt0adjust <- function(tt = 10, data=data, sigma = sigma){
    maturity <- data$Maturity
    issue <- data$Amount.issued
    yield <- data$Yield
    w <- issue * dnorm(maturity, tt, sigma)
    w <- w/sum(w)
    adjust1 <- rep(0, nrow(data))
    newdata <- data.frame(Maturity=data$Maturity, Amount.issued=data$Amount.issued,
                          Yield=data$Yield)
    resids1 <- data$Yield - t(sapply(data$Maturity, gswt0, data=newdata))[,3]
    adjust <- with(data, model.matrix(~Credit.rating))[,-1]%*%
      coef(lm(resids1~Credit.rating, data=data))[-1]
    i <- 0
    while(sum(adjust^2)>10^(-8)){
      i <- i+1
      #cat(i, sum(adjust^2),"\n")
      adjust1 <- adjust+adjust1
      newdata <- data.frame(Maturity=data$Maturity, Amount.issued=data$Amount.issued,
                            Yield=data$Yield-adjust1)
      resids1 <- data$Yield - adjust1 - t(sapply(data$Maturity, gswt0, data=newdata))[,3]
      adjust <- with(data, model.matrix(~Credit.rating))[,-1]%*%
        coef(lm(resids1~Credit.rating, data=data))[-1]
    }

    reg1 <- lm(maturity ~ 1, weights = w, model=T)
    reg2 <- lm(yield -adjust1 ~ 1, weights = w, model=T)
    result <- c(tt,sigma, coef(reg2), coef(reg1))
    names(result) <- c("Target Tenor","Sigma","Target Tenor Yield","Effective Term to Maturity")
    return(result)
  }

  interpmat <- t(sapply(c(3,5,7,10), gswt0adjust,
                        sigma=sigma, data=data))
  interpmat <- as.data.frame(interpmat)
  colnames(interpmat) <- c("Tenor","Sigma","SemiAnnYield","EffectiveTenor")
  AnnYield <- ((1+interpmat[,3]/200)^2-1)*100
  Interp7yrYield <- interpmat[3,3]+
    (interpmat[4,3]-interpmat[3,3])/(interpmat[4,4]-interpmat[3,4])*(7-interpmat[3,4])
  Extrap10yrYield <- interpmat[3,3]+
    (interpmat[4,3]-interpmat[3,3])/(interpmat[4,4]-interpmat[3,4])*(10-interpmat[3,4])
  Interp7yrYield_ann <- ((1+Interp7yrYield/200)^2-1)*100
  Extrap10yrYield_ann <- ((1+Extrap10yrYield/200)^2-1)*100
  AnnYieldMat <- cbind(interpmat, AnnYield)
  results <- list(round(AnnYieldMat,4), round(Interp7yrYield,4), round(Extrap10yrYield,4),
                  round(Interp7yrYield_ann,4), round(Extrap10yrYield_ann, 4))
  names(results) <- c("Annualized Yields", "Interpolated 7 year Yield (Semi-annual basis)",
                      "Extrapolated 10 year Yield (Semi-annual basis)",
                      "Interpolated 7 year Yield (Annualized Basis)",
                      "Extrapolated 10 year Yield (Annualized Basis)")
  results
}


print.GaussianSmoothAdjust <- function(obj){
  adj <- sum(as.numeric(names(table(obj[[6]])))*table(obj[[6]]))/sum(as.numeric(table(obj[[6]])))
  result <- c(obj[[3]], obj[[4]], coef(obj[[2]])+adj, coef(obj[[1]]))
  names(result) <- c("Target Tenor", "Sigma", "Target Tenor Yield",
                     "Effective Term to Maturity")
  return(result)
}


summary.GaussianSmoothAdjust <- function(obj){
  adj <- sum(as.numeric(names(table(obj[[6]])))*table(obj[[6]]))/sum(as.numeric(table(obj[[6]])))
  gett.GS <- function(obj){
    c(obj[[3]],obj[[4]],coef(obj[[2]])+adj,coef(obj[[1]]))
  }
  interpmat <- matrix(NA, 4, 4)
  interpmat[1,] <- gett.GS(gswt1(3,
                                 data=obj[[5]]))
  interpmat[2,] <- gett.GS(gswt1(5,
                                 data=obj[[5]]))
  interpmat[3,] <- gett.GS(gswt1(7,
                                 data=obj[[5]]))
  interpmat[4,] <- gett.GS(gswt1(10,
                                 data=obj[[5]]))
  interpmat <- as.data.frame(interpmat)
  colnames(interpmat) <- c("Tenor","Sigma","SemiAnnYield","EffectiveTenor")
  AnnYield <- ((1+interpmat[,3]/200)^2-1)*100
  Interp7yrYield <- interpmat[3,3]+
    (interpmat[4,3]-interpmat[3,3])/(interpmat[4,4]-interpmat[3,4])*(7-interpmat[3,4])
  Extrap10yrYield <- interpmat[3,3]+
    (interpmat[4,3]-interpmat[3,3])/(interpmat[4,4]-interpmat[3,4])*(10-interpmat[3,4])
  Interp7yrYield_ann <- ((1+Interp7yrYield/200)^2-1)*100
  Extrap10yrYield_ann <- ((1+Extrap10yrYield/200)^2-1)*100
  AnnYieldMat <- cbind(interpmat, AnnYield)
  results <- list(round(AnnYieldMat,4), round(Interp7yrYield,4), round(Extrap10yrYield,4),
                  round(Interp7yrYield_ann,4), round(Extrap10yrYield_ann, 4))
  names(results) <- c("Annualized Yields", "Interpolated 7 year Yield (Semi-annual basis)",
                      "Extrapolated 10 year Yield (Semi-annual basis)",
                      "Interpolated 7 year Yield (Annualized Basis)",
                      "Extrapolated 10 year Yield (Annualized Basis)")
  results
}

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

lambdastart <- function(maturity=2.5){
  beta2 <- function(lam){
    ((1-exp(-maturity*lam))/(maturity*lam)-exp(-maturity*lam))
  }
  optimize(f=beta2, interval=c(10^(-6),10^(6),1000), maximum=T)$maximum
}

nelsonsiegelorig <- function(maturity, lambda, beta1, beta2, beta3){
  return(beta1 + beta2 * (1-exp(-lambda*maturity)) / (lambda*maturity)+
           beta3 * ((1-exp(-lambda*maturity)) / (lambda*maturity)-exp(-lambda*maturity)))
}

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

print.NelsonSiegel <- function(obj){
  transformns <- function(obj){
    cc <- coef(obj)
    tt <- cc
    tt[1] <- exp(cc[1]); tt[2] <- exp(cc[2])-exp(cc[1])
    tt[4] <- exp(cc[4]);
    names(tt) <- c("beta1","beta2","beta3","lambda")
    return(tt)
  }

  x <- predict(obj, newdata=data.frame(Maturity=10))[1]
  p <- getFromNamespace("print.nls","stats")(obj)
  r <- list(transformns(obj),x,((1+x/200)^2-1)*100)
  attributes(r) <- NULL

  names(r) <- c("Original Parameters","Ten Year Yield (Semi-Annual Basis)",
                "Ten Year Yield (Annual Basis)")
  return(r)
}







summary.NelsonSiegel <- function(obj){
  transformns <- function(obj){
    cc <- coef(obj)
    tt <- cc
    tt[1] <- exp(cc[1]); tt[2] <- exp(cc[2])-exp(cc[1])
    tt[4] <- exp(cc[4]);
    names(tt) <- c("beta1","beta2","beta3","lambda")
    return(tt)
  }
  x <- predict(obj, newdata=data.frame(Maturity=10))[1]
  r <- list(getFromNamespace("summary.nls","stats")(obj),transformns(obj),x,((1+x/200)^2-1)*100)
  attributes(r) <- NULL
  names(r) <- c("Fitted Nelson-Siegel Model","Original Parameters","Ten Year Yield (Semi-Annual Basis)",
                "Ten Year Yield (Annual Basis)")
  return(r)
}


plot.NelsonSiegel <- function(obj){
  plot(obj$model$Maturity, obj$model$Yield, xlab="Maturity", ylab="Yield",
       main="Nelson-Siegel Model",las=1)
  lines(seq(0.1,max(obj$model$Maturity),0.1),
        predict(obj, newdata=data.frame(Maturity=seq(0.1,max(obj$model$Maturity),0.1))))
  abline(v=10,lty=2)
}


fitnelsonsiegelsvensson <- function(data, lambda1start=1/lambdastart(2.5),
                                    lambda2start=1/lambdastart(5), epsilon=1E-14){
  lambda1 <- lambda1start
  lambda2 <- lambda2start
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
  class(fitorig1) <- c("NelsonSiegelSvensson","nls")
  return(fitorig1)
}

print.NelsonSiegelSvensson <- function(obj){
  transformnss <- function(obj){
    cc <- coef(obj)
    tt <- cc
    tt[1] <- exp(cc[1]); tt[2] <- exp(cc[2])-exp(cc[1])
    tt[5] <- exp(cc[5]); tt[6] <- exp(cc[5])+exp(cc[6])
    names(tt) <- c("beta1","beta2","beta3","beta4","lambda1","lambda2")
    return(tt)
  }
  x <- predict(obj, newdata=data.frame(Maturity=10))[1]
  p <- getFromNamespace("print.nls","stats")(obj)
  r <- list(transformnss(obj),x,((1+x/200)^2-1)*100)
  attributes(r) <- NULL

  names(r) <- c("Original Parameters","Ten Year Yield (Semi-Annual Basis)",
                "Ten Year Yield (Annual Basis)")
  return(r)
}

summary.NelsonSiegelSvensson <- function(obj){
  transformnss <- function(obj){
    cc <- coef(obj)
    tt <- cc
    tt[1] <- exp(cc[1]); tt[2] <- exp(cc[2])-exp(cc[1])
    tt[5] <- exp(cc[5]); tt[6] <- exp(cc[5])+exp(cc[6])
    names(tt) <- c("beta1","beta2","beta3","beta4","lambda1","lambda2")
    return(tt)
  }
  Z10 <- 10/(obj$model$lambda1)
  Z20 <- 10/(obj$model$lambda2)
  F10 <- (1-exp(-Z10))/Z10
  F20 <- ((1-exp(-Z10))/Z10-exp(-Z10))
  F30 <-  ((1-exp(-Z20))/Z20-exp(-Z20))
  x <- predict(obj, newdata=data.frame(Maturity=10))[1]
  r <- list(
    getFromNamespace("summary.nls","stats")(obj),transformnss(obj),x,((1+x/200)^2-1)*100)
  attributes(r) <- NULL
  names(r) <- c("Fitted Nelson-Siegel-Svensson Model",
                "Original Parameters","Ten Year Yield (Semi-Annual Basis)",
                "Ten Year Yield (Annual Basis)")
  return(r)
}


plot.NelsonSiegelSvensson <- function(obj){
  plot(obj$model$Maturity, obj$model$Yield, xlab="Maturity", ylab="Yield",
       main="Nelson-Siegel-Svennson Model",las=1)
  lines(seq(0.1,max(obj$model$Maturity),0.1),
        predict(obj, newdata=data.frame(Maturity=seq(0.1,max(obj$model$Maturity),0.1))))
  abline(v=10,lty=2)
}




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

print.NelsonSiegelAdjust <- function(obj){
  transformnsa <- function(obj){
    cc <- coef(obj)
    tt <- cc
    tt[1] <- exp(cc[1]); tt[2] <- exp(cc[2])-exp(cc[1])
    tt[4] <- exp(cc[4]); tt[5] <- exp(cc[6]); tt[6] <- -exp(cc[5])
    names(tt) <- c("beta1","beta2","beta3","lambda", "beta5", "beta6")
    return(tt)
  }
  x1 <- predict(obj, newdata=data.frame(Maturity=10, BBBm=0, BBB=0))[1]
  x2 <- predict(obj, newdata=data.frame(Maturity=10, BBBm=0, BBB=1))[1]
  x3 <- predict(obj, newdata=data.frame(Maturity=10, BBBm=1, BBB=0))[1]
  n2 <- sum(obj$model$BBB, na.rm=T)
  n3 <- sum(obj$model$BBBm, na.rm=T)
  n1 <- length(obj$model$BBB[!is.na(obj$model$BBB)])-n2-n3
  w1 <- n1/(n1+n2+n3)
  w2 <- n2/(n1+n2+n3)
  w3 <- n3/(n1+n2+n3)
  x <- w1*x1+w2*x2+w3*x3
  p <- getFromNamespace("print.nls","stats")(obj)
  r <- list(transformnsa(obj),x,((1+x/200)^2-1)*100)
  attributes(r) <- NULL

  names(r) <- c("Original Parameters","Ten Year Yield (Semi-Annual Basis)",
                "Ten Year Yield (Annual Basis)")
  return(r)
}

summary.NelsonSiegelAdjust <- function(obj){
  transformnsa <- function(obj){
    cc <- coef(obj)
    tt <- cc
    tt[1] <- exp(cc[1]); tt[2] <- exp(cc[2])-exp(cc[1])
    tt[4] <- exp(cc[4]); tt[5] <- exp(cc[6]); tt[6] <- -exp(cc[5])
    names(tt) <- c("beta1","beta2","beta3","lambda", "beta5", "beta6")
    return(tt)
  }
  x1 <- predict(obj, newdata=data.frame(Maturity=10, BBBm=0, BBB=0))[1]
  x2 <- predict(obj, newdata=data.frame(Maturity=10, BBBm=0, BBB=1))[1]
  x3 <- predict(obj, newdata=data.frame(Maturity=10, BBBm=1, BBB=0))[1]
  n2 <- sum(obj$model$BBB, na.rm=T)
  n3 <- sum(obj$model$BBBm, na.rm=T)
  n1 <- length(obj$model$BBB[!is.na(obj$model$BBB)])-n2-n3
  w1 <- n1/(n1+n2+n3)
  w2 <- n2/(n1+n2+n3)
  w3 <- n3/(n1+n2+n3)
  x <- w1*x1+w2*x2+w3*x3
  r <- list(getFromNamespace("summary.nls","stats")(obj),transformnsa(obj),x,((1+x/200)^2-1)*100)
  attributes(r) <- NULL
  names(r) <- c("Fitted Nelson-Siegel Model","Original Parameters","Ten Year Yield (Semi-Annual Basis)",
                "Ten Year Yield (Annual Basis)")
  return(r)
}



plot.NelsonSiegelAdjust <- function(obj){
  plot(obj$model$Maturity, obj$model$Yield, xlab="Maturity", ylab="Yield",
       main="Nelson-Siegel Model\nAdjusted for Credit Ratings",las=1,
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


fitnelsonsiegelsvenssonadjust <- function(data, lambda1start=1/lambdastart(2.5),
                                          lambda2start=1/lambdastart(5), epsilon=1E-14){
  lambda1 <- lambda1start
  lambda2 <- lambda2start
  DF <- data
  y <- DF$Yield
  Z1 <- DF$Maturity/lambda1
  Z2 <- DF$Maturity/lambda2
  F0 <- 1-(1-exp(-Z1))/Z1
  F1 <- (1-exp(-Z1))/Z1
  F2 <- ((1-exp(-Z1))/Z1-exp(-Z1))
  F3 <-  ((1-exp(-Z2))/Z2-exp(-Z2))
  BBB <- as.numeric(as.character(DF$Credit.rating)=="BBB")
  BBBm <- as.numeric(as.character(DF$Credit.rating)=="BBB-")


  xx1 <- nnnpls(cbind(F0,F1,F2,F3,BBB+BBBm,BBBm),DF$Yield,c(1,1,-1,-1,1,1))
  xx2 <- nnnpls(cbind(F0,F1,F2,F3,BBB+BBBm,BBBm),DF$Yield,c(1,1,1,-1,1,1))
  xx3 <- nnnpls(cbind(F0,F1,F2,F3,BBB+BBBm,BBBm),DF$Yield,c(1,1,-1,1,1,1))
  xx4 <- nnnpls(cbind(F0,F1,F2,F3,BBB+BBBm,BBBm),DF$Yield,c(1,1,1,1,1,1))
  mindeviance=c(deviance(xx1),deviance(xx2),deviance(xx3),deviance(xx4))
  eval(parse(text=paste0("svA <- coef(xx",which.min(mindeviance),")")))
  fitorig1 <- nlsLM(Yield~ exp(delta1) + (exp(delta2)-exp(delta1)) * (1-exp(-Maturity/(exp(gamma1)))) /
                      (Maturity/(exp(gamma1))) +
                      delta3 * ((1-exp(-Maturity/(exp(gamma1)))) /
                                  (Maturity/(exp(gamma1)))-
                                  exp(-Maturity/(exp(gamma1))))+
                      delta4 * ((1-exp(-Maturity/(exp(gamma2)+exp(gamma1)))) /
                                  (Maturity/(exp(gamma2)+exp(gamma1)))-
                                  exp(-Maturity/(exp(gamma2)+exp(gamma1))))+
                      exp(theta1)*(BBB+BBBm)+exp(theta2)*BBBm,
                    data=DF,
                    start=list(delta1=log(svA[1]+epsilon),
                               delta2=log(svA[2]+epsilon),
                               delta3=svA[3],delta4=svA[4],
                               gamma1=log(lambda1start),
                               gamma2=log(lambda2start-lambda1start),
                               theta1=log(svA[5]+epsilon),
                               theta2=log(svA[6]+epsilon)),
                    control=nls.lm.control(maxiter=1024,maxfev=10000), trace=F,model=T)
  class(fitorig1) <- c("NelsonSiegelSvenssonAdjust","nls")
  return(fitorig1)
}





print.NelsonSiegelSvenssonAdjust <- function(obj){
  transformnssa <- function(obj){
    cc <- coef(obj)
    tt <- cc
    tt[1] <- exp(cc[1]); tt[2] <- exp(cc[2])-exp(cc[1])
    tt[5] <- exp(cc[5]); tt[6] <- exp(cc[6])+exp(cc[5]);
    tt[7] <- exp(cc[8]); tt[8] <- -exp(cc[7])
    names(tt) <- c("beta1","beta2","beta3","beta4","lambda1", "lambda2","beta5", "beta6")
    return(tt)
  }
  x1 <- predict(obj, newdata=data.frame(Maturity=10, BBBm=0, BBB=0))[1]
  x2 <- predict(obj, newdata=data.frame(Maturity=10, BBBm=0, BBB=1))[1]
  x3 <- predict(obj, newdata=data.frame(Maturity=10, BBBm=1, BBB=0))[1]
  n2 <- sum(obj$model$BBB, na.rm=T)
  n3 <- sum(obj$model$BBBm, na.rm=T)
  n1 <- length(obj$model$BBB[!is.na(obj$model$BBB)])-n2-n3
  w1 <- n1/(n1+n2+n3)
  w2 <- n2/(n1+n2+n3)
  w3 <- n3/(n1+n2+n3)
  x <- w1*x1+w2*x2+w3*x3
  p <- getFromNamespace("print.nls","stats")(obj)
  r <- list(transformnssa(obj),x,((1+x/200)^2-1)*100)
  attributes(r) <- NULL

  names(r) <- c("Original parameters","Ten Year Yield (Semi-Annual Basis)",
                "Ten Year Yield (Annual Basis)")
  return(r)
}

summary.NelsonSiegelSvenssonAdjust <- function(obj){
  transformnssa <- function(obj){
    cc <- coef(obj)
    tt <- cc
    tt[1] <- exp(cc[1]); tt[2] <- exp(cc[2])-exp(cc[1])
    tt[5] <- exp(cc[5]); tt[6] <- exp(cc[6])+exp(cc[5]);
    tt[7] <- exp(cc[8]); tt[8] <- -exp(cc[7])
    names(tt) <- c("beta1","beta2","beta3","beta4","lambda1", "lambda2","beta5", "beta6")
    return(tt)
  }
  x1 <- predict(obj, newdata=data.frame(Maturity=10, BBBm=0, BBB=0))[1]
  x2 <- predict(obj, newdata=data.frame(Maturity=10, BBBm=0, BBB=1))[1]
  x3 <- predict(obj, newdata=data.frame(Maturity=10, BBBm=1, BBB=0))[1]
  n2 <- sum(obj$model$BBB, na.rm=T)
  n3 <- sum(obj$model$BBBm, na.rm=T)
  n1 <- length(obj$model$BBB[!is.na(obj$model$BBB)])-n2-n3
  w1 <- n1/(n1+n2+n3)
  w2 <- n2/(n1+n2+n3)
  w3 <- n3/(n1+n2+n3)
  x <- w1*x1+w2*x2+w3*x3
  r <- list(getFromNamespace("summary.nls","stats")(obj),transformnssa(obj),x,((1+x/200)^2-1)*100)
  attributes(r) <- NULL
  names(r) <- c("Fitted Nelson-Siegel-Svensson Model","Original Parameters","Ten Year Yield (Semi-Annual Basis)",
                "Ten Year Yield (Annual Basis)")
  return(r)
}

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


drp <- function(GSmodel10, NSmodel, NSSmodel, avspotratesa){
  cod10 <- round(  (summary(GSmodel10)[[5]]+
                      100*((1+predict(NSmodel,newdata=data.frame(Maturity=10))/200)^2-1)+
                      100*((1+predict(NSSmodel,newdata=data.frame(Maturity=10))/200)^2-1 ))/3,4)
  avswaprateann <- round(((1+avspotratesa/200)^2-1)*100,4)
  results <- list(avspotratesa, avswaprateann, cod10, cod10-avswaprateann)
  names(results) <- c("Average swap rate (semi-annual basis)",
                      "Annualized Swap average (annual basis)",
                      "10 year final cost of debt estimate",
                      "10 year DRP")
  return(results)
}


drpadjust <- function(GSadjustmodel10, NSadjustmodel, NSSadjustmodel, avspotratesa){
  adj <- sum(as.numeric(names(table(GSadjustmodel10[[6]])))*table(GSadjustmodel10[[6]]))/
    sum(as.numeric(table(GSadjustmodel10[[6]])))
  x1 <- predict(NSadjustmodel, newdata=data.frame(Maturity=10, BBBm=0, BBB=0))[1]
  x2 <- predict(NSadjustmodel, newdata=data.frame(Maturity=10, BBBm=0, BBB=1))[1]
  x3 <- predict(NSadjustmodel, newdata=data.frame(Maturity=10, BBBm=1, BBB=0))[1]
  n2 <- sum(NSadjustmodel$model$BBB, na.rm=T)
  n3 <- sum(NSadjustmodel$model$BBBm, na.rm=T)
  n1 <- length(NSadjustmodel$model$BBB[!is.na(NSadjustmodel$model$BBB)])-n2-n3
  w1 <- n1/(n1+n2+n3)
  w2 <- n2/(n1+n2+n3)
  w3 <- n3/(n1+n2+n3)
  x4 <- w1*x1+w2*x2+w3*x3

  xx1 <- predict(NSSadjustmodel, newdata=data.frame(Maturity=10, BBBm=0, BBB=0))[1]
  xx2 <- predict(NSSadjustmodel, newdata=data.frame(Maturity=10, BBBm=0, BBB=1))[1]
  xx3 <- predict(NSSadjustmodel, newdata=data.frame(Maturity=10, BBBm=1, BBB=0))[1]
  nn2 <- sum(NSSadjustmodel$model$BBB, na.rm=T)
  nn3 <- sum(NSSadjustmodel$model$BBBm, na.rm=T)
  nn1 <- length(NSSadjustmodel$model$BBB[!is.na(NSSadjustmodel$model$BBB)])-nn2-nn3
  ww1 <- nn1/(nn1+nn2+nn3)
  ww2 <- nn2/(nn1+nn2+nn3)
  ww3 <- nn3/(nn1+nn2+nn3)
  xx4 <- ww1*xx1+ww2*xx2+ww3*xx3

  cod10 <- round(  (summary(GSmodel10)[[5]]+adj+
                      100*((1+x4/200)^2-1)+
                      100*((1+xx4/200)^2-1))/3,4)
  avswaprateann <- round(((1+avspotratesa/200)^2-1)*100,4)
  results <- list(avspotratesa, avswaprateann, cod10, cod10-avswaprateann)
  names(results) <- c("Average swap rate (semi-annual basis)",
                      "Annualized Swap average (annual basis)",
                      "10 year final cost of debt estimate",
                      "10 year DRP")
  return(results)
}

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
