#'gkest
#'
#'@description Interpolated and Extrapolated 7 and 10 year yields adjusting for Credit ratings
#'
#'@param sigma Smoothing Constant
#'@param filename Data file consisting of at least 3 columns: Maturity, Amount.issued, and Yield
#'@return list consisting of a matrix of Target Tenor, sigma, Target Tenor Yield, Effective Term to Maturity, and Annual Yield; Interpolated 7 year yield (Semi-annual basis); Extrapolated 10 year yield (Semi-annual basis); Interpolated 7 year yield (Annual basis); and Extrapolated 10 year yield (Annual basis)
#'@export
#'
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
