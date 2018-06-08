#'gswt1adjust
#'
#'@description Local Constant Smoothing adjusted for Credit ratings
#'
#'@param tt Target Tenor
#'@param data Data file consisting of at least 3 columns: Maturity, Amount.issued, and Yield
#'@param maturity Maturity
#'@param issue Amount issued
#'@param yield Yield
#'@param sigma Smoothing Constant
#'@return a list of class "GaussianSmoothAdjust"
#'@export
#'
#'



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









