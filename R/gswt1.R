#'gswt1
#'
#'@description Local Constant Smoothing
#'
#'@param tt Target Tenor
#'@param data Data file consisting of at least 3 columns: Maturity, Amount.issued, and Yield
#'@param maturity Maturity
#'@param issue Amount issued (should be numeric)
#'@param yield Yield
#'@param sigma Smoothing Constant
#'@return a list of class "GaussianSmoother"
#'@export
#'




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












