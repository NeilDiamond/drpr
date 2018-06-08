#'drpadjust
#'
#'@description Determine Debt Risk Premium adjusted for Credit ratings
#'
#'@param data Data file consisting of at least 4 columns: Maturity, Amount.issued, Yield, and Credit.rating
#'@param GSmodel10 a GaussianSmooth object
#'@param NSmodel a NelsonSiegel object
#'@param NSSmodel a NelsonSiegelSvensson object
#'@param avspotratesa average swap rates (semi-annual basis)
#'@return list consisting of Average Swap rate (semi-annual basis), Annualized Swap average, 10 year final cost of debt estimate","10 year DRP"
#'@export
#'


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

  cod10 <- round(  (summary(GSadjustmodel10)[[5]]+adj+
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


