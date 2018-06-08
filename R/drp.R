#'drp
#'
#'@description Determine Debt Risk Premium
#'
#'@param GSmodel10 a GaussianSmooth object
#'@param NSmodel a NelsonSiegel object
#'@param NSSmodel a NelsonSiegelSvensson object
#'@param avspotratesa average swap rates (semi-annual basis)
#'@return list consisting of Average Swap rate (semi-annual basis), Annualized Swap average, 10 year final cost of debt estimate","10 year DRP"
#'@export
#'


drp <- function(GSmodel10, NSmodel, NSSmodel, avspotratesa){
  cod10 <- round( (summary(GSmodel10)[[5]]+
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



