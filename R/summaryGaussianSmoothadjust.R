#'summary.GaussianSmoothadjust
#'
#'@description summary method for class "GaussianSmoothAdjust"
#'
#'@param obj an object of class "GaussianSmoothAdjust"
#'@return list consisting of Annualized Yields at 3, 5, 7, 10 years term to maturity; Interpolated 7 year Yield (Semi-annual basis), Extrapolated 10 year Yield (Semi-annual basis), Interpolated 7 year Yield (Annualized Basis), and Extrapolated 10 year Yield (Annualized Basis))
#'@export
#'

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
