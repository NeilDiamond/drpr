#' Nelson-Siegel model
#
#' Nelson-Siegel model with Diebold and Li Parameterization
#
#' @param maturity maturity in years
#' @param lambda decay factor
#' @param beta1 long term factor
#' @param beta2 short term factor
#' @param beta3 curvature factor
#' @return Nelson-Siegel function.
#' @export
nelsonsiegelorig <- function(maturity, lambda, beta1, beta2, beta3){
  return(beta1 + beta2 * (1-exp(-lambda*maturity)) / (lambda*maturity)+
           beta3 * ((1-exp(-lambda*maturity)) / (lambda*maturity)-exp(-lambda*maturity)))
}
