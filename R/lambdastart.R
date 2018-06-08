#' Determining starting value for decay factor
#
#' Determining starting value for decay factor with Diebold and Li Parameterization
#
#' @param maturity maturity at which second loading is maximized (in years).
#' @return lambda maximizing second loading, to be used as starting value.
#' @export
lambdastart <- function(maturity=2.5){
  beta2 <- function(lam){
    ((1-exp(-maturity*lam))/(maturity*lam)-exp(-maturity*lam))
  }
  optimize(f=beta2, interval=c(10^(-6),10^(6),1000), maximum=T)$maximum
}
