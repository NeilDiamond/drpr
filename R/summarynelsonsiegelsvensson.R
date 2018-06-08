#'summary.NelsonSiegelSvensson
#'
#'@description summary method for class "NelsonSiegelSvensson"
#'
#'@param obj an object of class "NelsonSiegel"
#'@return list consisting of inverse decay factors, fitted Nelson-Siegel-Svensson model, ten year yield (semi-annual basis), and ten year yield (annual basis)
#'@export
#'

summary.NelsonSiegelSvensson <- function(obj){
  transformnss <- function(obj){
    cc <- coef(obj)
    tt <- cc
    if(length(tt)==6){
      tt[1] <- exp(cc[1]); tt[2] <- exp(cc[2])-exp(cc[1])
      tt[5] <- exp(cc[5]); tt[6] <- exp(cc[5])+exp(cc[6])
      names(tt) <- c("beta1","beta2","beta3","beta4","lambda1","lambda2")
    } else {
      tt[1] <- exp(cc[1]); tt[2] <- exp(cc[2])-exp(cc[1])
      names(tt) <- c("beta1","beta2","beta3","beta4")
    }

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
