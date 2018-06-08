#'print.NelsonSiegelSvensson
#'
#'@description print method for class "NelsonSiegelSvensson"
#'
#'@param obj an object of class "NelsonSiegelSvensson"
#'@return list consisting of ten year yield (semi-annual basis) and ten year yield (annual basis)
#'@export
#'

print.NelsonSiegelSvensson <- function(obj){
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
  x <- predict(obj, newdata=data.frame(Maturity=10))[1]
  p <- getFromNamespace("print.nls","stats")(obj)
  r <- list(transformnss(obj),x,((1+x/200)^2-1)*100)
  attributes(r) <- NULL

  names(r) <- c("Original Parameters","Ten Year Yield (Semi-Annual Basis)",
                "Ten Year Yield (Annual Basis)")
  return(r)
}


