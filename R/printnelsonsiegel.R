#'print.NelsonSiegel
#'
#'@description print method for class "NelsonSiegel"
#'
#'@param obj an object of class "NelsonSiegel"
#'@return list consisting of ten year yield (semi-annual basis) and ten year yield (annual basis)
#'@export
#'

print.NelsonSiegel <- function(obj){
  transformns <- function(obj){
    cc <- coef(obj)
    tt <- cc
    tt[1] <- exp(cc[1]); tt[2] <- exp(cc[2])-exp(cc[1])
    tt[4] <- exp(cc[4]);
    names(tt) <- c("beta1","beta2","beta3","lambda")
    return(tt)
  }

  x <- predict(obj, newdata=data.frame(Maturity=10))[1]
  p <- getFromNamespace("print.nls","stats")(obj)
  r <- list(transformns(obj),x,((1+x/200)^2-1)*100)
  attributes(r) <- NULL

  names(r) <- c("Original Parameters","Ten Year Yield (Semi-Annual Basis)",
                "Ten Year Yield (Annual Basis)")
  return(r)
}
