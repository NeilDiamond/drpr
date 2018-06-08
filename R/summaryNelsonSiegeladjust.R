#'summary.NelsonSiegeladjust
#'
#'@description summary method for class "NelsonSiegelAdjust"
#'
#'@param obj an object of class "NelsonSiegelAdjust"
#'@return list consisting of Fitted Nelson-Siegel model, ten year yield (semi-annual basis),  and ten year yield (annual basis)
#'@export

summary.NelsonSiegelAdjust <- function(obj){
  transformnsa <- function(obj){
    cc <- coef(obj)
    tt <- cc
    tt[1] <- exp(cc[1]); tt[2] <- exp(cc[2])-exp(cc[1])
    tt[4] <- exp(cc[4]); tt[5] <- exp(cc[6]); tt[6] <- -exp(cc[5])
    names(tt) <- c("beta1","beta2","beta3","lambda", "beta5", "beta6")
    return(tt)
  }
  x1 <- predict(obj, newdata=data.frame(Maturity=10, BBBm=0, BBB=0))[1]
  x2 <- predict(obj, newdata=data.frame(Maturity=10, BBBm=0, BBB=1))[1]
  x3 <- predict(obj, newdata=data.frame(Maturity=10, BBBm=1, BBB=0))[1]
  n2 <- sum(obj$model$BBB, na.rm=T)
  n3 <- sum(obj$model$BBBm, na.rm=T)
  n1 <- length(obj$model$BBB[!is.na(obj$model$BBB)])-n2-n3
  w1 <- n1/(n1+n2+n3)
  w2 <- n2/(n1+n2+n3)
  w3 <- n3/(n1+n2+n3)
  x <- w1*x1+w2*x2+w3*x3
  r <- list(getFromNamespace("summary.nls","stats")(obj),transformnsa(obj),x,((1+x/200)^2-1)*100)
  attributes(r) <- NULL
  names(r) <- c("Fitted Nelson-Siegel Model","Original Parameters","Ten Year Yield (Semi-Annual Basis)",
                "Ten Year Yield (Annual Basis)")
  return(r)
}


