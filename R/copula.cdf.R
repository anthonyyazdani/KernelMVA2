#' Bivariate CDF estimation of copula
#'
#' @param RV Matrix: Realizations of two random variables.
#' @param q Vector: Quantiles belonging to (0,1)
#'
#' @return Returns the CDF estimate for the quantiles of interest.
#' @import stats
#' @export
#'
#' @examples copula.cdf(RV = cbind(rnorm(1000),rnorm(1000)), q = c(0.5,0.5))
copula.cdf <- function(RV, q){
  #### Defining univariate CDF function ####
  unicdf <- function(RVa, R){
    T = length(RVa)
    hx =(1.06*sd(RVa)*T^(-1/5))
    mean(pnorm((RVa-R)/hx, lower.tail = F))
  }
  ##########################################

  #### Optimize ####
  g<-function(x){unicdf(RV[,1],R=x)-q[1]}
  q[1]=uniroot(g, interval = c(min(RV[,1])-sd(RV[,1])*100,max(RV[,1])+sd(RV[,1])*100))$root
  g<-function(x){unicdf(RV[,2],R=x)-q[2]}
  q[2]=uniroot(g, interval = c(min(RV[,2])-sd(RV[,2])*100,max(RV[,2])+sd(RV[,2])*100))$root
  ##################

  T = length(RV[,1])
  hx =(1.06*sd(RV[,1])*T^(-1/5))
  hy = (1.06*sd(RV[,2])*T^(-1/5))
  (1/T)*(t(pnorm((RV[,1]-q[1])/hx, lower.tail = F))%*%pnorm((RV[,2]-q[2])/hy, lower.tail = F))
}
