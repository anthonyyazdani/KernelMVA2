#' Bivariate PDF estimation of copula
#'
#' @param RV Matrix: Realizations of two random variables.
#' @param q Vector: Quantiles belonging to ]0,1[
#'
#' @return Returns the PDF estimate for the quantiles of interest.
#' @import stats
#' @export
#'
#' @examples copula.pdf(RV = cbind(rnorm(1000),rnorm(1000)), q = c(0.5,0.5))
copula.pdf <- function(RV, q){
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
  if(is.na(bi.pdf(RV,q)/(un.pdf(RV[,1],q[1])*un.pdf(RV[,2],q[2]))))
    {stop("Quantiles must belong to ]0,1[")}else{return(bi.pdf(RV,q)/(un.pdf(RV[,1],q[1])*un.pdf(RV[,2],q[2])))}
}
