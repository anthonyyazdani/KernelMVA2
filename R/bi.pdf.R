#' Bivariate PDF estimation
#'
#' @param RV Matrix: Realizations of two random variables
#' @param q Vector: Quantiles
#'
#' @return Returns the PDF estimate for the quantiles of interest.
#' @import stats
#' @export
#'
#' @examples bi.pdf(RV = cbind(rnorm(1000),rnorm(1000)), q = c(0,0))
bi.pdf <- function(RV, q){
  T = length(RV[,1])
  hx =(1.06*sd(RV[,1])*T^(-1/5))
  hy = (1.06*sd(RV[,2])*T^(-1/5))
  (1/(T*hx*hy))*(t(dnorm((RV[,1]-q[1])/hx))%*%dnorm((RV[,2]-q[2])/hy))
}
