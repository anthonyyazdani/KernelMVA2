#' Univariate CDF estimation
#'
#' @param RVa Vector: Realizations of a random variable
#' @param q Scalar: Quantile
#'
#' @return Returns the CDF estimate for the quantile of interest.
#' @import stats
#' @export
#'
#' @examples un.cdf(RVa = rnorm(10000), q = 0)
un.cdf <- function(RVa, q){
  T = length(RVa)
  hx =(1.06*sd(RVa)*T^(-1/5))
  mean(pnorm((RVa-q)/hx, lower.tail = F))
}
