#' Univariate PDF estimation
#'
#' @param RVa Vector: Realizations of a random variable
#' @param q Scalar: Quantile
#'
#' @return Returns the PDF estimate for the quantile of interest.
#' @import stats
#' @export
#'
#' @examples un.pdf(RVa = rnorm(10000), q = 0)
un.pdf <- function(RVa, q){
  T = length(RVa)
  hx =(1.06*sd(RVa)*T^(-1/5))
  (hx^(-1))*mean(dnorm((RVa-q)/hx))
}
