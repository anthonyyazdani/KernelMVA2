#' Spearman's rho based on copula CDF estimation
#'
#' @param RV Matrix: Realizations of two random variables.
#'
#' @return Returns the Spearman's rho based on copula CDF estimation.
#' @import pracma
#' @import stats
#' @export
#'
#' @examples spearmans.rho(RV = cbind(rnorm(1000),rnorm(1000)))
spearmans.rho <- function(RV){
  f <- function(x,y){copula.cdf(RV,c(x,y))}
  Int <- integral2(f, xmin = 0, xmax = 1, ymin = 0, ymax = 1, vectorized = F)
  Copula.rho = (12*Int$Q-3)
  return(Copula.rho)
}
