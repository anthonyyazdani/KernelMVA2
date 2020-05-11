#' Gini's gamma based on copula CDF estimation
#'
#' @param RV Matrix: Realizations of two random variables.
#'
#' @return Returns the Gini's gamma based on copula CDF estimation.
#' @import stats
#' @export
#'
#' @examples ginis.gamma(RV = cbind(rnorm(1000),rnorm(1000)))
ginis.gamma <- function(RV){
  f <- function(x){copula.cdf(RV,c(x,(1-x)))+copula.cdf(RV,c(x,x))}
  Int <- integrate(Vectorize(f), lower = 0, upper = 1)
  Copula.gamma = (4*Int$value-2)
  return(Copula.gamma)
}
