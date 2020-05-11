#' Blomqvist's beta based on copula CDF estimation
#'
#' @param RV Matrix: Realizations of two random variables.
#'
#' @return Returns the Blomqvist's beta based on copula CDF estimation.
#' @export
#'
#' @examples blomqvists.beta(RV = cbind(rnorm(1000),rnorm(1000)))
blomqvists.beta<-function(RV){
  return(as.numeric(4*copula.cdf(RV,c(0.5,0.5))-1))
}
