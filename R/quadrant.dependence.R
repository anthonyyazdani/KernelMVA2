#' Quadrant dependence based on copula CDF estimation
#'
#' @param RV Matrix: Realizations of two random variables.
#' @param q Vector: Quantiles belonging to (0,1)
#'
#' @return Returns the Quadrant dependence based on copula CDF estimation.
#' @export
#'
#' @examples quadrant.dependence(RV = cbind(rnorm(1000),rnorm(1000)), q = c(0.1,0.1))
quadrant.dependence <- function(RV, q){
  QD = copula.cdf(RV,c(q[1],q[2]))-q[1]*q[2]
  if(QD>=0){print(noquote(paste("We observe a local PQD of",round(QD,digits = 10),"for u =",q[1],"and v =",q[2])))}
  if(QD<=0){print(noquote(paste("We observe a local NQD of",round(QD,digits = 10),"for u =",q[1],"and v =",q[2])))}
  return(as.numeric(QD))
}
