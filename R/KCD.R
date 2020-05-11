#' Kernel Copula Dependance
#'
#' @param RV Matrix: Realizations of two random variables.
#'
#' @return Returns the Kernel Copula Dependance based on copula CDF estimation.
#' @import pracma
#' @import stats
#' @export
#'
#' @examples KCD(RV = cbind(rnorm(1000),rnorm(1000)))
KCD <- function (RV){
  COMO <- cbind(seq(0,1,(1/(dim(RV)[1]-1))),seq(0,1,(1/(dim(RV)[1]-1))))
  COUNTER <- cbind(seq(0,1,(1/(dim(RV)[1]-1))),-seq(0,1,(1/(dim(RV)[1]-1))))

  f <- function(x, y){
    copula.cdf(COMO, c(x, y))-copula.cdf(RV, c(x, y))
  }
  IntRV <- integral2(f, xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                     vectorized = F)
  DRV = IntRV$Q

  g <- function(x, y){
    copula.cdf(COMO, c(x, y))-copula.cdf(COUNTER, c(x, y))
  }
  IntCOUNTER <- integral2(g, xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                          vectorized = F)
  DCOUNTER = IntCOUNTER$Q
  Output = 1-2*(DRV/DCOUNTER)
  return(Output)
}
