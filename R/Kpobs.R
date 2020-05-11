#' Kernel Pseudo-Observations
#'
#' @param X n x d-matrix (or d-vector) of random variates to be converted to pseudo-observations.
#'
#' @return Compute the Kernel pseudo-observations for the given data matrix.
#' @import stats
#' @export
#'
#' @examples Kpobs(X = rnorm(10000))
Kpobs = function(X){
  X = as.data.frame(X)
  Output = matrix(NA, ncol = ncol(X), nrow = nrow(X))
  T = dim(X)[1]
  for (i in 1:dim(X)[2]){
    if(is.numeric(X[,i])){
      hx =(1.06*sd(X[,i])*T^(-1/5))
      for (j in 1:dim(X)[1]){
        Output[j,i] = mean(pnorm((X[,i]-X[j,i])/hx, lower.tail = F))
      }
    }
  }
  Output = as.matrix(Output)
  return(Output)
}
