#' Bivariate CDF estimation of copula - plot
#'
#' @param RV Matrix: Realizations of two random variables.
#' @param grid_size Scalar : The size of the grid on which the plot will be made.
#'
#' @return Returns the CDF estimate across the grid.
#' @rawNamespace import(plotly, except = filter)
#' @importFrom copula pobs
#' @export
#'
#' @examples OBJ = copula.cdf.plot(RV = cbind(rnorm(1000),rnorm(1000)), grid_size = 100)
#' OBJ$Plot3D
#' OBJ$Contour
copula.cdf.plot <- function(RV, grid_size){
  Xg = seq(min(pobs(RV)[,1]), max(pobs(RV)[,1]), (max(pobs(RV)[,1])-min(pobs(RV)[,1]))/(grid_size-1))
  Yg = seq(min(pobs(RV)[,2]), max(pobs(RV)[,2]), (max(pobs(RV)[,2])-min(pobs(RV)[,2]))/(grid_size-1))
  Output = matrix(apply(expand.grid(Xg,Yg),
                        1, copula.cdf, RV = RV), nrow = grid_size, ncol = grid_size)
  return(list(X.values = Xg, Y.values = Yg, Z.coordinates = Output, Plot3D = plot_ly(x = Xg, y = Yg, z = Output)
              %>% add_surface(contours = list(z = list(show=T, usecolormap=T, highlightcolor="#ff0000" ,project=list(z=T)))), Contour = plot_ly(x = Yg, y = Xg, z = Output, type = "contour")))
}
