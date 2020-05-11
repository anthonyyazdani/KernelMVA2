#' Bivariate PDF estimation - plot
#'
#' @param RV Matrix: Realizations of two random variables.
#' @param grid_size Scalar : The size of the grid on which the plot will be made.
#'
#' @return Returns the PDF estimate across the grid.
#' @rawNamespace import(plotly, except = filter)
#' @export
#'
#' @examples OBJ = bi.pdf.plot(RV = cbind(rnorm(1000),rnorm(1000)), grid_size = 100)
#' OBJ$Plot3D
#' OBJ$Contour
bi.pdf.plot <- function(RV, grid_size){
  Xg = seq(min(RV[,1]), max(RV[,1]), (max(RV[,1])-min(RV[,1]))/(grid_size-1))
  Yg = seq(min(RV[,2]), max(RV[,2]), (max(RV[,2])-min(RV[,2]))/(grid_size-1))
  Output = matrix(apply(expand.grid(Xg,Yg),
                        1, bi.pdf, RV = RV), nrow = grid_size, ncol = grid_size)
  return(list(X.values = Xg, Y.values = Yg, Z.coordinates = Output, Plot3D = plot_ly(x = Yg, y = Xg, z = Output)
              %>% add_surface(contours = list(z = list(show=T, usecolormap=T, highlightcolor="#ff0000" ,project=list(z=T)))), Contour = plot_ly(x = Yg, y = Xg, z = Output, type = "contour")))
}
