#' Univariate CDF estimation - plot
#'
#' @param RVa Vector: Realizations of a random variable
#' @param grid_size Scalar : The size of the grid on which the plot will be made.
#'
#' @return Returns the CDF estimate across the grid.
#' @rawNamespace import(plotly, except = filter)
#' @export
#'
#' @examples OBJ = un.cdf.plot(RVa = rnorm(10000), grid_size = 1000)
#' OBJ$Plot
un.cdf.plot <- function(RVa, grid_size){
  Grid = seq(min(RVa), max(RVa), (max(RVa)-min(RVa))/(grid_size-1))
  Output = sapply(Grid, un.cdf, RVa = RVa)
  axx <- list(title = "x"); axy <- list(title = "F(x)"); fig = plot_ly(x = Grid, y = Output, type = 'scatter', mode = 'lines') %>% layout(title = "Kernel CDF estimation", xaxis=axx,yaxis=axy, showlegend = FALSE) %>% add_trace(x = RVa, y = rep(0,length(RVa)), name = 'trace 2', mode = 'markers', marker = list(color = 'rgb(0,0,0)'))
  list("x.values" = Grid, "f(x).coordinates" = Output, Plot = fig)
}
