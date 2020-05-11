#' Univariate PDF estimation - plot
#'
#' @param RVa Vector: Realizations of a random variable
#' @param grid_size Scalar : The size of the grid on which the plot will be made.
#'
#' @return Returns the PDF estimate across the grid.
#' @rawNamespace import(plotly, except = filter)
#' @export
#'
#' @examples OBJ = un.pdf.plot(RVa = rnorm(10000), grid_size = 1000)
#' OBJ$Plot
un.pdf.plot <- function(RVa, grid_size){
  Grid = seq(min(RVa), max(RVa), (max(RVa)-min(RVa))/(grid_size-1))
  Output = sapply(Grid, un.pdf, RVa = RVa)
  axx <- list(title = "x"); axy <- list(title = "f(x)"); fig = plot_ly(x = Grid, y = Output, type = 'scatter', mode = 'lines') %>% layout(title = "Kernel PDF estimation", xaxis=axx,yaxis=axy, showlegend = FALSE) %>% add_trace(x = RVa, y = rep(0,length(RVa)), name = 'trace 2', mode = 'markers', marker = list(color = 'rgb(0,0,0)'))
  list("x.values" = Grid, "f(x).coordinates" = Output, Plot = fig)
}
