#' Bivariate PDF estimation of copula - plot
#'
#' @param RV Matrix: Realizations of two random variables.
#' @param grid_size Scalar : The size of the grid on which the plot will be made.
#' @param sqrt if sqrt = TRUE copula estimate is shown on the square root scale. It helps to have a better look at some of the details in the PDF. However, the estimate is misleading.
#'
#' @return Returns the PDF estimate across the grid.
#' @rawNamespace import(plotly, except = filter)
#' @importFrom copula pobs
#' @export
#'
#' @examples OBJ = copula.pdf.plot(RV = cbind(rnorm(1000),rnorm(1000)), grid_size = 100)
#' OBJ$Plot3D
#' OBJ$Contour
copula.pdf.plot <- function(RV, grid_size, sqrt = FALSE){
  copula.pdf <- function(RV, q){
    unicdf <- function(RVa, R){
      T = length(RVa)
      hx =(1.06*sd(RVa)*T^(-1/5))
      mean(pnorm((RVa-R)/hx, lower.tail = F))
    }
    g<-function(x){unicdf(RV[,1],R=x)-q[1]}
    q[1]=uniroot(g, interval = c(min(RV[,1])-sd(RV[,1])*100,max(RV[,1])+sd(RV[,1])*100))$root
    g<-function(x){unicdf(RV[,2],R=x)-q[2]}
    q[2]=uniroot(g, interval = c(min(RV[,2])-sd(RV[,2])*100,max(RV[,2])+sd(RV[,2])*100))$root
    ##################
    if(is.na(bi.pdf(RV,q)/(un.pdf(RV[,1],q[1])*un.pdf(RV[,2],q[2]))))
    {stop("Quantiles must belong to ]0,1[")}else{return(bi.pdf(RV,q)/(un.pdf(RV[,1],q[1])*un.pdf(RV[,2],q[2])))}}

  Xg = seq(min(pobs(RV)[,1]), max(pobs(RV)[,1]), (max(pobs(RV)[,1])-min(pobs(RV)[,1]))/(grid_size-1))
  Yg = seq(min(pobs(RV)[,2]), max(pobs(RV)[,2]), (max(pobs(RV)[,2])-min(pobs(RV)[,2]))/(grid_size-1))
  Output = matrix(apply(expand.grid(Xg,Yg),
                        1, copula.pdf, RV = RV), nrow = grid_size, ncol = grid_size)
  if(sqrt){Output = sqrt(Output)}
  return(list(X.values = Xg, Y.values = Yg, Z.coordinates = Output,   Plot3D = plot_ly(x = Xg, y = Yg, z = Output)
              %>% add_surface(contours = list(z = list(show=T, usecolormap=T, highlightcolor="#ff0000" ,project=list(z=T))))
              %>% layout(
                scene = list(
                  camera=list(
                    eye = list(x=-1, y=-2, z=2)
                  )
                )
              ), Contour = plot_ly(x = Yg, y = Xg, z = Output, type = "contour")))
}
