% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spearmans.rho.R
\name{spearmans.rho}
\alias{spearmans.rho}
\title{Spearman's rho based on copula CDF estimation}
\usage{
spearmans.rho(RV)
}
\arguments{
\item{RV}{Matrix: Realizations of two random variables.}
}
\value{
Returns the Spearman's rho based on copula CDF estimation.
}
\description{
Spearman's rho based on copula CDF estimation
}
\examples{
spearmans.rho(RV = cbind(rnorm(1000),rnorm(1000)))
}
