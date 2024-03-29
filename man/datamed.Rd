% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datamed.R
\docType{data}
\name{datamed}
\alias{datamed}
\title{Data for Examples (Binary Mediator)}
\format{
A data frame with 1000 rows and 5 variables:
\describe{
  \item{X}{exposure, binary variable}
  \item{M}{mediator, binary variable}
  \item{Y}{outcome, binary variable}
  \item{C1}{first covariate, binary variable}
  \item{C2}{second covariate, continuous variable}
}
}
\usage{
data(datamed)
}
\description{
Simulated data set containing 1000 observations on 5 measured variables with no missing values.
    The first three variables are the binary exposure, mediator and outcome, respectively, while the last
    two variables are the potential adjustment covariates (one binary and one continuous).
}
\keyword{datasets}
