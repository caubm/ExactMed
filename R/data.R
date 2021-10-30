#' @title Data for Examples
#'
#' @description Simulated data containing 1000 rows and 5 columns with no missing values.
#'     The binary mediator was generated according to a logistic regression model
#'     with the exposure and two covariates as main effect terms. The binary outcome was generated using
#'     a logistic regression model including exposure, mediator, exposure-mediator interaction
#'     and covariates terms.
#'
#' @docType data
#'
#' @usage data(datamed)
#'
#' @format A data frame with 1000 rows and 5 variables:
#' \describe{
#'   \item{X}{exposure, binary variable}
#'
#'   \item{C1}{first covariate, binary variable}
#'   \item{C2}{second covariate, continuous variable}
#'   \item{M}{mediator, binary variable}
#'   \item{Y}{outcome, binary variable}
#' }
#' @keywords datasets
"datamed"
