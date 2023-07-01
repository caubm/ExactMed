#' @title Data for Examples (Categorical Mediator)
#'
#' @description Simulated data set containing 1000 observations on 5 measured variables with no missing values.
#'     The first three variables are the binary exposure, the categorical mediator and the binary outcome,
#'     respectively, while the last two variables are the potential adjustment covariates (one binary and one continuous).
#'
#' @docType data
#'
#' @usage data(datamed_cat)
#'
#' @format A data frame with 1000 rows and 5 variables:
#' \describe{
#'   \item{X}{exposure, binary variable}
#'   \item{M}{mediator, categorical variable}
#'   \item{Y}{outcome, binary variable}
#'   \item{C1}{first covariate, binary variable}
#'   \item{C2}{second covariate, continuous variable}
#' }
#' @keywords datasets
"datamed_cat"
