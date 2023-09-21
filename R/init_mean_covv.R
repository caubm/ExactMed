#' @noRd


.init_mean_covv <- function(data, my_cov, my_cov_cond) {
  mean_covv <- numeric(0)
  if (!is.null(my_cov)) {
    for (i in my_cov) {
      if (is.factor(data[[i]])) {
        if (i %in% names(my_cov_cond)) {
          if (!(my_cov_cond[[i]] %in% levels(data[[i]]))) {
            stop("Invalid value for ", i, " covariate.
                 Make sure that 'm_cov_cond' and 'y_cov_cond' are list-type vectors
                 if there is a mixture of numerical and categorical variables")
          }
          auxv <- vector("numeric", nlevels(data[[i]]) - 1L)
          auxl <- which(levels(data[[i]]) == my_cov_cond[[i]]) - 1
          if (auxl != 0L) {
            auxv[auxl] <- 1
          }
          mean_covv <- c(mean_covv, auxv)
        } else {
          freqcat <- summary(data[[i]])[-1] / nrow(data)
          names(freqcat) <- NULL
          mean_covv <- c(mean_covv, freqcat)
        }
      } else {
        if (i %in% names(my_cov_cond)) {
          if (!(is.vector(my_cov_cond[[i]], mode = "numeric") && length(my_cov_cond[[i]]) == 1L)) {
            stop("Invalid value for ", i, " covariate.
                 Make sure that 'm_cov_cond' and 'y_cov_cond' are list-type vectors
                 if there is a mixture of numerical and categorical variables")
          }
          mean_covv <- c(mean_covv, my_cov_cond[[i]])
        } else {
          mean_covv <- c(mean_covv, mean(data[[i]]))
        }
      }
    }
  }
  return(mean_covv)
}
