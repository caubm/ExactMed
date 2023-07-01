#' @noRd

.check_input_param_cat <- function(data, a, m, y, a1, a0, m_cov, y_cov, m_cov_cond,
                                y_cov_cond, adjusted, interaction, Firth, boot, nboot, bootseed,
                                confcoef, blevel_m, hvalue_y, yprevalence, mf) {
  if (!(is.data.frame(data) && !is.null(colnames(data)))) stop("'data' must be a data frame with column names")

  if (any(duplicated(colnames(data)))) stop("'data' has duplicated column names")

  if (any(is.na(colnames(data))) || any(colnames(data) == "")) stop("'data' has some unnamed columns")

  if (!(is.vector(a, mode = "character") && length(a) == 1L && a %in% colnames(data))) {
    stop("'a' has to be a column name of 'data'")
  }

  if (!(is.vector(m, mode = "character") && length(m) == 1L && m %in% colnames(data))) {
    stop("'m' has to be a column name of 'data'")
  }

  if (!(is.vector(y, mode = "character") && length(y) == 1L && y %in% colnames(data))) {
    stop("'y' has to be a column name of 'data'")
  }

  if (!(is.vector(a1, mode = "numeric") && length(a1) == 1L)) stop("'a1' has to be a real number")

  if (!(is.vector(a0, mode = "numeric") && length(a0) == 1L)) stop("'a0' has to be a real number")

  if( a0 >=  a1) {
    warning("The value of the low level of exposure is not smaller than that of the high level")
  }

  if (!(is.vector(confcoef, mode = "numeric") && length(confcoef) == 1L && 0 < confcoef && confcoef < 1)) {
    stop("'confcoef' has to be a valid real number")
  }

  if (!(is.vector(boot, mode = "logical") && length(boot) == 1L) || is.na(boot)) {
    stop("'boot' must specify a logical value")
  }

  if (boot == TRUE) {
    if (!(is.vector(nboot, mode = "numeric") && length(nboot) == 1L && round(nboot) == nboot)) {
      stop("'nboot' has to be an integer")
    }

    if (!(is.vector(bootseed, mode = "numeric") && length(bootseed) == 1L && round(bootseed) == bootseed)) {
      stop("'bootseed' has to be an integer")
    }
  }

  if (!(is.vector(interaction, mode = "logical") && length(interaction) == 1L) || is.na(interaction)) {
    stop("'interaction' must specify a logical value")
  }

  if (!(is.vector(Firth, mode = "logical") && length(Firth) == 1L) || is.na(Firth)) {
    stop("'Firth' must specify a logical value")
  }

  if (!(is.vector(adjusted, mode = "logical") && length(adjusted) == 1L) || is.na(adjusted)) {
    stop("'adjusted' must specify a logical value")
  }

  if (adjusted == TRUE && is.null(m_cov) && is.null(y_cov)) {
    message("'exactmed' will compute unadjusted natural effects")
  }

  if (adjusted == FALSE && !(is.null(m_cov) && is.null(y_cov))) {
    message("'exactmed' will compute unadjusted natural effects")
  }

  if (!(is.null(m_cov) || is.vector(m_cov, mode = "character"))) {
    stop("'m_cov' must be NULL or a vector of covariate names")
  }

  if (any(is.na(m_cov))) stop("'m_cov' has NAs")

  if (any(duplicated(m_cov))) stop("'m_cov' has duplicated covariates names")

  if (!all(m_cov %in% setdiff(colnames(data), c(a, m, y)))) {
    stop("'m_cov' can only contain names of covariates included in the data frame")
  }

  if (!(is.null(y_cov) || is.vector(y_cov, mode = "character"))) {
    stop("'y_cov' must be NULL or a vector of covariate names")
  }

  if (any(is.na(y_cov))) stop("'y_cov' has NAs")

  if (any(duplicated(y_cov))) stop("'y_cov' has duplicated covariates names")

  if (!all(y_cov %in% setdiff(colnames(data), c(a, m, y)))) {
    stop("'y_cov' can only contain names of covariates included in the data frame")
  }

  if (!(is.null(m_cov_cond) || is.vector(m_cov_cond))) {
    stop("'m_cov_cond' must be NULL or a vector")
  }

  if (any(is.na(names(m_cov_cond))) || any(names(m_cov_cond) == "")) {
    stop("'m_cov_cond' has missing names")
  }

  if (any(duplicated(names(m_cov_cond)))) stop("'m_cov_cond' has duplicated names")

  if (!all(names(m_cov_cond) %in% m_cov)) {
    stop("The names of the elements of 'm_cov_cond' must be in 'm_cov'")
  }

  if (!(is.null(y_cov_cond) || is.vector(y_cov_cond))) {
    stop("'y_cov_cond' must be NULL or a vector")
  }

  if (any(is.na(names(y_cov_cond))) || any(names(y_cov_cond) == "")) {
    stop("'y_cov_cond' has missing names")
  }

  if (any(duplicated(names(y_cov_cond)))) stop("'y_cov_cond' has duplicated names")

  if (!all(names(y_cov_cond) %in% y_cov)) {
    stop("The names of the elements of 'y_cov_cond' must be in 'y_cov'")
  }

  if (!is.null(m_cov_cond)) {
    if (is.null(names(m_cov_cond))) stop("'m_cov_cond' must be a named vector")

    for (i in names(m_cov_cond)) {
      if (!(is.atomic(m_cov_cond[[i]]) && length(m_cov_cond[[i]]) == 1L && is.null(dim(m_cov_cond[[i]])) && !is.na(m_cov_cond[[i]]))) {
        stop("'m_cov_cond' has a invalid value in the ", i, " component")
      }
    }
  }

  if (!is.null(y_cov_cond)) {
    if (is.null(names(y_cov_cond))) stop("'y_cov_cond' must be a named vector")

    for (i in names(y_cov_cond)) {
      if (!(is.atomic(y_cov_cond[[i]]) && length(y_cov_cond[[i]]) == 1L && is.null(dim(y_cov_cond[[i]])) && !is.na(y_cov_cond[[i]]))) {
        stop("'y_cov_cond' has a invalid value in the ", i, " component")
      }
    }
  }

  if (!is.null(m_cov_cond)) {
    for (i in names(m_cov_cond)) {
      if (i %in% y_cov) {
        if (i %in% names(y_cov_cond)) {
          if (m_cov_cond[[i]] != y_cov_cond[[i]]) {
            stop("Covariate ", i, " has two different values specified")
          }
        } else {
          stop("Covariate ", i, " has two different values specified (one implicitly)")
        }
      }
    }
  }

  if (!is.null(y_cov_cond)) {
    for (i in names(y_cov_cond)) {
      if (i %in% m_cov && !(i %in% names(m_cov_cond))) {
        stop("Covariate ", i, " has two different values specified (one implicitly)")
      }
    }
  }


  if (any(is.na(data[c(a, m, y, union(m_cov, y_cov))]))) {
    stop("'data' contains missing values")
  }

  if (!is.numeric(data[[a]])) stop("Exposure must be numerical variable")

  if (length(unique(data[[a]])) == 2) {
    if (!a0 %in% data[[a]] && a1 %in% data[[a]]) {
      warning("The low level of the exposure ('a0') is not an observed value")
    } else if (a0 %in% data[[a]] && !a1 %in% data[[a]]) {
      warning("The high level of the exposure ('a1') is not an observed value")
    } else if(!a0 %in% data[[a]] && !a1 %in% data[[a]]) {
      warning("The levels of the exposure ('a0' and 'a1') are not observed values")
    }
  }

  if (!(is.null(hvalue_y) || (is.atomic(hvalue_y) && length(hvalue_y) == 1L && is.null(dim(hvalue_y)) && !is.na(hvalue_y)))) {
    stop("Invalid type or length for input parameter 'hvalue_y'")
  }

  if (!is.factor(data[[m]])) stop("Mediator must be factor type variable")

  if (length(unique(data[[y]])) > 2) {
    stop("Outcome takes more than two different values in 'data'")
  }

  if (is.factor(data[[y]])) {
    if (is.null(hvalue_y)) stop("High level for the outcome must be specified")

    if (!hvalue_y %in% levels(data[[y]])) {
      stop("Invalid value for high level of outcome")
    }
  } else if (is.numeric(data[[y]]) && all(data[[y]] %in% c(1, 0))) {
    if (!(is.null(hvalue_y) || hvalue_y %in% data[[y]])) {
      stop("Invalid value for high level of outcome")
    }
  } else {
    if (is.null(hvalue_y)) stop("High level for the outcome must be specified")

    if (!hvalue_y %in% data[[y]]) {
      stop("Invalid value for high level of outcome")
    }
  }

  if (!(is.null(yprevalence) || (is.vector(yprevalence, mode = "numeric") && length(yprevalence) == 1L && 0 < yprevalence && yprevalence < 1))) {
    stop("'yprevalence' must be NULL or a valid real number")
  }


  if(!(is.null(blevel_m) || blevel_m %in% levels(data[[m]]))) {
    stop("Invalid value for 'blevel_m' parameter")
  }


  if (!(is.null(mf) || mf %in% levels(data[[m]]))) {
    stop("Invalid value for 'mf' parameter")
  }



}




