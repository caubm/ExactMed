#' @title  Exact Mediation Effects Computation
#' @description Relying on a regression-based approach, the \code{exactmed()} function calculates standard
#'     causal mediation effects when the outcome and the mediator are binary. More precisely, \code{exactmed()}
#'     uses a logistic regression specification for both the outcome and the mediator in order to compute \emph{exact}
#'     conditional natural direct and indirect effects (see details in Samoilenko and Lefebvre, 2021).
#'     The function returns point and interval estimates for the conditional natural effects without making any assumption
#'     regarding the rareness or commonness of the outcome (hence the term exact). For completeness, \code{exactmed()} also
#'     calculates the conditional controlled direct effects at both values of the mediator. Natural and controlled effects
#'     estimates are reported using three different scales: odds ratio (OR), risk ratio (RR) and risk difference (RD).
#'     The interval estimates can be obtained either by the delta method or the bootstrap.
#' @param data a named data frame that includes the exposure, mediator and outcome variables as well as the covariates
#'     to be adjusted for in the models. The exposure can be either binary or continuous. If a covariate is categorical,
#'     it has to be included in the data frame as a factor, character or logical variable.
#' @param a the name of the exposure variable.
#' @param m the name of the mediator variable.
#' @param y the name of the outcome variable.
#' @param a1 a value corresponding to the high level of the exposure.
#' @param a0 a value corresponding to the low level of the exposure.
#' @param m_cov a vector containing the names of the adjustment variables (covariates) in the mediator model.
#' @param y_cov a vector containing the names of the adjustment variables (covariates) in the outcome model.
#' @param m_cov_cond a named vector (atomic vector or list) containing specific values for some or all
#'     of the adjustment covariates \code{m_cov} in the mediator model. Please consult the package vignette for details.
#' @param y_cov_cond a named vector (atomic vector or list) containing specific values for some or all
#'     of the adjustment covariates \code{y_cov} in the outcome model. Please consult the package vignette for details.
#' @param adjusted  a logical variable specifying whether to obtain unadjusted or adjusted estimates.
#'     If \code{adjusted == FALSE}, vectors \code{m_cov} and \code{y_cov} are ignored by the procedure.
#' @param interaction a logical variable specifying whether there is an exposure-mediator interaction term in the outcome model.
#' @param Firth a logical variable specifying whether to compute conventional maximum likelihood estimates
#'     or Firth  penalized estimates in the logistic regression models.
#' @param boot a logical value specifying whether the confidence intervals are obtained
#'     by the delta method or by percentile bootstrap.
#' @param nboot   The number of bootstrap replications used to obtain the confidence intervals if \code{boot == TRUE}.
#' @param bootseed The value of the initial seed (positive integer) for random number generation if \code{boot == TRUE}.
#' @param confcoef a number between 0 and 1 for the confidence coefficient (ex:0.95) of the interval estimates.
#' @param hvalue_m the value corresponding to the high level of the mediator. If the mediator is already coded
#'     as a numerical binary variable taking 0 or 1 values, then by default \code{hvalue_m == 1}.
#' @param hvalue_y the value corresponding to the high level of the outcome. If the outcome is already coded
#'     as a numerical binary variable taking 0 or 1 values, then by default \code{hvalue_y == 1}.
#' @importFrom logistf logistf
#' @importFrom stats as.formula binomial glm qnorm quantile terms vcov na.omit pnorm sd
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @details By default, \code{exactmed()} reports mediation effects evaluated at the sample-specific mean values of the numerical covariates
#'     (including the dummy variables created internally by the function to represent the categorical covariates).
#'     In order to estimate mediation effects at specific values of some covariates (that is, stratum-specific effects),
#'     the user needs to provide named vectors \code{m_cov_cond} and/or \code{y_cov_cond} containing those values or levels. The adjustment
#'     covariates appearing in both \code{m_cov} and \code{y_cov} (common adjustment covariates) must have the same values; otherwise,
#'     \code{exactmed()}'s execution is aborted and an error message is displayed in the R console.
#' @return Returns natural direct, indirect and total effect estimates as well as controlled direct effects
#'     estimates on the OR, RR and RD scales.
#' @note \code{exactmed()} only works for complete data. Users can apply multiple imputation techniques (e.g., R package \emph{mice})
#'  or remove observations of variables used in mediation analysis that have missing values (NA).
#' @references
#' Samoilenko M, Lefebvre G. Parametric-Regression-Based Causal Mediation Analysis of Binary Outcomes and Binary Mediators:
#' Moving Beyond the Rareness or Commonness of the Outcome, \emph{American Journal of Epidemiology}.2021;190(9):1846-1858.
#'
#' Samoilenko M, Blais L, Lefebvre G. Comparing logistic and log-binomial models for causal mediation analyses of
#' binary mediators and rare binary outcomes: evidence to support cross-checking of mediation results in practice.
#' \emph{Observational Studies}.2018;4(1):193-216.
#' @export
#' @examples
#' exactmed(
#'   data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
#'   m_cov = c("C1", "C2"), y_cov = c("C1", "C2")
#' )
#'
#' m_cov_cond <- c(C1 = 0.1, C2 = 0.4)
#' y_cov_cond <- c(C1 = 0.1, C2 = 0.4)
#'
#' exactmed(
#'   data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
#'   m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
#'   m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
#' )
#'
#' C1b <- factor(sample(c("a", "b", "c"), nrow(datamed), replace = TRUE))
#' datamed$C1 <- C1b
#'
#' m_cov_cond <- list(C1 = "c", C2 = 0.4)
#' y_cov_cond <- list(C1 = "c", C2 = 0.4)
#'
#' exactmed(
#'   data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
#'   m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
#'   m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
#' )
exactmed <- function(data, a, m, y, a1, a0, m_cov = NULL, y_cov = NULL, m_cov_cond = NULL,
                     y_cov_cond = NULL, adjusted = TRUE, interaction = TRUE, Firth = FALSE,
                     boot = FALSE, nboot = 1000, bootseed = 1991, confcoef = 0.95,
                     hvalue_m = NULL, hvalue_y = NULL) {
  .check_input_param(
    data = data, a = a, m = m, y = y, a1 = a1, a0 = a0, m_cov = m_cov, y_cov = y_cov,
    m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond, adjusted = adjusted,
    interaction = interaction, Firth = Firth, boot = boot, nboot = nboot,
    bootseed = bootseed, confcoef = confcoef, hvalue_m = hvalue_m, hvalue_y = hvalue_y
  )


  if (!is.null(hvalue_m)) {
    if (is.factor(data[[m]])) {
      lv <- vector("integer", length = 2L)
      hl <- which(levels(data[[m]]) == hvalue_m)
      lv[hl] <- 1L
      levels(data[[m]]) <- lv
      data[[m]] <- as.integer(as.character(data[[m]]))
    } else {
      lv <- vector("integer", length = nrow(data))
      hl <- which(data[[m]] == hvalue_m)
      lv[hl] <- 1L
      data[[m]] <- lv
    }
  }


  if (!is.null(hvalue_y)) {
    if (is.factor(data[[y]])) {
      lv <- vector("integer", length = 2L)
      hl <- which(levels(data[[y]]) == hvalue_y)
      lv[hl] <- 1L
      levels(data[[y]]) <- lv
      data[[y]] <- as.integer(as.character(data[[y]]))
    } else {
      lv <- vector("integer", length = nrow(data))
      hl <- which(data[[y]] == hvalue_y)
      lv[hl] <- 1L
      data[[y]] <- lv
    }
  }

  for (i in setdiff(colnames(data), c(a, m, y))) {
    if (!(is.numeric(data[[i]]) || is.factor(data[[i]]))) {
      data[[i]] <- as.factor(data[[i]])
    }
  }

  expit <- function(x) {
    return(exp(x) / (1 + exp(x)))
  }

  if (!adjusted == TRUE) {
    m_cov <- NULL
    y_cov <- NULL
  }

  mean_covmv <- .init_mean_covv(data = data, my_cov = m_cov, my_cov_cond = m_cov_cond)
  mean_covyv <- .init_mean_covv(data = data, my_cov = y_cov, my_cov_cond = y_cov_cond)
  names(mean_covmv) <- NULL
  names(mean_covyv) <- NULL

  # Beta coefficients estimation (logistic regression model for binary mediator m)
  # and Theta coefficients estimation (logistic regression model for binary outcome y)

  Mform <- as.formula(paste(m, "~", paste(c(a, m_cov), collapse = " + ")))

  if (interaction == TRUE) {
    Yform <- as.formula(paste(y, "~", paste(c(paste(a, "*", m), y_cov), collapse = " + ")))
  } else {
    Yform <- as.formula(paste(y, "~", paste(c(a, m, y_cov), collapse = " + ")))
  }

  Yform <- terms(Yform, keep.order = TRUE)

  if (Firth == TRUE) {
    Mreg <- logistf(Mform, data = data)
    Yreg <- logistf(Yform, data = data)
  } else {
    Mreg <- glm(Mform, data = data, family = binomial(link = "logit"))
    Yreg <- glm(Yform, data = data, family = binomial(link = "logit"))
  }

  betacoef <- Mreg$coefficients
  thetacoef <- Yreg$coefficients

  names(betacoef) <- NULL
  names(thetacoef) <- NULL

  # Function 'gg' for Nested probabilities P(y(a,m(b)) =1|C=c) and gradient computation

  if (boot == FALSE) {
    gg <- function(a, b, betav, covmv, thetav, covyv, interaction) {
      if (interaction == TRUE) {
        probY1M1 <- expit(thetav[1] + thetav[3] +
          (thetav[2] + thetav[4]) * a + as.numeric(thetav[-(1:4)] %*% covyv))

        probY1M0 <- expit(thetav[1] + thetav[2] * a + as.numeric(thetav[-(1:4)] %*% covyv))
      } else {
        probY1M1 <- expit(thetav[1] + thetav[3] +
          thetav[2] * a + as.numeric(thetav[-(1:3)] %*% covyv))

        probY1M0 <- expit(thetav[1] + thetav[2] * a + as.numeric(thetav[-(1:3)] %*% covyv))
      }

      probM1 <- expit(betav[1] + betav[2] * b + as.numeric(betav[-(1:2)] %*% covmv))
      probM0 <- 1 - probM1

      Dgb0 <- probM1 * probM0 * (probY1M1 - probY1M0)
      Dgb1 <- b * Dgb0
      VDgb2 <- Dgb0 * covmv

      Dgt0 <- probY1M1 * (1 - probY1M1) * probM1 + probY1M0 * (1 - probY1M0) * probM0
      Dgt1 <- a * Dgt0
      Dgt2 <- probY1M1 * (1 - probY1M1) * probM1

      if (interaction == TRUE) {
        Dgt3 <- a * Dgt2
        Dgt4 <- Dgt0 * covyv
      } else {
        Dgt3 <- Dgt0 * covyv
        Dgt4 <- NULL
      }

      result <- vector("list", length = 2)
      result[[1]] <- probY1M1 * probM1 + probY1M0 * probM0
      result[[2]] <- c(Dgb0, Dgb1, VDgb2, Dgt0, Dgt1, Dgt2, Dgt3, Dgt4)

      return(result)
    }

    # Function 'gg_cde' for  probabilities P(y(a,m) =1|C=c) and gradient computation

    gg_cde <- function(a, m, thetav, covyv, interaction) {
      if (interaction == TRUE) {
        P <- expit(thetav[1] + thetav[2] * a + thetav[3] * m +
          thetav[4] * a * m + as.numeric(thetav[-(1:4)] %*% covyv))

        Dgt0 <- P * (1 - P)
        Dgt1 <- a * Dgt0
        Dgt2 <- m * Dgt0
        Dgt3 <- a * Dgt2
        Dgt4 <- Dgt0 * covyv
      } else {
        P <- expit(thetav[1] + thetav[2] * a + thetav[3] * m +
          as.numeric(thetav[-(1:3)] %*% covyv))

        Dgt0 <- P * (1 - P)
        Dgt1 <- a * Dgt0
        Dgt2 <- m * Dgt0
        Dgt3 <- Dgt0 * covyv
        Dgt4 <- NULL
      }

      result <- vector("list", length = 2)
      result[[1]] <- P
      result[[2]] <- c(Dgt0, Dgt1, Dgt2, Dgt3, Dgt4)

      return(result)
    }

    # Nested probabilities P(y(a,m(b)) =1|C=c) and gradient computation

    gg10 <- gg(a1, a0, betacoef, mean_covmv, thetacoef, mean_covyv, interaction)
    gg00 <- gg(a0, a0, betacoef, mean_covmv, thetacoef, mean_covyv, interaction)
    gg11 <- gg(a1, a1, betacoef, mean_covmv, thetacoef, mean_covyv, interaction)

    # Natural effects computation

    P10 <- gg10[[1]]
    P00 <- gg00[[1]]
    P11 <- gg11[[1]]

    ORd <- (P10 / (1 - P10)) / (P00 / (1 - P00))
    ORi <- (P11 / (1 - P11)) / (P10 / (1 - P10))
    ORt <- ORd * ORi

    RRd <- P10 / P00
    RRi <- P11 / P10
    RRt <- RRd * RRi

    RDd <- P10 - P00
    RDi <- P11 - P10
    RDt <- RDd + RDi

    # Confidence intervals computation

    Sigmabeta <- vcov(Mreg)
    Sigmatheta <- vcov(Yreg)

    l1 <- length(betacoef)
    l2 <- length(thetacoef)
    l <- l1 + l2

    Sigma <- matrix(0, nrow = l, ncol = l)
    Sigma[1:l1, 1:l1] <- Sigmabeta
    Sigma[(l1 + 1):l, (l1 + 1):l] <- Sigmatheta

    confcoefint <- 1 - (1 - confcoef) / 2
    int0 <- exp(-qnorm(confcoefint))
    int1 <- exp(qnorm(confcoefint))

    gradlnORd <- gg10[[2]] / (gg10[[1]] * (1 - gg10[[1]])) - gg00[[2]] / (gg00[[1]] * (1 - gg00[[1]]))
    gradlnORi <- gg11[[2]] / (gg11[[1]] * (1 - gg11[[1]])) - gg10[[2]] / (gg10[[1]] * (1 - gg10[[1]]))
    gradlnORt <- gradlnORd + gradlnORi

    selnORd <- sqrt(as.numeric(gradlnORd %*% Sigma %*% gradlnORd))
    selnORi <- sqrt(as.numeric(gradlnORi %*% Sigma %*% gradlnORi))
    selnORt <- sqrt(as.numeric(gradlnORt %*% Sigma %*% gradlnORt))

    CI_ORd <- ORd * c(int0, int1)^selnORd
    CI_ORi <- ORi * c(int0, int1)^selnORi
    CI_ORt <- ORt * c(int0, int1)^selnORt

    lnORd <- log(ORd)
    lnORi <- log(ORi)
    lnORt <- log(ORt)

    zORd <- lnORd / selnORd
    zORi <- lnORi / selnORi
    zORt <- lnORt / selnORt

    pvalueORd <- 2 * (1 - pnorm(abs(zORd)))
    pvalueORi <- 2 * (1 - pnorm(abs(zORi)))
    pvalueORt <- 2 * (1 - pnorm(abs(zORt)))

    seORd <- ORd * selnORd
    seORi <- ORi * selnORi
    seORt <- ORt * selnORt

    gradlnRRd <- gg10[[2]] / gg10[[1]] - gg00[[2]] / gg00[[1]]
    gradlnRRi <- gg11[[2]] / gg11[[1]] - gg10[[2]] / gg10[[1]]
    gradlnRRt <- gradlnRRd + gradlnRRi

    selnRRd <- sqrt(as.numeric(gradlnRRd %*% Sigma %*% gradlnRRd))
    selnRRi <- sqrt(as.numeric(gradlnRRi %*% Sigma %*% gradlnRRi))
    selnRRt <- sqrt(as.numeric(gradlnRRt %*% Sigma %*% gradlnRRt))

    CI_RRd <- RRd * c(int0, int1)^selnRRd
    CI_RRi <- RRi * c(int0, int1)^selnRRi
    CI_RRt <- RRt * c(int0, int1)^selnRRt

    lnRRd <- log(RRd)
    lnRRi <- log(RRi)
    lnRRt <- log(RRt)

    zRRd <- lnRRd / selnRRd
    zRRi <- lnRRi / selnRRi
    zRRt <- lnRRt / selnRRt

    pvalueRRd <- 2 * (1 - pnorm(abs(zRRd)))
    pvalueRRi <- 2 * (1 - pnorm(abs(zRRi)))
    pvalueRRt <- 2 * (1 - pnorm(abs(zRRt)))

    seRRd <- RRd * selnRRd
    seRRi <- RRi * selnRRi
    seRRt <- RRt * selnRRt

    gradRDd <- gg10[[2]] - gg00[[2]]
    gradRDi <- gg11[[2]] - gg10[[2]]
    gradRDt <- gradRDd + gradRDi

    seRDd <- sqrt(as.numeric(gradRDd %*% Sigma %*% gradRDd))
    seRDi <- sqrt(as.numeric(gradRDi %*% Sigma %*% gradRDi))
    seRDt <- sqrt(as.numeric(gradRDt %*% Sigma %*% gradRDt))

    CI_RDd <- RDd + seRDd * log(c(int0, int1))
    CI_RDi <- RDi + seRDi * log(c(int0, int1))
    CI_RDt <- RDt + seRDt * log(c(int0, int1))

    zRDd <- RDd / seRDd
    zRDi <- RDi / seRDi
    zRDt <- RDt / seRDt

    pvalueRDd <- 2 * (1 - pnorm(abs(zRDd)))
    pvalueRDi <- 2 * (1 - pnorm(abs(zRDi)))
    pvalueRDt <- 2 * (1 - pnorm(abs(zRDt)))

    # Probabilities P(y(a,m) =1|C=c) and gradient computation (m=0)

    gg_cde1m0 <- gg_cde(a1, m = 0, thetacoef, mean_covyv, interaction)
    gg_cde0m0 <- gg_cde(a0, m = 0, thetacoef, mean_covyv, interaction)

    # Controled effects computation (m=0)

    P1m0 <- gg_cde1m0[[1]]
    P0m0 <- gg_cde0m0[[1]]

    ORm0 <- (P1m0 / (1 - P1m0)) / (P0m0 / (1 - P0m0))
    RRm0 <- P1m0 / P0m0
    RDm0 <- P1m0 - P0m0

    # Confidence intervals computation (m=0)

    gradlnORm0 <- gg_cde1m0[[2]] / (P1m0 * (1 - P1m0)) -
      gg_cde0m0[[2]] / (P0m0 * (1 - P0m0))

    gradlnRRm0 <- gg_cde1m0[[2]] / P1m0 - gg_cde0m0[[2]] / P0m0
    gradRDm0 <- gg_cde1m0[[2]] - gg_cde0m0[[2]]

    selnORm0 <- sqrt(as.numeric(gradlnORm0 %*% Sigmatheta %*% gradlnORm0))
    selnRRm0 <- sqrt(as.numeric(gradlnRRm0 %*% Sigmatheta %*% gradlnRRm0))
    seRDm0 <- sqrt(as.numeric(gradRDm0 %*% Sigmatheta %*% gradRDm0))

    CI_ORm0 <- ORm0 * c(int0, int1)^selnORm0
    CI_RRm0 <- RRm0 * c(int0, int1)^selnRRm0
    CI_RDm0 <- RDm0 + seRDm0 * log(c(int0, int1))

    lnORm0 <- log(ORm0)
    lnRRm0 <- log(RRm0)

    zORm0 <- lnORm0 / selnORm0
    zRRm0 <- lnRRm0 / selnRRm0
    zRDm0 <- RDm0 / seRDm0

    pvalueORm0 <- 2 * (1 - pnorm(abs(zORm0)))
    pvalueRRm0 <- 2 * (1 - pnorm(abs(zRRm0)))
    pvalueRDm0 <- 2 * (1 - pnorm(abs(zRDm0)))

    seORm0 <- ORm0 * selnORm0
    seRRm0 <- RRm0 * selnRRm0

    # Probabilities P(y(a,m) =1|C=c) and gradient computation (m=1)

    gg_cde1m1 <- gg_cde(a1, m = 1, thetacoef, mean_covyv, interaction)
    gg_cde0m1 <- gg_cde(a0, m = 1, thetacoef, mean_covyv, interaction)

    # Controlled effects computation (m=1)

    P1m1 <- gg_cde1m1[[1]]
    P0m1 <- gg_cde0m1[[1]]

    ORm1 <- (P1m1 / (1 - P1m1)) / (P0m1 / (1 - P0m1))
    RRm1 <- P1m1 / P0m1
    RDm1 <- P1m1 - P0m1

    # Confidence intervals computation (m=1)

    gradlnORm1 <- gg_cde1m1[[2]] / (P1m1 * (1 - P1m1)) -
      gg_cde0m1[[2]] / (P0m1 * (1 - P0m1))

    gradlnRRm1 <- gg_cde1m1[[2]] / P1m1 - gg_cde0m1[[2]] / P0m1
    gradRDm1 <- gg_cde1m1[[2]] - gg_cde0m1[[2]]

    selnORm1 <- sqrt(as.numeric(gradlnORm1 %*% Sigmatheta %*% gradlnORm1))
    selnRRm1 <- sqrt(as.numeric(gradlnRRm1 %*% Sigmatheta %*% gradlnRRm1))
    seRDm1 <- sqrt(as.numeric(gradRDm1 %*% Sigmatheta %*% gradRDm1))

    CI_ORm1 <- ORm1 * c(int0, int1)^selnORm1
    CI_RRm1 <- RRm1 * c(int0, int1)^selnRRm1
    CI_RDm1 <- RDm1 + seRDm1 * log(c(int0, int1))

    lnORm1 <- log(ORm1)
    lnRRm1 <- log(RRm1)

    zORm1 <- lnORm1 / selnORm1
    zRRm1 <- lnRRm1 / selnRRm1
    zRDm1 <- RDm1 / seRDm1

    pvalueORm1 <- 2 * (1 - pnorm(abs(zORm1)))
    pvalueRRm1 <- 2 * (1 - pnorm(abs(zRRm1)))
    pvalueRDm1 <- 2 * (1 - pnorm(abs(zRDm1)))

    seORm1 <- ORm1 * selnORm1
    seRRm1 <- RRm1 * selnRRm1

    CIsup <- paste(confcoefint * 100, "%", sep = "")
    CIinf <- paste((1 - confcoefint) * 100, "%", sep = "")

    OR <- matrix(0, nrow = 3, ncol = 5)
    RR <- matrix(0, nrow = 3, ncol = 5)
    RD <- matrix(0, nrow = 3, ncol = 5)

    rownames(OR) <- c("Direct effect", "Indirect effect", "Total effect")
    colnames(OR) <- c("Estimate", "Std.error", CIinf, CIsup, "P.val")

    OR[1, ] <- c(ORd, seORd, CI_ORd, pvalueORd)
    OR[2, ] <- c(ORi, seORi, CI_ORi, pvalueORi)
    OR[3, ] <- c(ORt, seORt, CI_ORt, pvalueORt)

    rownames(RR) <- c("Direct effect", "Indirect effect", "Total effect")
    colnames(RR) <- c("Estimate", "Std.error", CIinf, CIsup, "P.val")

    RR[1, ] <- c(RRd, seRRd, CI_RRd, pvalueRRd)
    RR[2, ] <- c(RRi, seRRi, CI_RRi, pvalueRRi)
    RR[3, ] <- c(RRt, seRRt, CI_RRt, pvalueRRt)

    rownames(RD) <- c("Direct effect", "Indirect effect", "Total effect")
    colnames(RD) <- c("Estimate", "Std.error", CIinf, CIsup, "P.val")

    RD[1, ] <- c(RDd, seRDd, CI_RDd, pvalueRDd)
    RD[2, ] <- c(RDi, seRDi, CI_RDi, pvalueRDi)
    RD[3, ] <- c(RDt, seRDt, CI_RDt, pvalueRDt)

    ContEffm0 <- matrix(0, nrow = 3, ncol = 5)
    ContEffm1 <- matrix(0, nrow = 3, ncol = 5)

    rownames(ContEffm0) <- c("OR scale", "RR scale", "RD scale")
    colnames(ContEffm0) <- c("Estimate", "Std.error", CIinf, CIsup, "P.val")

    ContEffm0[1, ] <- c(ORm0, seORm0, CI_ORm0, pvalueORm0)
    ContEffm0[2, ] <- c(RRm0, seRRm0, CI_RRm0, pvalueRRm0)
    ContEffm0[3, ] <- c(RDm0, seRDm0, CI_RDm0, pvalueRDm0)

    rownames(ContEffm1) <- c("OR scale", "RR scale", "RD scale")
    colnames(ContEffm1) <- c("Estimate", "Std.error", CIinf, CIsup, "P.val")

    ContEffm1[1, ] <- c(ORm1, seORm1, CI_ORm1, pvalueORm1)
    ContEffm1[2, ] <- c(RRm1, seRRm1, CI_RRm1, pvalueRRm1)
    ContEffm1[3, ] <- c(RDm1, seRDm1, CI_RDm1, pvalueRDm1)

    results <- vector("list", 5)
    names(results) <- c(
      "Natural effects on OR scale",
      "Natural effects on RR scale",
      "Natural effects on RD scale",
      "Controlled direct effects (m=0)",
      "Controlled direct effects (m=1)"
    )

    OR <- as.data.frame(OR)
    RR <- as.data.frame(RR)
    RD <- as.data.frame(RD)
    ContEffm0 <- as.data.frame(ContEffm0)
    ContEffm1 <- as.data.frame(ContEffm1)

    OR[[5]] <- format.pval(OR[[5]], digits = 5)
    RR[[5]] <- format.pval(RR[[5]], digits = 5)
    RD[[5]] <- format.pval(RD[[5]], digits = 5)
    ContEffm0[[5]] <- format.pval(ContEffm0[[5]], digits = 5)
    ContEffm1[[5]] <- format.pval(ContEffm1[[5]], digits = 5)

    results[[1]] <- cbind(round(OR[1:4], digits = 5), OR[5])
    results[[2]] <- cbind(round(RR[1:4], digits = 5), RR[5])
    results[[3]] <- cbind(round(RD[1:4], digits = 5), RD[5])
    results[[4]] <- cbind(round(ContEffm0[1:4], digits = 5), ContEffm0[5])
    results[[5]] <- cbind(round(ContEffm1[1:4], digits = 5), ContEffm1[5])

  } else {
    gg <- function(a, b, betav, covmv, thetav, covyv, interaction) {
      if (interaction == TRUE) {
        probY1M1 <- expit(thetav[1] + thetav[3] +
          (thetav[2] + thetav[4]) * a + as.numeric(thetav[-(1:4)] %*% covyv))

        probY1M0 <- expit(thetav[1] + thetav[2] * a + as.numeric(thetav[-(1:4)] %*% covyv))
      } else {
        probY1M1 <- expit(thetav[1] + thetav[3] +
          thetav[2] * a + as.numeric(thetav[-(1:3)] %*% covyv))

        probY1M0 <- expit(thetav[1] + thetav[2] * a + as.numeric(thetav[-(1:3)] %*% covyv))
      }

      probM1 <- expit(betav[1] + betav[2] * b + as.numeric(betav[-(1:2)] %*% covmv))
      probM0 <- 1 - probM1

      return(probY1M1 * probM1 + probY1M0 * probM0)
    }


    gg_cde <- function(a, m, thetav, covyv, interaction) {
      if (interaction == TRUE) {
        P <- expit(thetav[1] + thetav[2] * a + thetav[3] * m +
          thetav[4] * a * m + as.numeric(thetav[-(1:4)] %*% covyv))
      } else {
        P <- expit(thetav[1] + thetav[2] * a + thetav[3] * m +
          as.numeric(thetav[-(1:3)] %*% covyv))
      }

      return(P)
    }

    P10 <- gg(a1, a0, betacoef, mean_covmv, thetacoef, mean_covyv, interaction)
    P00 <- gg(a0, a0, betacoef, mean_covmv, thetacoef, mean_covyv, interaction)
    P11 <- gg(a1, a1, betacoef, mean_covmv, thetacoef, mean_covyv, interaction)

    ORd <- (P10 / (1 - P10)) / (P00 / (1 - P00))
    ORi <- (P11 / (1 - P11)) / (P10 / (1 - P10))
    ORt <- ORd * ORi

    RRd <- P10 / P00
    RRi <- P11 / P10
    RRt <- RRd * RRi

    RDd <- P10 - P00
    RDi <- P11 - P10
    RDt <- RDd + RDi

    P1m0 <- gg_cde(a1, m = 0, thetacoef, mean_covyv, interaction)
    P0m0 <- gg_cde(a0, m = 0, thetacoef, mean_covyv, interaction)

    ORm0 <- (P1m0 / (1 - P1m0)) / (P0m0 / (1 - P0m0))
    RRm0 <- P1m0 / P0m0
    RDm0 <- P1m0 - P0m0

    P1m1 <- gg_cde(a1, m = 1, thetacoef, mean_covyv, interaction)
    P0m1 <- gg_cde(a0, m = 1, thetacoef, mean_covyv, interaction)

    ORm1 <- (P1m1 / (1 - P1m1)) / (P0m1 / (1 - P0m1))
    RRm1 <- P1m1 / P0m1
    RDm1 <- P1m1 - P0m1

    set.seed(bootseed)

    n <- nrow(data)

    ORboot <- matrix(0, nboot, 3)
    RRboot <- matrix(0, nboot, 3)
    RDboot <- matrix(0, nboot, 3)

    ORm0boot <- vector("numeric", length = nboot)
    RRm0boot <- vector("numeric", length = nboot)
    RDm0boot <- vector("numeric", length = nboot)

    ORm1boot <- vector("numeric", length = nboot)
    RRm1boot <- vector("numeric", length = nboot)
    RDm1boot <- vector("numeric", length = nboot)

    progress_bar <- txtProgressBar(min = 0, max = nboot, style = 3, char = "=")

    for (i in 1:nboot) {
      vboot <- sample(1:n, n, replace = TRUE)

      DATAboot <- data[vboot, ]

      if (Firth == TRUE) {
        Mreg <- logistf(Mform, data = DATAboot)
        Yreg <- logistf(Yform, data = DATAboot)
      } else {
        Mreg <- glm(Mform, data = DATAboot, family = binomial(link = "logit"))
        Yreg <- glm(Yform, data = DATAboot, family = binomial(link = "logit"))
      }

      betacoef <- Mreg$coefficients
      thetacoef <- Yreg$coefficients

      names(betacoef) <- NULL
      names(thetacoef) <- NULL

      P10 <- gg(a1, a0, betacoef, mean_covmv, thetacoef, mean_covyv, interaction)
      P00 <- gg(a0, a0, betacoef, mean_covmv, thetacoef, mean_covyv, interaction)
      P11 <- gg(a1, a1, betacoef, mean_covmv, thetacoef, mean_covyv, interaction)

      ORboot[i, 1] <- (P10 / (1 - P10)) / (P00 / (1 - P00))
      ORboot[i, 2] <- (P11 / (1 - P11)) / (P10 / (1 - P10))
      ORboot[i, 3] <- ORboot[i, 1] * ORboot[i, 2]

      RRboot[i, 1] <- P10 / P00
      RRboot[i, 2] <- P11 / P10
      RRboot[i, 3] <- RRboot[i, 1] * RRboot[i, 2]

      RDboot[i, 1] <- P10 - P00
      RDboot[i, 2] <- P11 - P10
      RDboot[i, 3] <- RDboot[i, 1] + RDboot[i, 2]

      P1m0 <- gg_cde(a1, m = 0, thetacoef, mean_covyv, interaction)
      P0m0 <- gg_cde(a0, m = 0, thetacoef, mean_covyv, interaction)

      ORm0boot[i] <- (P1m0 / (1 - P1m0)) / (P0m0 / (1 - P0m0))
      RRm0boot[i] <- P1m0 / P0m0
      RDm0boot[i] <- P1m0 - P0m0

      P1m1 <- gg_cde(a1, m = 1, thetacoef, mean_covyv, interaction)
      P0m1 <- gg_cde(a0, m = 1, thetacoef, mean_covyv, interaction)

      ORm1boot[i] <- (P1m1 / (1 - P1m1)) / (P0m1 / (1 - P0m1))
      RRm1boot[i] <- P1m1 / P0m1
      RDm1boot[i] <- P1m1 - P0m1

      setTxtProgressBar(progress_bar, value = i)
    }

    confcoefint <- 1 - (1 - confcoef) / 2
    cisup <- confcoefint
    ciinf <- 1 - confcoefint

    CI_ORd <- quantile(ORboot[, 1], c(ciinf, cisup))
    CI_ORi <- quantile(ORboot[, 2], c(ciinf, cisup))
    CI_ORt <- quantile(ORboot[, 3], c(ciinf, cisup))

    CI_RRd <- quantile(RRboot[, 1], c(ciinf, cisup))
    CI_RRi <- quantile(RRboot[, 2], c(ciinf, cisup))
    CI_RRt <- quantile(RRboot[, 3], c(ciinf, cisup))

    CI_RDd <- quantile(RDboot[, 1], c(ciinf, cisup))
    CI_RDi <- quantile(RDboot[, 2], c(ciinf, cisup))
    CI_RDt <- quantile(RDboot[, 3], c(ciinf, cisup))

    CI_ORm0 <- quantile(ORm0boot, c(ciinf, cisup))
    CI_RRm0 <- quantile(RRm0boot, c(ciinf, cisup))
    CI_RDm0 <- quantile(RDm0boot, c(ciinf, cisup))

    CI_ORm1 <- quantile(ORm1boot, c(ciinf, cisup))
    CI_RRm1 <- quantile(RRm1boot, c(ciinf, cisup))
    CI_RDm1 <- quantile(RDm1boot, c(ciinf, cisup))

    seORd <- sd(ORboot[, 1])
    seORi <- sd(ORboot[, 2])
    seORt <- sd(ORboot[, 3])

    seRRd <- sd(RRboot[, 1])
    seRRi <- sd(RRboot[, 2])
    seRRt <- sd(RRboot[, 3])

    seRDd <- sd(RDboot[, 1])
    seRDi <- sd(RDboot[, 2])
    seRDt <- sd(RDboot[, 3])

    seORm0 <- sd(ORm0boot)
    seRRm0 <- sd(RRm0boot)
    seRDm0 <- sd(RDm0boot)

    seORm1 <- sd(ORm1boot)
    seRRm1 <- sd(RRm1boot)
    seRDm1 <- sd(RDm1boot)

    #  Results

    CIsup <- paste(confcoefint * 100, "%", sep = "")
    CIinf <- paste((1 - confcoefint) * 100, "%", sep = "")

    OR <- matrix(0, nrow = 3, ncol = 4)
    RR <- matrix(0, nrow = 3, ncol = 4)
    RD <- matrix(0, nrow = 3, ncol = 4)

    rownames(OR) <- c("Direct effect", "Indirect effect", "Total effect")
    colnames(OR) <- c("Estimate", "Std.error", CIinf, CIsup)

    OR[1, ] <- c(ORd, seORd, CI_ORd)
    OR[2, ] <- c(ORi, seORi, CI_ORi)
    OR[3, ] <- c(ORt, seORt, CI_ORt)

    rownames(RR) <- c("Direct effect", "Indirect effect", "Total effect")
    colnames(RR) <- c("Estimate", "Std.error", CIinf, CIsup)

    RR[1, ] <- c(RRd, seRRd, CI_RRd)
    RR[2, ] <- c(RRi, seRRi, CI_RRi)
    RR[3, ] <- c(RRt, seRRt, CI_RRt)

    rownames(RD) <- c("Direct effect", "Indirect effect", "Total effect")
    colnames(RD) <- c("Estimate", "Std.error", CIinf, CIsup)

    RD[1, ] <- c(RDd, seRDd, CI_RDd)
    RD[2, ] <- c(RDi, seRDi, CI_RDi)
    RD[3, ] <- c(RDt, seRDt, CI_RDt)

    ContEffm0 <- matrix(0, nrow = 3, ncol = 4)
    ContEffm1 <- matrix(0, nrow = 3, ncol = 4)

    rownames(ContEffm0) <- c("OR scale", "RR scale", "RD scale")
    colnames(ContEffm0) <- c("Estimate", "Std.error", CIinf, CIsup)

    ContEffm0[1, ] <- c(ORm0, seORm0, CI_ORm0)
    ContEffm0[2, ] <- c(RRm0, seRRm0, CI_RRm0)
    ContEffm0[3, ] <- c(RDm0, seRDm0, CI_RDm0)

    rownames(ContEffm1) <- c("OR scale", "RR scale", "RD scale")
    colnames(ContEffm1) <- c("Estimate", "Std.error", CIinf, CIsup)

    ContEffm1[1, ] <- c(ORm1, seORm1, CI_ORm1)
    ContEffm1[2, ] <- c(RRm1, seRRm1, CI_RRm1)
    ContEffm1[3, ] <- c(RDm1, seRDm1, CI_RDm1)

    results <- vector("list", 5)
    names(results) <- c(
      "Natural effects on OR scale",
      "Natural effects on RR scale",
      "Natural effects on RD scale",
      "Controlled direct effects (m=0)",
      "Controlled direct effects (m=1)"
    )

    results[[1]] <- round(OR, digits = 5)
    results[[2]] <- round(RR, digits = 5)
    results[[3]] <- round(RD, digits = 5)
    results[[4]] <- round(ContEffm0, digits = 5)
    results[[5]] <- round(ContEffm1, digits = 5)

    close(progress_bar)
  }



  return(results)
}
