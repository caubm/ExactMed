#' @title  Exact Mediation Effects Computation (Binary Mediator)
#' @description Relying on a regression-based approach, the \code{exactmed()} function calculates standard
#'     causal mediation effects when the outcome and the mediator are binary. More precisely, \code{exactmed()}
#'     uses a logistic regression specification for both the outcome and the mediator in order to compute \emph{exact}
#'     conditional natural direct and indirect effects (see details in Samoilenko and Lefebvre, 2021).
#'     The function returns point and interval estimates for the conditional natural effects without making any assumption
#'     regarding the rareness or commonness of the outcome (hence the term exact). For completeness, \code{exactmed()} also
#'     calculates the conditional controlled direct effects at both values of the mediator. Natural and controlled effects
#'     estimates are reported using three different scales: odds ratio (OR), risk ratio (RR) and risk difference (RD).
#'     The interval estimates can be obtained either by the delta method or the bootstrap.
#' @param data A named data frame that includes the exposure, mediator and outcome variables as well as the covariates
#'     to be adjusted for in the models. The exposure can be either binary or continuous. If a covariate is categorical,
#'     it has to be included in the data frame as a factor, character or logical variable.
#' @param a The name of the binary or continuous exposure variable.
#' @param m The name of the binary mediator variable.
#' @param y The name of the binary outcome variable.
#' @param a1 A value corresponding to the high level of the exposure.
#' @param a0 A value corresponding to the low level of the exposure.
#' @param m_cov A vector containing the names of the adjustment variables (covariates) in the mediator model.
#' @param y_cov A vector containing the names of the adjustment variables (covariates) in the outcome model.
#' @param m_cov_cond A named vector (atomic vector or list) containing specific values for some or all
#'     of the adjustment covariates \code{m_cov} in the mediator model. Please consult the package vignette for details.
#' @param y_cov_cond A named vector (atomic vector or list) containing specific values for some or all
#'     of the adjustment covariates \code{y_cov} in the outcome model. Please consult the package vignette for details.
#' @param adjusted  A logical variable specifying whether to obtain adjusted or unadjusted estimates.
#'     If \code{adjusted = FALSE}, vectors \code{m_cov} and \code{y_cov} are ignored by the procedure.
#' @param interaction A logical variable specifying whether there is an exposure-mediator interaction term in the outcome model.
#' @param Firth A logical variable specifying whether to compute conventional or penalized maximum likelihood estimates
#'     for the logistic regression models (see details).
#' @param boot A logical value specifying whether the confidence intervals are obtained
#'     by the delta method or by percentile bootstrap.
#' @param nboot The number of bootstrap replications used to obtain the confidence intervals if \code{boot = TRUE}.
#'     By default \code{nboot = 1000}.
#' @param bootseed The value of the initial seed (positive integer) for random number generation if \code{boot = TRUE}.
#' @param confcoef A number between 0 and 1 for the confidence coefficient (ex.: 0.95) of the interval estimates.
#' @param hvalue_m The value corresponding to the high level of the mediator. If the mediator is already coded
#'     as a numerical binary variable taking 0 or 1 values, then by default \code{hvalue_m = 1}.
#' @param hvalue_y The value corresponding to the high level of the outcome. If the outcome is already coded
#'     as a numerical binary variable taking 0 or 1 values, then by default \code{hvalue_y = 1}.
#' @param yprevalence The prevalence of the outcome in the population (a number between 0 and 1).
#'     Option used when case-control data are used.
#'     The low level of the outcome is treated as the control level.
#' @importFrom stats as.formula binomial glm qnorm quantile terms vcov na.omit pnorm sd
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom sandwich vcovHC
#' @importFrom lmtest coeftest
#' @importFrom pkgcond suppress_warnings
#' @importFrom brglm2 brglmFit
#' @details By default, \code{exactmed()} reports mediation effects evaluated at the sample-specific mean values of the numerical covariates
#'     (including the dummy variables created internally by the function to represent the non-reference levels of the categorical covariates).
#'     In order to estimate mediation effects at specific values of some covariates (that is, stratum-specific effects),
#'     the user needs to provide named vectors \code{m_cov_cond} and/or \code{y_cov_cond} containing those values or levels. The adjustment
#'     covariates appearing in both \code{m_cov} and \code{y_cov} (common adjustment covariates) must have the same values; otherwise,
#'     \code{exactmed()}'s execution is aborted and an error message is displayed in the R console.
#'
#'     The Firth parameter allows to reduce the bias of the regression coefficients estimators when facing a problem of
#'     separation or quasi-separation. The bias reduction is achieved by the \code{\link[brglm2]{brglmFit}} fitting method of the \emph{brglm2} package.
#'     More precisely, estimates are obtained via penalized maximum likelihood with a Jeffreys prior penalty, which is equivalent to the mean
#'     bias-reducing adjusted score equation approach in Firth (1993).
#'
#'     When the data come from a case-control study, the \code{yprevalence} parameter should be used and its value ideally correspond to the true outcome prevalence.
#'     \code{exactmed()} accounts for the ascertainment in the sample by employing weighted regression techniques that use inverse probability weighting (IPW)
#'     with robust standard errors. These errors are obtained via the \code{\link[sandwich]{vcovHC}} function of the R package \emph{sandwich}.
#'     Specifically, we use the HC3 type covariance matrix estimator (default type of the \code{\link[sandwich]{vcovHC}} function).
#'
#'     For the mediation effects expressed on the multiplicative scales (odds ratio, OR; risk ratio, RR), the \code{exactmed()} function
#'     returns delta method confidence intervals by exponentiating the lower and upper limits of the normal confidence intervals obtained
#'     for the logarithmic transformations of the effects. The \code{exactmed()} function also provides the estimated standard errors of
#'     natural and controlled direct effects estimators that are not log-transformed, where those are derived using a first order Taylor expansion
#'     (e.g., \eqn{\hat{SE}(\hat{OR})=\hat{OR} \times \hat{SE}(\log(\hat{OR}))}). The function performs Z-tests (null hypothesis: there is no effect)
#'     computing the corresponding two-tailed \emph{p}-values. Note that for the multiplicative scales, the standard scores (test statistics)
#'     are obtained by dividing the logarithm of an effect estimator by the estimator of the corresponding standard error
#'     (e.g., \eqn{\log(\hat{OR}) / \hat{SE}(\log(\hat{OR}))}). No log-transformation is applied when working on the risk difference scale.
#'
#' @return An object of class \code{results} is returned:
#' \item{ne.or}{Natural effects estimates on OR scale.}
#' \item{ne.rr}{Natural effects estimates on RR scale.}
#' \item{ne.rd}{Natural effects estimates on RD scale.}
#' \item{cdem0}{Controlled direct effect (m=0) estimates.}
#' \item{cdem1}{Controlled direct effect (m=1) estimates.}
#' \item{med.reg}{Summary of the mediator regression.}
#' \item{out.reg}{Summary of the outcome regression.}
#'
#' If \code{boot==TRUE}, the returned object also contains:
#'
#' \item{boot.ne.or}{Bootstrap replications of natural effects on OR scale.}
#' \item{boot.ne.rr}{Bootstrap replications of natural effects on RR scale.}
#' \item{boot.ne.rd}{Bootstrap replications of natural effects on RD scale.}
#' \item{boot.cdem0.or}{Bootstrap replications of controlled direct effect (m=0) on OR scale.}
#' \item{boot.cdem0.rr}{Bootstrap replications of controlled direct effect (m=0) on RR scale.}
#' \item{boot.cdem0.rd}{Bootstrap replications of controlled direct effect (m=0) on RD scale.}
#' \item{boot.cdem1.or}{Bootstrap replications of controlled direct effect (m=1) on OR scale.}
#' \item{boot.cdem1.rr}{Bootstrap replications of controlled direct effect (m=1) on RR scale.}
#' \item{boot.cdem1.rd}{Bootstrap replications of controlled direct effect (m=1) on RD scale.}
#' \item{boot.ind}{Indices of the observations sampled in each bootstrap replication (one replication per column).}
#'
#' @note The \code{exactmed()} function only works for complete data. Users can apply multiple imputation techniques (e.g., R package \emph{mice})
#'     or remove observations of variables used in mediation analysis that have missing values (NA).
#' @references
#' Samoilenko M, Blais L, Lefebvre G. Comparing logistic and log-binomial models for causal mediation analyses of
#' binary mediators and rare binary outcomes: evidence to support cross-checking of mediation results in practice.
#' \emph{Observational Studies}.2018;4(1):193-216. \doi{10.1353/obs.2018.0013}.
#'
#' Samoilenko M, Lefebvre G. Parametric-regression-based causal mediation analysis of binary outcomes and binary mediators:
#' moving beyond the rareness or commonness of the outcome. \emph{American Journal of Epidemiology}.2021;190(9):1846-1858. \doi{10.1093/aje/kwab055}.
#'
#' Samoilenko M, Lefebvre G. An exact regression-based approach for the estimation of the natural direct and indirect effects
#' with a binary outcome and a continuous mediator. \emph{Statistics in Medicine}.2023; 42(3): 353–387. \doi{10.1002/sim.9621}.
#'
#' Firth D. Bias reduction of maximum likelihood estimates.
#' \emph{Biometrika}.1993;80:27-38. \doi{10.2307/2336755}.
#' @export
#' @examples
#' exactmed(
#'   data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
#'   m_cov = c("C1", "C2"), y_cov = c("C1", "C2")
#' )
#'
#' exactmed(
#'   data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
#'   m_cov = c("C1", "C2"), y_cov = c("C1", "C2"), yprevalence = 0.1
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
                     hvalue_m = NULL, hvalue_y = NULL, yprevalence = NULL) {
  .check_input_param(
    data = data, a = a, m = m, y = y, a1 = a1, a0 = a0, m_cov = m_cov, y_cov = y_cov,
    m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond, adjusted = adjusted,
    interaction = interaction, Firth = Firth, boot = boot, nboot = nboot,
    bootseed = bootseed, confcoef = confcoef, hvalue_m = hvalue_m, hvalue_y = hvalue_y,
    yprevalence
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

  # Function 'gg' for Nested probabilities P(y(a,m(b)) =1|C=c) and gradient computation

  if (boot == FALSE) {

    if(is.null(yprevalence)) {
      if (Firth == TRUE) {
        coef_estimate <- function(data, Mform, Yform){
          Mreg <- glm(Mform, data = data, family = binomial(link = "logit"),
                      method = brglmFit, type = "MPL_Jeffreys")
          Yreg <- glm(Yform, data = data, family = binomial(link = "logit"),
                      method = brglmFit, type = "MPL_Jeffreys")

          result <- vector("list", length = 6)
          result[[1]] <- Mreg$coefficients
          result[[2]] <- Yreg$coefficients
          result[[3]] <- vcov(Mreg)
          result[[4]] <- vcov(Yreg)
          result[[5]] <- Mreg
          result[[6]] <- Yreg
          names(result[[1]]) <- NULL
          names(result[[2]]) <- NULL

          return(result)
        }
      } else {
        coef_estimate <- function(data, Mform, Yform){
          Mreg <- glm(Mform, data = data, family = binomial(link = "logit"))
          Yreg <- glm(Yform, data = data, family = binomial(link = "logit"))

          result <- vector("list", length = 6)
          result[[1]] <- Mreg$coefficients
          result[[2]] <- Yreg$coefficients
          result[[3]] <- vcov(Mreg)
          result[[4]] <- vcov(Yreg)
          result[[5]] <- Mreg
          result[[6]] <- Yreg
          names(result[[1]]) <- NULL
          names(result[[2]]) <- NULL

          return(result)
        }
      }
    } else {
      prob1 <- mean(data[[y]] == 1)
      cc_weights <- ifelse(data[[y]] == 1, yprevalence / prob1,
                           (1 - yprevalence) / (1 - prob1))
      if (Firth == TRUE) {
        coef_estimate <- function(data, Mform, Yform){
          Mreg <- suppress_warnings(glm(Mform, data = data, family = binomial(link = "logit"),
                      method = brglmFit, type = "MPL_Jeffreys", weights = cc_weights))
          Yreg <- suppress_warnings(glm(Yform, data = data, family = binomial(link = "logit"),
                                        method = brglmFit, type = "MPL_Jeffreys", weights = cc_weights))

          result <- vector("list", length = 6)
          result[[1]] <- Mreg$coefficients
          result[[2]] <- Yreg$coefficients
          result[[3]] <- vcovHC(Mreg)
          result[[4]] <- vcovHC(Yreg)
          result[[5]] <- coeftest(Mreg, vcov. = vcovHC(Mreg))
          result[[6]] <- coeftest(Yreg, vcov. = vcovHC(Yreg))
          names(result[[1]]) <- NULL
          names(result[[2]]) <- NULL

          return(result)
        }
      } else {
        coef_estimate <- function(data, Mform, Yform){
          Mreg <- suppress_warnings(glm(Mform, data = data, family = binomial(link = "logit"),
                      weights = cc_weights))
          Yreg <- suppress_warnings(glm(Yform, data = data, family = binomial(link = "logit"),
                      weights = cc_weights))

          result <- vector("list", length = 6)
          result[[1]] <- Mreg$coefficients
          result[[2]] <- Yreg$coefficients
          result[[3]] <- vcovHC(Mreg)
          result[[4]] <- vcovHC(Yreg)
          result[[5]] <- coeftest(Mreg, vcov. = vcovHC(Mreg))
          result[[6]] <- coeftest(Yreg, vcov. = vcovHC(Yreg))
          names(result[[1]]) <- NULL
          names(result[[2]]) <- NULL

          return(result)
        }
      }
    }

    beta_theta_coef <- coef_estimate(data, Mform, Yform)

    betacoef <- beta_theta_coef[[1]]
    thetacoef <- beta_theta_coef[[2]]

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

    gg_cde <- function(a, astar, m, thetav, covyv, interaction) {
      if (interaction == TRUE) {
        terms_a <- thetav[1] + thetav[2] * a + thetav[3] * m + thetav[4] * a * m +
          as.numeric(thetav[-(1:4)] %*% covyv)

        terms_astar <- thetav[1] + thetav[2] * astar + thetav[3] * m +
          thetav[4] * astar * m + as.numeric(thetav[-(1:4)] %*% covyv)

        exp_a <- exp(terms_a)
        exp_astar <- exp(terms_astar)
        expit_a <- expit(terms_a)
        expit_astar <- expit(terms_astar)

        OR <- exp(thetav[2] * (a - astar) + thetav[4] * (a - astar) * m)
        RR <- OR * (1 + exp_astar) / (1 + exp_a)
        RD <- expit_a - expit_astar

        DlnORtheta1 <- 0
        DlnORtheta2 <- a - astar
        DlnRRtheta1 <- -RD
        DlnRRtheta2 <- a / (1 + exp_a) - astar / (1 + exp_astar)
        DlnRDtheta1 <- exp_a / ((1 + exp_a)^2) - exp_astar / ((1 + exp_astar)^2)
        DlnRDtheta2 <- a * exp_a / ((1 + exp_a)^2) - astar * exp_astar / ((1 + exp_astar)^2)

        gradlnOR <- c(DlnORtheta1, DlnORtheta2, m * DlnORtheta1, m * DlnORtheta2, covyv * DlnORtheta1)
        gradlnRR <- c(DlnRRtheta1, DlnRRtheta2, m * DlnRRtheta1, m * DlnRRtheta2, covyv * DlnRRtheta1)
        gradlnRD <- c(DlnRDtheta1, DlnRDtheta2, m * DlnRDtheta1, m * DlnRDtheta2, covyv * DlnRDtheta1)
      } else {
        terms_a <- thetav[1] + thetav[2] * a + thetav[3] * m  + as.numeric(thetav[-(1:3)] %*% covyv)
        terms_astar <- thetav[1] + thetav[2] * astar + thetav[3] * m  + as.numeric(thetav[-(1:3)] %*% covyv)

        exp_a <- exp(terms_a)
        exp_astar <- exp(terms_astar)
        expit_a <- expit(terms_a)
        expit_astar <- expit(terms_astar)

        OR <- exp(thetav[2] * (a - astar))
        RR <- OR * (1 + exp_astar) / (1 + exp_a)
        RD <- expit_a - expit_astar

        DlnORtheta1 <- 0
        DlnORtheta2 <- a - astar
        DlnRRtheta1 <- -RD
        DlnRRtheta2 <- a / (1 + exp_a) - astar / (1 + exp_astar)
        DlnRDtheta1 <- exp_a / ((1 + exp_a)^2) - exp_astar / ((1 + exp_astar)^2)
        DlnRDtheta2 <- a * exp_a / ((1 + exp_a)^2) - astar * exp_astar / ((1 + exp_astar)^2)

        gradlnOR <- c(DlnORtheta1, DlnORtheta2, m * DlnORtheta1, covyv * DlnORtheta1)
        gradlnRR <- c(DlnRRtheta1, DlnRRtheta2, m * DlnRRtheta1, covyv * DlnRRtheta1)
        gradlnRD <- c(DlnRDtheta1, DlnRDtheta2, m * DlnRDtheta1, covyv * DlnRDtheta1)
      }

      result <- vector("list", length = 3)
      names(result) <- c("OR", "RR", "RD")
      result$OR <- vector("list", length = 2)
      result$RR <- vector("list", length = 2)
      result$RD <- vector("list", length = 2)

      result$OR[[1]] <- OR
      result$OR[[2]] <- gradlnOR
      result$RR[[1]] <- RR
      result$RR[[2]] <- gradlnRR
      result$RD[[1]] <- RD
      result$RD[[2]] <- gradlnRD

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

    Sigmabeta <- beta_theta_coef[[3]]
    Sigmatheta <- beta_theta_coef[[4]]

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

    # Controlled direct effect and gradient computations (m=0)

    gg_cdem0 <- gg_cde(a = a1, astar = a0, m = 0, thetav = thetacoef,
                       covyv = mean_covyv, interaction = interaction)

    # Controlled direct effect (m=0)

    ORm0 <- gg_cdem0$OR[[1]]
    RRm0 <- gg_cdem0$RR[[1]]
    RDm0 <- gg_cdem0$RD[[1]]

    # Confidence intervals computation (m=0)

    gradlnORm0 <- gg_cdem0$OR[[2]]
    gradlnRRm0 <- gg_cdem0$RR[[2]]
    gradRDm0 <- gg_cdem0$RD[[2]]

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

    # Controlled direct effect and gradient computations (m=1)

    gg_cdem1 <- gg_cde(a = a1, astar = a0, m = 1, thetav = thetacoef,
                       covyv = mean_covyv, interaction = interaction)

    # Controlled effects (m=1)

    ORm1 <- gg_cdem1$OR[[1]]
    RRm1 <- gg_cdem1$RR[[1]]
    RDm1 <- gg_cdem1$RD[[1]]

    # Confidence intervals computation (m=1)

    gradlnORm1 <- gg_cdem1$OR[[2]]
    gradlnRRm1 <- gg_cdem1$RR[[2]]
    gradRDm1 <- gg_cdem1$RD[[2]]

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

    results <- vector("list", 7)
    names(results) <- c(
      "ne.or",
      "ne.rr",
      "ne.rd",
      "cdem0",
      "cdem1",
      "med.reg",
      "out.reg"
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
    results[[6]] <- beta_theta_coef[[5]]
    results[[7]] <- beta_theta_coef[[6]]

    class(results) <- c("results", "list")

  } else {

    if(is.null(yprevalence)) {
      repli_data <- function(data, n, y) {
        vboot <- sample(1:n, n, replace = TRUE)
        DATAboot <- data[vboot, ]
        result <- vector("list", length = 2)
        result[[1]] <- DATAboot
        result[[2]] <- vboot
        return(result)
      }

      if (Firth == TRUE) {
        coef_estimate <- function(data, Mform, Yform){
          Mreg <- glm(Mform, data = data, family = binomial(link = "logit"),
                      method = brglmFit, type = "MPL_Jeffreys")
          Yreg <- glm(Yform, data = data, family = binomial(link = "logit"),
                      method = brglmFit, type = "MPL_Jeffreys")

          result <- vector("list", length = 2)
          names(result) <- c('Mreg', 'Yreg')
          result$Mreg <- Mreg
          result$Yreg <- Yreg

          return(result)
        }
      } else {
        coef_estimate <- function(data, Mform, Yform){
          Mreg <- glm(Mform, data = data, family = binomial(link = "logit"))
          Yreg <- glm(Yform, data = data, family = binomial(link = "logit"))

          result <- vector("list", length = 2)
          names(result) <- c('Mreg', 'Yreg')
          result$Mreg <- Mreg
          result$Yreg <- Yreg

          return(result)
        }
      }
    } else {
      prob1 <- mean(data[[y]] == 1)
      w_case <- yprevalence / prob1
      w_control <- (1 - yprevalence) / (1 - prob1)
      cc_weights <- c(rep(w_case, sum(data[[y]] == 1)), rep(w_control, sum(data[[y]] == 0)))
      v_data <- c(which(data[[y]] == 1), which(data[[y]] == 0))
      data <- data[v_data, ]

      repli_data <- function(data, n, y) {
        case_indice <- sample(which(data[[y]] == 1), sum(data[[y]] == 1), replace = TRUE)
        control_indice <- sample(which(data[[y]] == 0), sum(data[[y]] == 0), replace = TRUE)
        DATAboot <- data[c(case_indice, control_indice), ]
        result <- vector("list", length = 2)
        result[[1]] <- DATAboot
        result[[2]] <- v_data[c(case_indice, control_indice)]
        return(result)
      }

      if (Firth == TRUE) {
        coef_estimate <- function(data, Mform, Yform){
          Mreg <- suppress_warnings(glm(Mform, data = data, family = binomial(link = "logit"),
                                        method = brglmFit, type = "MPL_Jeffreys", weights = cc_weights))
          Yreg <- suppress_warnings(glm(Yform, data = data, family = binomial(link = "logit"),
                                        method = brglmFit, type = "MPL_Jeffreys", weights = cc_weights))

          result <- vector("list", length = 2)
          names(result) <- c('Mreg', 'Yreg')
          result$Mreg <- Mreg
          result$Yreg <- Yreg

          return(result)
        }
      } else {
        coef_estimate <- function(data, Mform, Yform){
          Mreg <- suppress_warnings(glm(Mform, data = data, family = binomial(link = "logit"),
                      weights = cc_weights))
          Yreg <- suppress_warnings(glm(Yform, data = data, family = binomial(link = "logit"),
                      weights = cc_weights))

          result <- vector("list", length = 2)
          names(result) <- c('Mreg', 'Yreg')
          result$Mreg <- Mreg
          result$Yreg <- Yreg

          return(result)
        }
      }
    }

    beta_theta_coef_ini <- coef_estimate(data, Mform, Yform)

    betacoef <- beta_theta_coef_ini$Mreg$coefficients
    thetacoef <- beta_theta_coef_ini$Yreg$coefficients

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

    gg_cde <- function(a, astar, m, thetav, covyv, interaction) {
      if (interaction == TRUE) {
        terms_a <- thetav[1] + thetav[2] * a + thetav[3] * m + thetav[4] * a * m +
          as.numeric(thetav[-(1:4)] %*% covyv)

        terms_astar <- thetav[1] + thetav[2] * astar + thetav[3] * m +
          thetav[4] * astar * m + as.numeric(thetav[-(1:4)] %*% covyv)

        exp_a <- exp(terms_a)
        exp_astar <- exp(terms_astar)
        expit_a <- expit(terms_a)
        expit_astar <- expit(terms_astar)

        OR <- exp(thetav[2] * (a - astar) + thetav[4] * (a - astar) * m)
        RR <- OR * (1 + exp_astar) / (1 + exp_a)
        RD <- expit_a - expit_astar
      } else {
        terms_a <- thetav[1] + thetav[2] * a + thetav[3] * m  + as.numeric(thetav[-(1:3)] %*% covyv)
        terms_astar <- thetav[1] + thetav[2] * astar + thetav[3] * m  + as.numeric(thetav[-(1:3)] %*% covyv)

        exp_a <- exp(terms_a)
        exp_astar <- exp(terms_astar)
        expit_a <- expit(terms_a)
        expit_astar <- expit(terms_astar)

        OR <- exp(thetav[2] * (a - astar))
        RR <- OR * (1 + exp_astar) / (1 + exp_a)
        RD <- expit_a - expit_astar
      }

      result <- vector("list", length = 3)
      names(result) <- c("OR", "RR", "RD")
      result$OR <- OR
      result$RR <- RR
      result$RD <- RD

      return(result)
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

    # Controled effects (m=0)

    gg_cdem0 <- gg_cde(a = a1, astar = a0, m = 0, thetav = thetacoef,
                       covyv = mean_covyv, interaction = interaction)

    ORm0 <- gg_cdem0$OR
    RRm0 <- gg_cdem0$RR
    RDm0 <- gg_cdem0$RD

    # Controled effects (m=1)

    gg_cdem1 <- gg_cde(a = a1, astar = a0, m = 1, thetav = thetacoef,
                       covyv = mean_covyv, interaction = interaction)

    ORm1 <- gg_cdem1$OR
    RRm1 <- gg_cdem1$RR
    RDm1 <- gg_cdem1$RD

    set.seed(bootseed)

    n <- nrow(data)

    ORboot <- matrix(0, nboot, 3)
    RRboot <- matrix(0, nboot, 3)
    RDboot <- matrix(0, nboot, 3)

    colnames(ORboot) <- c("Direct effect", "Indirect effect", "Total effect")
    colnames(RRboot) <- c("Direct effect", "Indirect effect", "Total effect")
    colnames(RDboot) <- c("Direct effect", "Indirect effect", "Total effect")

    ORm0boot <- vector("numeric", length = nboot)
    RRm0boot <- vector("numeric", length = nboot)
    RDm0boot <- vector("numeric", length = nboot)

    ORm1boot <- vector("numeric", length = nboot)
    RRm1boot <- vector("numeric", length = nboot)
    RDm1boot <- vector("numeric", length = nboot)

    indiceboot <- matrix(0, n, nboot)

    progress_bar <- txtProgressBar(min = 0, max = nboot, style = 3, char = "=")

    for (i in 1:nboot) {
      repli_list <- repli_data(data, n, y)
      DATAboot <- repli_list[[1]]
      indiceboot[, i] <- repli_list[[2]]

      beta_theta_coef <- coef_estimate(DATAboot, Mform, Yform)

      betacoef <- beta_theta_coef$Mreg$coefficients
      thetacoef <- beta_theta_coef$Yreg$coefficients

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

      gg_cdem0 <- gg_cde(a = a1, astar = a0, m = 0, thetav = thetacoef,
                         covyv = mean_covyv, interaction = interaction)

      ORm0boot[i] <- gg_cdem0$OR
      RRm0boot[i] <- gg_cdem0$RR
      RDm0boot[i] <- gg_cdem0$RD


      gg_cdem1 <- gg_cde(a = a1, astar = a0, m = 1, thetav = thetacoef,
                         covyv = mean_covyv, interaction = interaction)

      ORm1boot[i] <- gg_cdem1$OR
      RRm1boot[i] <- gg_cdem1$RR
      RDm1boot[i] <- gg_cdem1$RD

      setTxtProgressBar(progress_bar, value = i)
    }

    confcoefint <- 1 - (1 - confcoef) / 2
    cisup <- confcoefint
    ciinf <- 1 - confcoefint

    CI_ORd <- quantile(ORboot[, 1], c(ciinf, cisup), na.rm = TRUE)
    CI_ORi <- quantile(ORboot[, 2], c(ciinf, cisup), na.rm = TRUE)
    CI_ORt <- quantile(ORboot[, 3], c(ciinf, cisup), na.rm = TRUE)

    CI_RRd <- quantile(RRboot[, 1], c(ciinf, cisup), na.rm = TRUE)
    CI_RRi <- quantile(RRboot[, 2], c(ciinf, cisup), na.rm = TRUE)
    CI_RRt <- quantile(RRboot[, 3], c(ciinf, cisup), na.rm = TRUE)

    CI_RDd <- quantile(RDboot[, 1], c(ciinf, cisup), na.rm = TRUE)
    CI_RDi <- quantile(RDboot[, 2], c(ciinf, cisup), na.rm = TRUE)
    CI_RDt <- quantile(RDboot[, 3], c(ciinf, cisup), na.rm = TRUE)

    CI_ORm0 <- quantile(ORm0boot, c(ciinf, cisup), na.rm = TRUE)
    CI_RRm0 <- quantile(RRm0boot, c(ciinf, cisup), na.rm = TRUE)
    CI_RDm0 <- quantile(RDm0boot, c(ciinf, cisup), na.rm = TRUE)

    CI_ORm1 <- quantile(ORm1boot, c(ciinf, cisup), na.rm = TRUE)
    CI_RRm1 <- quantile(RRm1boot, c(ciinf, cisup), na.rm = TRUE)
    CI_RDm1 <- quantile(RDm1boot, c(ciinf, cisup), na.rm = TRUE)

    seORd <- sd(ORboot[, 1], na.rm = TRUE)
    seORi <- sd(ORboot[, 2], na.rm = TRUE)
    seORt <- sd(ORboot[, 3], na.rm = TRUE)

    seRRd <- sd(RRboot[, 1], na.rm = TRUE)
    seRRi <- sd(RRboot[, 2], na.rm = TRUE)
    seRRt <- sd(RRboot[, 3], na.rm = TRUE)

    seRDd <- sd(RDboot[, 1], na.rm = TRUE)
    seRDi <- sd(RDboot[, 2], na.rm = TRUE)
    seRDt <- sd(RDboot[, 3], na.rm = TRUE)

    seORm0 <- sd(ORm0boot, na.rm = TRUE)
    seRRm0 <- sd(RRm0boot, na.rm = TRUE)
    seRDm0 <- sd(RDm0boot, na.rm = TRUE)

    seORm1 <- sd(ORm1boot, na.rm = TRUE)
    seRRm1 <- sd(RRm1boot, na.rm = TRUE)
    seRDm1 <- sd(RDm1boot, na.rm = TRUE)

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

    results <- vector("list", 17)
    names(results) <- c(
      "ne.or",
      "ne.rr",
      "ne.rd",
      "cdem0",
      "cdem1",
      "boot.ne.or",
      "boot.ne.rr",
      "boot.ne.rd",
      "boot.cdem0.or",
      "boot.cdem0.rr",
      "boot.cdem0.rd",
      "boot.cdem1.or",
      "boot.cdem1.rr",
      "boot.cdem1.rd",
      "boot.ind",
      "med.reg",
      "out.reg"
    )

      results[[1]] <- round(OR, digits = 5)
      results[[2]] <- round(RR, digits = 5)
      results[[3]] <- round(RD, digits = 5)
      results[[4]] <- round(ContEffm0, digits = 5)
      results[[5]] <- round(ContEffm1, digits = 5)
      results[[6]] <- ORboot
      results[[7]] <- RRboot
      results[[8]] <- RDboot
      results[[9]] <- ORm0boot
      results[[10]] <- RRm0boot
      results[[11]] <- RDm0boot
      results[[12]] <- ORm1boot
      results[[13]] <- RRm1boot
      results[[14]] <- RDm1boot
      results[[15]] <- indiceboot
      results[[16]] <- beta_theta_coef_ini$Mreg
      results[[17]] <- beta_theta_coef_ini$Yreg

      class(results) <- c("results", "list")


    close(progress_bar)
  }

  return(results)
}
