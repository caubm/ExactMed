#' @title  Exact Mediation Effects Computation (Continuous Mediator)
#' @description Relying on a regression-based approach, the \code{exactmed_c()} function calculates standard
#'     causal mediation effects when the outcome is binary and the mediator is continuous. More precisely, \code{exactmed_c()}
#'     relies on logistic and linear models for the outcome and mediator, respectively, in order to compute \emph{exact}
#'     conditional natural direct and indirect effects.
#'     Nested counterfactual probabilities underlying the definition of natural effects are calculated using numerical integration.
#'     The function returns point and interval estimates for the conditional natural effects without making any assumption
#'     regarding the rareness or commonness of the outcome (hence the term exact). For completeness, \code{exactmed_c()} also
#'     calculates the conditional controlled direct effect at a specified value of the mediator. Natural and controlled effects
#'     estimates are reported using three different scales: odds ratio (OR), risk ratio (RR) and risk difference (RD).
#'     The interval estimates can be obtained either by the delta method or the bootstrap.
#' @param data A named data frame that includes the exposure, mediator and outcome variables as well as the covariates
#'     to be adjusted for in the models. The exposure can be either binary or continuous. If a covariate is categorical,
#'     it has to be included in the data frame as a factor, character or logical variable.
#' @param a The name of the binary or continuous exposure variable.
#' @param m The name of the continuous mediator variable.
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
#'     for the outcome logistic regression model.
#' @param boot A logical value specifying whether the confidence intervals are obtained
#'     by the delta method or by percentile bootstrap.
#' @param nboot   The number of bootstrap replications used to obtain the confidence intervals if \code{boot = TRUE}.
#' @param bootseed The value of the initial seed (positive integer) for random number generation if \code{boot = TRUE}.
#' @param confcoef A number between 0 and 1 for the confidence coefficient (ex.: 0.95) of the interval estimates.
#' @param hvalue_y The value corresponding to the high level of the outcome. If the outcome is already coded
#'     as a numerical binary variable taking 0 or 1 values, then by default \code{hvalue_y = 1}.
#' @param yprevalence The prevalence of the outcome in the population. Option used when case-control data are used.
#'     The low level of the outcome is treated as the control level.
#' @param mf The value of the mediator at which the conditional controlled direct effect is computed. If it is not specified,
#'      \code{mf} is fixed at the sample-specific mean of the mediator (default).
#' @importFrom stats as.formula binomial glm qnorm quantile terms vcov na.omit pnorm sd integrate lm
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom sandwich vcovHC
#' @importFrom lmtest coeftest
#' @importFrom pkgcond suppress_warnings
#' @importFrom brglm2 brglmFit
#' @details By default, \code{exactmed_c()} reports mediation effects evaluated at the sample-specific mean values of the numerical covariates
#'     (including the dummy variables created internally by the function to represent the non-reference levels of the categorical covariates).
#'     In order to estimate mediation effects at specific values of some covariates (that is, stratum-specific effects),
#'     the user needs to provide named vectors \code{m_cov_cond} and/or \code{y_cov_cond} containing those values or levels. The adjustment
#'     covariates appearing in both \code{m_cov} and \code{y_cov} (common adjustment covariates) must have the same values; otherwise,
#'     \code{exactmed_c()}'s execution is aborted and an error message is displayed in the R console.
#'
#'     The Firth parameter allows to reduce the bias of the outcome logistic regression coefficients estimators when facing a problem of
#'     separation or quasi-separation. The bias reduction is achieved by the \code{\link[brglm2]{brglmFit}} fitting method of the \emph{brglm2} package.
#'     More precisely, estimates are obtained using a penalized maximum likelihood with a Jeffreys prior penalty, which is equivalent to the mean
#'     bias-reducing adjusted score equation approach in Firth (1993).
#'
#'     When the data come from a case-control study, the \code{yprevalence} parameter should be used and its value ideally correspond to the true outcome prevalence.
#'     \code{exactmed()} accounts for the ascertainment in the sample by employing weighted regression techniques that use inverse probability weighting (IPW)
#'     with robust standard errors. These errors are obtained via the \code{\link[sandwich]{vcovHC}} function of the R package \emph{sandwich}.
#'     Specifically, we use the HC3 type covariance matrix estimator (default type of the \code{\link[sandwich]{vcovHC}} function).
#'
#' @return An object of class \code{results_c} is returned:
#' \item{ne.or}{Natural effects estimates on OR scale.}
#' \item{ne.rr}{Natural effects estimates on RR scale.}
#' \item{ne.rd}{Natural effects estimates on RD scale.}
#' \item{cde}{Controlled direct effect estimates.}
#' \item{med.reg}{Summary of the mediator regression.}
#' \item{out.reg}{Summary of the outcome regression.}
#'
#' If \code{boot==TRUE}, the returned object also contains:
#' \item{boot.ne.or}{Bootstrap replications of natural effects on OR scale.}
#' \item{boot.ne.rr}{Bootstrap replications of natural effects on RR scale.}
#' \item{boot.ne.rd}{Bootstrap replications of natural effects on RD scale.}
#' \item{boot.cde.or}{Bootstrap replications of controlled direct effect on OR scale.}
#' \item{boot.cde.rr}{Bootstrap replications of controlled direct effect on RR scale.}
#' \item{boot.cde.rd}{Bootstrap replications of controlled direct effect on RD scale.}
#' \item{boot.ind}{Indices of the observations sampled in each bootstrap replication (one replication per column).}
#'
#' @note The \code{exactmed_c()} function only works for complete data. Users can apply multiple imputation techniques (e.g., R package \emph{mice})
#'  or remove observations of variables used in mediation analysis that have missing values (NA).
#' @references
#' Samoilenko M, Blais L, Lefebvre G. Comparing logistic and log-binomial models for causal mediation analyses of
#' binary mediators and rare binary outcomes: evidence to support cross-checking of mediation results in practice.
#' \emph{Observational Studies}.2018;4(1):193-216.
#'
#' Samoilenko M, Lefebvre G. Parametric-regression-based causal mediation analysis of binary outcomes and binary mediators:
#' moving beyond the rareness or commonness of the outcome, \emph{American Journal of Epidemiology}.2021;190(9):1846-1858. \doi{10.1093/aje/kwab055}.
#'
#' Samoilenko M, Lefebvre G. An exact regression-based approach for the estimation of the natural direct and indirect effects
#' with a binary outcome and a continuous mediator. \emph{Statistics in Medicine}.2022;1-35. \doi{10.1002/sim.9621}.
#'
#' Firth D. Bias reduction of maximum likelihood estimates.
#' \emph{Biometrika}.1993;80:27-38. \doi{10.2307/2336755}.
#' @export
#' @examples
#' exactmed_c(
#'   data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
#'   m_cov = c("C1", "C2"), y_cov = c("C1", "C2")
#' )
#'
#' exactmed_c(
#'   data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
#'   m_cov = c("C1", "C2"), y_cov = c("C1", "C2"), yprevalence = 0.1
#' )
#'
#' m_cov_cond <- c(C1 = 0.1, C2 = 0.4)
#' y_cov_cond <- c(C1 = 0.1, C2 = 0.4)
#'
#' exactmed_c(
#'   data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
#'   m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
#'   m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
#' )
#'
#' C1b <- factor(sample(c("a", "b", "c"), nrow(datamed_c), replace = TRUE))
#' datamed_c$C1 <- C1b
#'
#' m_cov_cond <- list(C1 = "c", C2 = 0.4)
#' y_cov_cond <- list(C1 = "c", C2 = 0.4)
#'
#' exactmed_c(
#'   data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
#'   m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
#'   m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
#' )

exactmed_c <- function(data, a, m, y, a1, a0, m_cov = NULL, y_cov = NULL, m_cov_cond = NULL,
                      y_cov_cond = NULL, adjusted = TRUE, interaction = TRUE, Firth = FALSE,
                      boot = FALSE, nboot = 1000, bootseed = 1991, confcoef = 0.95,
                      hvalue_y = NULL, yprevalence = NULL, mf = NULL) {
  .check_input_param_c(
    data = data, a = a, m = m, y = y, a1 = a1, a0 = a0, m_cov = m_cov, y_cov = y_cov,
    m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond, adjusted = adjusted,
    interaction = interaction, Firth = Firth, boot = boot, nboot = nboot,
    bootseed = bootseed, confcoef = confcoef, hvalue_y = hvalue_y,
    yprevalence = yprevalence, mf = mf
  )

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

  if (is.null(mf)) {
    mf <- mean(data[[m]])
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

  # Beta coefficients estimation (linear regression model for binary mediator m)
  # and Theta coefficients estimation (logistic regression model for binary outcome y)

  Mform <- as.formula(paste(m, "~", paste(c(a, m_cov), collapse = " + ")))

  if (interaction == TRUE) {
    Yform <- as.formula(paste(y, "~", paste(c(paste(a, "*", m), y_cov), collapse = " + ")))
  } else {
    Yform <- as.formula(paste(y, "~", paste(c(a, m, y_cov), collapse = " + ")))
  }

  Yform <- terms(Yform, keep.order = TRUE)

  if (boot == FALSE) {

    if(is.null(yprevalence)) {
      if (Firth == TRUE) {
        coef_estimate <- function(data, Mform, Yform){
          Mreg <- lm(Mform, data = data)
          Yreg <- glm(Yform, data = data, family = binomial(link = "logit"),
                      method = brglmFit, type = "MPL_Jeffreys")

          result <- vector("list", length = 8)
          result[[1]] <- Mreg$coefficients
          result[[2]] <- Yreg$coefficients
          result[[3]] <- sum(Mreg$residuals^2)/Mreg$df.residual
          result[[4]] <- Mreg$df.residual
          result[[5]] <- vcov(Mreg)
          result[[6]] <- vcov(Yreg)
          result[[7]] <- Mreg
          result[[8]] <- Yreg
          names(result[[1]]) <- NULL
          names(result[[2]]) <- NULL

          return(result)
        }
      } else {
        coef_estimate <- function(data, Mform, Yform){
          Mreg <- lm(Mform, data = data)
          Yreg <- glm(Yform, data = data, family = binomial(link = "logit"))

          result <- vector("list", length = 8)
          result[[1]] <- Mreg$coefficients
          result[[2]] <- Yreg$coefficients
          result[[3]] <- sum(Mreg$residuals^2)/Mreg$df.residual
          result[[4]] <- Mreg$df.residual
          result[[5]] <- vcov(Mreg)
          result[[6]] <- vcov(Yreg)
          result[[7]] <- Mreg
          result[[8]] <- Yreg
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
          Mreg <- lm(Mform, data = data, weights = cc_weights)
          Yreg <- suppress_warnings(glm(Yform, data = data, family = binomial(link = "logit"),
                   method = brglmFit, type = "MPL_Jeffreys", weights = cc_weights))

          result <- vector("list", length = 8)
          result[[1]] <- Mreg$coefficients
          result[[2]] <- Yreg$coefficients
          result[[3]] <- sum(Mreg$residuals^2)/Mreg$df.residual
          result[[4]] <- Mreg$df.residual
          result[[5]] <- vcovHC(Mreg)
          result[[6]] <- vcovHC(Yreg)
          result[[7]] <- coeftest(Mreg, vcov. = vcovHC(Mreg))
          result[[8]] <- coeftest(Yreg, vcov. = vcovHC(Yreg))
          names(result[[1]]) <- NULL
          names(result[[2]]) <- NULL

          return(result)
        }
      } else {
        coef_estimate <- function(data, Mform, Yform){
          Mreg <- lm(Mform, data = data, weights = cc_weights)
          Yreg <- suppress_warnings(glm(Yform, data = data, family = binomial(link = "logit"),
                                        weights = cc_weights))

          result <- vector("list", length = 8)
          result[[1]] <- Mreg$coefficients
          result[[2]] <- Yreg$coefficients
          result[[3]] <- sum(Mreg$residuals^2)/Mreg$df.residual
          result[[4]] <- Mreg$df.residual
          result[[5]] <- vcovHC(Mreg)
          result[[6]] <- vcovHC(Yreg)
          result[[7]] <- coeftest(Mreg, vcov. = vcovHC(Mreg))
          result[[8]] <- coeftest(Yreg, vcov. = vcovHC(Yreg))
          names(result[[1]]) <- NULL
          names(result[[2]]) <- NULL

          return(result)
        }
      }
    }

    beta_theta_coef <- coef_estimate(data, Mform, Yform)

    betacoef <- beta_theta_coef[[1]]
    thetacoef <- beta_theta_coef[[2]]
    varcoef <- beta_theta_coef[[3]]

    # Function 'gg' for Nested probabilities P(y(a,m(b)) =1|C=c) and gradient computation

    gg <- function(a, b, betav, varcoef, covmv, thetav, covyv, interaction){

      if(interaction == TRUE) {

        medfunc1 <- function(a, b, betav, varcoef, covmv, thetav, covyv,x){

          au <- exp(-(thetav[1] + thetav[2]*a + thetav[3]*x + thetav[4]*a*x + as.numeric(thetav[-(1:4)]%*%covyv)))

          return(1/(1+au)*exp(-(x-(betav[1] + betav[2]*b + as.numeric(betav[-(1:2)]%*%covmv)))^2/(2*varcoef)))
        }

        medfunc2 <- function(a, b, betav, varcoef, covmv, thetav, covyv,x){

          au <- exp(-(thetav[1] + thetav[2]*a + thetav[3]*x + thetav[4]*a*x + as.numeric(thetav[-(1:4)]%*%covyv)))

          return(x/(1+au)*exp(-(x-(betav[1] + betav[2]*b + as.numeric(betav[-(1:2)]%*%covmv)))^2/(2*varcoef)))
        }

        medfunc3 <- function(a, b, betav, varcoef, covmv, thetav, covyv,x){

          au <- exp(-(thetav[1] + thetav[2]*a + thetav[3]*x + thetav[4]*a*x + as.numeric(thetav[-(1:4)]%*%covyv)))

          return(1/(2+au+1/au)*exp(-(x-(betav[1] + betav[2]*b + as.numeric(betav[-(1:2)]%*%covmv)))^2/(2*varcoef)))
        }

        medfunc4 <- function(a, b, betav, varcoef, covmv, thetav, covyv,x){

          au <- exp(-(thetav[1] + thetav[2]*a + thetav[3]*x + thetav[4]*a*x + as.numeric(thetav[-(1:4)]%*%covyv)))

          return(x/(2+au+1/au)*exp(-(x-(betav[1] + betav[2]*b + as.numeric(betav[-(1:2)]%*%covmv)))^2/(2*varcoef)))
        }

        medfunc5 <- function(a, b, betav, varcoef, covmv, thetav, covyv,x){

          au <- exp(-(thetav[1] + thetav[2]*a + thetav[3]*x + thetav[4]*a*x + as.numeric(thetav[-(1:4)]%*%covyv)))

          return(1/(1+au)*((x-(betav[1] + betav[2]*b + as.numeric(betav[-(1:2)]%*%covmv)))^2)*
                   exp(-(x-(betav[1] + betav[2]*b + as.numeric(betav[-(1:2)]%*%covmv)))^2/(2*varcoef)))
        }


        CPab <- 1/sqrt(2*pi*varcoef)*integrate(medfunc1,-Inf, Inf,a=a, b=b, betav=betav, varcoef=varcoef,
                                               covmv=covmv, thetav=thetav, covyv=covyv, rel.tol=1e-13)$value

        Dgb0 <- 1/(varcoef*sqrt(2*pi*varcoef))*integrate(medfunc2,-Inf, Inf,a=a, b=b, betav=betav, varcoef=varcoef,
                                                         covmv=covmv, thetav=thetav, covyv=covyv, rel.tol=1e-13)$value -
          (betav[1] + betav[2]*b + as.numeric(betav[-(1:2)]%*%covmv))/varcoef*CPab

        Dgb1 <- b*Dgb0
        VDgb2 <- Dgb0*covmv

        Dgt0 <- 1/sqrt(2*pi*varcoef)*integrate(medfunc3,-Inf, Inf,a=a, b=b, betav=betav, varcoef=varcoef,
                                               covmv=covmv, thetav=thetav, covyv=covyv, rel.tol=1e-13)$value

        Dgt1 <- a*Dgt0

        Dgt2 <- 1/sqrt(2*pi*varcoef)*integrate(medfunc4,-Inf, Inf,a=a, b=b, betav=betav, varcoef=varcoef,
                                               covmv=covmv, thetav=thetav, covyv=covyv, rel.tol=1e-13)$value

        Dgt3 <- a*Dgt2
        Dgt4 <- Dgt0*covyv

        Dgt5 <- -1/(2*varcoef)*CPab + 1/(2*varcoef^2*sqrt(2*pi*varcoef))*integrate(medfunc5,-Inf, Inf,a=a, b=b, betav=betav,
                                                                                   varcoef=varcoef,covmv=covmv, thetav=thetav, covyv=covyv, rel.tol=1e-13)$value

      }else {


        medfunc1 <- function(a, b, betav, varcoef, covmv, thetav, covyv,x){

          au <- exp(-(thetav[1] + thetav[2]*a + thetav[3]*x + as.numeric(thetav[-(1:3)]%*%covyv)))

          return(1/(1+au)*exp(-(x-(betav[1] + betav[2]*b + as.numeric(betav[-(1:2)]%*%covmv)))^2/(2*varcoef)))
        }

        medfunc2 <- function(a, b, betav, varcoef, covmv, thetav, covyv,x){

          au <- exp(-(thetav[1] + thetav[2]*a + thetav[3]*x  + as.numeric(thetav[-(1:3)]%*%covyv)))

          return(x/(1+au)*exp(-(x-(betav[1] + betav[2]*b + as.numeric(betav[-(1:2)]%*%covmv)))^2/(2*varcoef)))
        }

        medfunc3 <- function(a, b, betav, varcoef, covmv, thetav, covyv,x){

          au <- exp(-(thetav[1] + thetav[2]*a + thetav[3]*x  + as.numeric(thetav[-(1:3)]%*%covyv)))

          return(1/(2+au+1/au)*exp(-(x-(betav[1] + betav[2]*b + as.numeric(betav[-(1:2)]%*%covmv)))^2/(2*varcoef)))
        }

        medfunc4 <- function(a, b, betav, varcoef, covmv, thetav, covyv,x){

          au <- exp(-(thetav[1] + thetav[2]*a + thetav[3]*x  + as.numeric(thetav[-(1:3)]%*%covyv)))

          return(x/(2+au+1/au)*exp(-(x-(betav[1] + betav[2]*b + as.numeric(betav[-(1:2)]%*%covmv)))^2/(2*varcoef)))
        }

        medfunc5 <- function(a, b, betav, varcoef, covmv, thetav, covyv,x){

          au <- exp(-(thetav[1] + thetav[2]*a + thetav[3]*x  + as.numeric(thetav[-(1:3)]%*%covyv)))

          return(1/(1+au)*((x-(betav[1] + betav[2]*b + as.numeric(betav[-(1:2)]%*%covmv)))^2)*
                   exp(-(x-(betav[1] + betav[2]*b + as.numeric(betav[-(1:2)]%*%covmv)))^2/(2*varcoef)))
        }


        CPab <- 1/sqrt(2*pi*varcoef)*integrate(medfunc1,-Inf, Inf,a=a, b=b, betav=betav, varcoef=varcoef,
                                               covmv=covmv, thetav=thetav, covyv=covyv, rel.tol=1e-13)$value

        Dgb0 <- 1/(varcoef*sqrt(2*pi*varcoef))*integrate(medfunc2,-Inf, Inf,a=a, b=b, betav=betav, varcoef=varcoef,
                                                         covmv=covmv, thetav=thetav, covyv=covyv, rel.tol=1e-13)$value -
          (betav[1] + betav[2]*b + as.numeric(betav[-(1:2)]%*%covmv))/varcoef*CPab

        Dgb1 <- b*Dgb0
        VDgb2 <- Dgb0*covmv

        Dgt0 <- 1/sqrt(2*pi*varcoef)*integrate(medfunc3,-Inf, Inf,a=a, b=b, betav=betav, varcoef=varcoef,
                                               covmv=covmv, thetav=thetav, covyv=covyv, rel.tol=1e-13)$value

        Dgt1 <- a*Dgt0

        Dgt2 <- 1/sqrt(2*pi*varcoef)*integrate(medfunc4,-Inf, Inf,a=a, b=b, betav=betav, varcoef=varcoef,
                                               covmv=covmv, thetav=thetav, covyv=covyv, rel.tol=1e-13)$value


        Dgt3 <- Dgt0*covyv

        Dgt4 <- -1/(2*varcoef)*CPab + 1/(2*varcoef^2*sqrt(2*pi*varcoef))*integrate(medfunc5,-Inf, Inf,a=a, b=b, betav=betav,
                                                                                   varcoef=varcoef,covmv=covmv, thetav=thetav, covyv=covyv, rel.tol=1e-13)$value
        Dgt5 <- NULL

      }

      result <- vector("list", length =2)
      result[[1]] <- CPab
      result[[2]] <- c(Dgb0, Dgb1, VDgb2, Dgt0, Dgt1, Dgt2, Dgt3, Dgt4,Dgt5)

      return(result)

    }

    # Function 'gg_cde' for  probabilities P(y(a,m) =1|C=c) and gradient computation

    gg_cde <- function(a, astar, mf, thetav, covyv, interaction) {
      if (interaction == TRUE) {
        terms_a <- thetav[1] + thetav[2] * a + thetav[3] * mf + thetav[4] * a * mf +
          as.numeric(thetav[-(1:4)] %*% covyv)

        terms_astar <- thetav[1] + thetav[2] * astar + thetav[3] * mf +
          thetav[4] * astar * mf + as.numeric(thetav[-(1:4)] %*% covyv)

        exp_a <- exp(terms_a)
        exp_astar <- exp(terms_astar)
        expit_a <- expit(terms_a)
        expit_astar <- expit(terms_astar)

        OR <- exp(thetav[2] * (a - astar) + thetav[4] * (a - astar) * mf)
        RR <- OR * (1 + exp_astar) / (1 + exp_a)
        RD <- expit_a - expit_astar

        DlnORtheta1 <- 0
        DlnORtheta2 <- a - astar
        DlnRRtheta1 <- -RD
        DlnRRtheta2 <- a / (1 + exp_a) - astar / (1 + exp_astar)
        DlnRDtheta1 <- exp_a / ((1 + exp_a)^2) - exp_astar / ((1 + exp_astar)^2)
        DlnRDtheta2 <- a * exp_a / ((1 + exp_a)^2) - astar * exp_astar / ((1 + exp_astar)^2)

        gradlnOR <- c(DlnORtheta1, DlnORtheta2, mf * DlnORtheta1, mf * DlnORtheta2, covyv * DlnORtheta1)
        gradlnRR <- c(DlnRRtheta1, DlnRRtheta2, mf * DlnRRtheta1, mf * DlnRRtheta2, covyv * DlnRRtheta1)
        gradlnRD <- c(DlnRDtheta1, DlnRDtheta2, mf * DlnRDtheta1, mf * DlnRDtheta2, covyv * DlnRDtheta1)
      } else {
        terms_a <- thetav[1] + thetav[2] * a + thetav[3] * mf  + as.numeric(thetav[-(1:3)] %*% covyv)
        terms_astar <- thetav[1] + thetav[2] * astar + thetav[3] * mf  + as.numeric(thetav[-(1:3)] %*% covyv)

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

        gradlnOR <- c(DlnORtheta1, DlnORtheta2, mf * DlnORtheta1, covyv * DlnORtheta1)
        gradlnRR <- c(DlnRRtheta1, DlnRRtheta2, mf * DlnRRtheta1, covyv * DlnRRtheta1)
        gradlnRD <- c(DlnRDtheta1, DlnRDtheta2, mf * DlnRDtheta1, covyv * DlnRDtheta1)
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

    gg10 <- gg(a1, a0, betacoef,varcoef, mean_covmv, thetacoef,mean_covyv,interaction)
    gg00 <- gg(a0, a0, betacoef,varcoef, mean_covmv, thetacoef,mean_covyv,interaction)
    gg11 <- gg(a1, a1, betacoef,varcoef, mean_covmv, thetacoef,mean_covyv,interaction)

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

    residual <- beta_theta_coef[[4]]
    Sigmabeta <- beta_theta_coef[[5]]
    Sigmatheta <- beta_theta_coef[[6]]

    l1 <- length(betacoef)
    l2 <- length(thetacoef)
    l <- l1 + l2 + 1

    Sigma <- matrix(0, nrow = l, ncol = l)
    Sigma[1:l1, 1:l1] <- Sigmabeta
    Sigma[(l1 + 1):(l-1), (l1 + 1):(l-1)] <- Sigmatheta
    Sigma[l,l] <- 2*varcoef^2/(residual + 2)

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

    # Controlled direct effects and gradient computations (m=mf)

    gg_cdem <- gg_cde(a = a1, astar = a0, m = mf, thetav = thetacoef,
                       covyv = mean_covyv, interaction = interaction)

    # Controlled effects (m=mf)

    ORm <- gg_cdem$OR[[1]]
    RRm <- gg_cdem$RR[[1]]
    RDm <- gg_cdem$RD[[1]]

    # Confidence intervals computation (m=mf)

    gradlnORm <- gg_cdem$OR[[2]]
    gradlnRRm <- gg_cdem$RR[[2]]
    gradRDm <- gg_cdem$RD[[2]]

    selnORm <- sqrt(as.numeric(gradlnORm %*% Sigmatheta %*% gradlnORm))
    selnRRm <- sqrt(as.numeric(gradlnRRm %*% Sigmatheta %*% gradlnRRm))
    seRDm <- sqrt(as.numeric(gradRDm %*% Sigmatheta %*% gradRDm))

    CI_ORm <- ORm * c(int0, int1)^selnORm
    CI_RRm <- RRm * c(int0, int1)^selnRRm
    CI_RDm <- RDm + seRDm * log(c(int0, int1))

    lnORm <- log(ORm)
    lnRRm <- log(RRm)

    zORm <- lnORm / selnORm
    zRRm <- lnRRm / selnRRm
    zRDm <- RDm / seRDm

    pvalueORm <- 2 * (1 - pnorm(abs(zORm)))
    pvalueRRm <- 2 * (1 - pnorm(abs(zRRm)))
    pvalueRDm <- 2 * (1 - pnorm(abs(zRDm)))

    seORm <- ORm * selnORm
    seRRm <- RRm * selnRRm

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

    ContEffm <- matrix(0, nrow = 3, ncol = 5)

    rownames(ContEffm) <- c("OR scale", "RR scale", "RD scale")
    colnames(ContEffm) <- c("Estimate", "Std.error", CIinf, CIsup, "P.val")

    ContEffm[1, ] <- c(ORm, seORm, CI_ORm, pvalueORm)
    ContEffm[2, ] <- c(RRm, seRRm, CI_RRm, pvalueRRm)
    ContEffm[3, ] <- c(RDm, seRDm, CI_RDm, pvalueRDm)


    results <- vector("list", 6)
    names(results) <- c(
      "ne.or",
      "ne.rr",
      "ne.rd",
      "cde",
      "med.reg",
      "out.reg"
    )

    OR <- as.data.frame(OR)
    RR <- as.data.frame(RR)
    RD <- as.data.frame(RD)
    ContEffm <- as.data.frame(ContEffm)

    OR[[5]] <- format.pval(OR[[5]], digits = 5)
    RR[[5]] <- format.pval(RR[[5]], digits = 5)
    RD[[5]] <- format.pval(RD[[5]], digits = 5)
    ContEffm[[5]] <- format.pval(ContEffm[[5]], digits = 5)

    results[[1]] <- cbind(round(OR[1:4], digits = 5), OR[5])
    results[[2]] <- cbind(round(RR[1:4], digits = 5), RR[5])
    results[[3]] <- cbind(round(RD[1:4], digits = 5), RD[5])
    results[[4]] <- cbind(round(ContEffm[1:4], digits = 5), ContEffm[5])
    results[[5]] <- beta_theta_coef[[7]]
    results[[6]] <- beta_theta_coef[[8]]

    class(results) <- c("results_c", "list")

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
          Mreg <- lm(Mform, data = data)
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
          Mreg <- lm(Mform, data = data)
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
          Mreg <- lm(Mform, data = data, weights = cc_weights)
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
          Mreg <- lm(Mform, data = data, weights = cc_weights)
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
    varcoef <- sum(beta_theta_coef_ini$Mreg$residuals^2) / beta_theta_coef_ini$Mreg$df.residual

    gg <- function(a, b, betav, varcoef, covmv, thetav, covyv, interaction){

      if(interaction == TRUE) {

        medfunc1 <- function(a, b, betav, varcoef, covmv, thetav, covyv,x){

          au <- exp(-(thetav[1] + thetav[2]*a + thetav[3]*x + thetav[4]*a*x + as.numeric(thetav[-(1:4)]%*%covyv)))

          return(1/(1+au)*exp(-(x-(betav[1] + betav[2]*b + as.numeric(betav[-(1:2)]%*%covmv)))^2/(2*varcoef)))
        }



        CPab <- 1/sqrt(2*pi*varcoef)*integrate(medfunc1,-Inf, Inf,a=a, b=b, betav=betav, varcoef=varcoef,
                                               covmv=covmv, thetav=thetav, covyv=covyv, rel.tol=1e-13)$value


      }else {


        medfunc1 <- function(a, b, betav, varcoef, covmv, thetav, covyv,x){

          au <- exp(-(thetav[1] + thetav[2]*a + thetav[3]*x + as.numeric(thetav[-(1:3)]%*%covyv)))

          return(1/(1+au)*exp(-(x-(betav[1] + betav[2]*b + as.numeric(betav[-(1:2)]%*%covmv)))^2/(2*varcoef)))
        }



        CPab <- 1/sqrt(2*pi*varcoef)*integrate(medfunc1,-Inf, Inf,a=a, b=b, betav=betav, varcoef=varcoef,
                                               covmv=covmv, thetav=thetav, covyv=covyv, rel.tol=1e-13)$value


      }

      return(CPab)

    }

    gg_cde <- function(a, astar, mf, thetav, covyv, interaction) {
      if (interaction == TRUE) {
        terms_a <- thetav[1] + thetav[2] * a + thetav[3] * mf + thetav[4] * a * mf +
          as.numeric(thetav[-(1:4)] %*% covyv)

        terms_astar <- thetav[1] + thetav[2] * astar + thetav[3] * mf +
          thetav[4] * astar * mf + as.numeric(thetav[-(1:4)] %*% covyv)

        exp_a <- exp(terms_a)
        exp_astar <- exp(terms_astar)
        expit_a <- expit(terms_a)
        expit_astar <- expit(terms_astar)

        OR <- exp(thetav[2] * (a - astar) + thetav[4] * (a - astar) * mf)
        RR <- OR * (1 + exp_astar) / (1 + exp_a)
        RD <- expit_a - expit_astar
      } else {
        terms_a <- thetav[1] + thetav[2] * a + thetav[3] * mf  + as.numeric(thetav[-(1:3)] %*% covyv)
        terms_astar <- thetav[1] + thetav[2] * astar + thetav[3] * mf  + as.numeric(thetav[-(1:3)] %*% covyv)

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

    P10 <- gg(a1, a0, betacoef,varcoef, mean_covmv, thetacoef, mean_covyv, interaction)
    P00 <- gg(a0, a0, betacoef,varcoef, mean_covmv, thetacoef, mean_covyv, interaction)
    P11 <- gg(a1, a1, betacoef,varcoef, mean_covmv, thetacoef, mean_covyv, interaction)

    ORd <- (P10 / (1 - P10)) / (P00 / (1 - P00))
    ORi <- (P11 / (1 - P11)) / (P10 / (1 - P10))
    ORt <- ORd * ORi

    RRd <- P10 / P00
    RRi <- P11 / P10
    RRt <- RRd * RRi

    RDd <- P10 - P00
    RDi <- P11 - P10
    RDt <- RDd + RDi

    gg_cdem <- gg_cde(a = a1, astar = a0, m = mf, thetav = thetacoef,
                       covyv = mean_covyv, interaction = interaction)

    ORm <- gg_cdem$OR
    RRm <- gg_cdem$RR
    RDm <- gg_cdem$RD

    set.seed(bootseed)

    n <- nrow(data)

    ORboot <- matrix(0, nboot, 3)
    RRboot <- matrix(0, nboot, 3)
    RDboot <- matrix(0, nboot, 3)

    colnames(ORboot) <- c("Direct effect", "Indirect effect", "Total effect")
    colnames(RRboot) <- c("Direct effect", "Indirect effect", "Total effect")
    colnames(RDboot) <- c("Direct effect", "Indirect effect", "Total effect")

    ORmboot <- vector("numeric", length = nboot)
    RRmboot <- vector("numeric", length = nboot)
    RDmboot <- vector("numeric", length = nboot)

    indiceboot <- matrix(0, n, nboot)

    progress_bar <- txtProgressBar(min = 0, max = nboot, style = 3, char = "=")

    for (i in 1:nboot) {
      repli_list <- repli_data(data, n, y)
      DATAboot <- repli_list[[1]]
      indiceboot[, i] <- repli_list[[2]]

      beta_theta_coef <- coef_estimate(DATAboot, Mform, Yform)

      betacoef <- beta_theta_coef$Mreg$coefficients
      thetacoef <- beta_theta_coef$Yreg$coefficients
      varcoef <- sum(beta_theta_coef$Mreg$residuals^2) / beta_theta_coef$Mreg$df.residual

      P10 <- gg(a1, a0, betacoef,varcoef, mean_covmv, thetacoef,mean_covyv,interaction)
      P00 <- gg(a0, a0, betacoef,varcoef, mean_covmv, thetacoef,mean_covyv,interaction)
      P11 <- gg(a1, a1, betacoef,varcoef, mean_covmv, thetacoef,mean_covyv,interaction)

      ORboot[i, 1] <- (P10 / (1 - P10)) / (P00 / (1 - P00))
      ORboot[i, 2] <- (P11 / (1 - P11)) / (P10 / (1 - P10))
      ORboot[i, 3] <- ORboot[i, 1] * ORboot[i, 2]

      RRboot[i, 1] <- P10 / P00
      RRboot[i, 2] <- P11 / P10
      RRboot[i, 3] <- RRboot[i, 1] * RRboot[i, 2]

      RDboot[i, 1] <- P10 - P00
      RDboot[i, 2] <- P11 - P10
      RDboot[i, 3] <- RDboot[i, 1] + RDboot[i, 2]

      gg_cdem <- gg_cde(a = a1, astar = a0, m = mf, thetav = thetacoef,
                         covyv = mean_covyv, interaction = interaction)

      ORmboot[i] <- gg_cdem$OR
      RRmboot[i] <- gg_cdem$RR
      RDmboot[i] <- gg_cdem$RD

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

    CI_ORm <- quantile(ORmboot, c(ciinf, cisup), na.rm = TRUE)
    CI_RRm <- quantile(RRmboot, c(ciinf, cisup), na.rm = TRUE)
    CI_RDm <- quantile(RDmboot, c(ciinf, cisup), na.rm = TRUE)


    seORd <- sd(ORboot[, 1], na.rm = TRUE)
    seORi <- sd(ORboot[, 2], na.rm = TRUE)
    seORt <- sd(ORboot[, 3], na.rm = TRUE)

    seRRd <- sd(RRboot[, 1], na.rm = TRUE)
    seRRi <- sd(RRboot[, 2], na.rm = TRUE)
    seRRt <- sd(RRboot[, 3], na.rm = TRUE)

    seRDd <- sd(RDboot[, 1], na.rm = TRUE)
    seRDi <- sd(RDboot[, 2], na.rm = TRUE)
    seRDt <- sd(RDboot[, 3], na.rm = TRUE)

    seORm <- sd(ORmboot, na.rm = TRUE)
    seRRm <- sd(RRmboot, na.rm = TRUE)
    seRDm <- sd(RDmboot, na.rm = TRUE)

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

    ContEffm <- matrix(0, nrow = 3, ncol = 4)

    rownames(ContEffm) <- c("OR scale", "RR scale", "RD scale")
    colnames(ContEffm) <- c("Estimate", "Std.error", CIinf, CIsup)

    ContEffm[1, ] <- c(ORm, seORm, CI_ORm)
    ContEffm[2, ] <- c(RRm, seRRm, CI_RRm)
    ContEffm[3, ] <- c(RDm, seRDm, CI_RDm)

    results <- vector("list", 13)
    names(results) <- c(
      "ne.or",
      "ne.rr",
      "ne.rd",
      "cde",
      "boot.ne.or",
      "boot.ne.rr",
      "boot.ne.rd",
      "boot.cde.or",
      "boot.cde.rr",
      "boot.cde.rd",
      "boot.ind",
      "med.reg",
      "out.reg"
    )

    results[[1]] <- round(OR, digits = 5)
    results[[2]] <- round(RR, digits = 5)
    results[[3]] <- round(RD, digits = 5)
    results[[4]] <- round(ContEffm, digits = 5)
    results[[5]] <- ORboot
    results[[6]] <- RRboot
    results[[7]] <- RDboot
    results[[8]] <- ORmboot
    results[[9]] <- RRmboot
    results[[10]] <- RDmboot
    results[[11]] <- indiceboot
    results[[12]] <- beta_theta_coef_ini$Mreg
    results[[13]] <- beta_theta_coef_ini$Yreg

    class(results) <- c("results_c", "list")

    close(progress_bar)
  }

  return(results)
}
