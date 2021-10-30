#' @title  Exact Mediation Effects Computation
#' @description The EXACTMED function calculates popular causal mediation effects when the outcome and
#'     the mediator are binary. More precisely, it returns point and interval estimates for the conditional
#'     natural direct and indirect effects without making any assumption regarding the rareness or commonness
#'     of the outcome (hence the term exact). EXACTMED adopts a logistic regression specification for both
#'     the outcome and mediator in order to compute the exact closed-form effects estimators (see the details in Samoilenko and Lefebvre, 2021).
#'     For completeness, the EXACTMED function also calculates the conditional controlled direct effects
#'     at both values of the mediator. Natural and controlled effects estimates are reported in three different
#'     scales: odds ratio (OR), risk ratio (RR) and risk difference (RD). The interval estimates can be
#'     obtained either by the delta method or the bootstrap.
#' @param DATA a named data frame that includes the outcome, exposure and mediator variables as well as the covariates
#'     to be adjusted for in the model. The exposure can be either binary or continuous. If a covariate is categorical,
#'     it has to be included in the data frame as a factor, character or logical variable.
#' @param A the name of the exposure variable.
#' @param M the name of the mediator variable.
#' @param Y the name of the outcome variable.
#' @param a1 a value corresponding to the high level of the exposure.
#' @param a0 a value corresponding to the low level of the exposure.
#' @param M_COV a vector containing the names of the adjustment variables (covariates) in the mediator model.
#' @param Y_COV a vector containing the names of the adjustment variables (covariates) in the outcome model.
#' @param M_COV_cond a named vector (atomic vector or list) containing specific values for some or all
#'     of the adjustment covariates \code{M_COV} in the mediator model. Please consult the package vignette for details.
#' @param Y_COV_cond a named vector (atomic vector or list) containing specific values for some or all
#'     of the adjustment covariates \code{Y_COV} in the outcome model. Please consult the package vignette for details.
#' @param adjusted  a logical variable specifying whether to obtain unadjusted or adjusted estimates.
#'     If \code{adjusted == FALSE}, vectors \code{M_COV} and \code{Y_COV} are ignored by the procedure.
#' @param interaction a logical variable specifying whether there is exposure-mediator interaction in the outcome model.
#' @param Firth a logical variable specifying whether to compute conventional maximum likelihood estimates
#'     or Firth  penalized estimates in the logistic regression models.
#' @param boot a logical value specifying whether the confidence intervals are obtained
#'     by the delta method or by the bootstrap.
#' @param nboot  If \code{boot == TRUE}, the number of bootstrap replications to obtain the confidence intervals.
#' @param bootseed If \code{boot == TRUE}, the value of the initial seed (positive integer) for random number generation.
#' @param confcoef a number between 0 and 1 for the confidence coefficient (ex:0.95) for the interval estimates.
#' @param hvalueM the value corresponding to the high level of the mediator. If the mediator is already coded
#'     as a numerical binary variable taking 0 or 1 values, then the value of this parameter will be ignored.
#' @param hvalueY the value corresponding to the high level of the outcome. If the outcome is already coded
#'     as a numerical binary variable taking 0 or 1 values, then the value of this parameter will be ignored.
#' @importFrom logistf logistf
#' @importFrom stats as.formula binomial glm qnorm quantile terms vcov na.omit
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @details By default, EXACTMED reports mediation effects evaluated at the sample-specific mean values of the numerical covariates
#'     (including the dummy variables created internally by the function to represent the categorical covariates).
#'     In order to estimate mediation effects at specific values of some covariates (that is, stratum-specific effects)
#'     the user needs to provide named vectors \code{M_COV_cond} and/or \code{Y_COV_cond} containing those values or levels. The adjustment
#'     covariates appearing in both \code{M_COV} and \code{Y_COV} (common adjustment covariates) must have the same values; otherwise,
#'     EXACTMED's execution is aborted and an error message is displayed in the R console.
#' @return Returns natural direct, indirect and total effect estimates as well as controlled direct effects
#'     estimates on the OR, RR and RD scales.
#' @note EXACTMED only works for complete data. Users can apply multiple imputation techniques (e.g., R package \emph{mice})
#'  or remove observations with any missing values (NA) by specifying \code{data = na.omit(mydataset)}.
#' @references
#' Samoilenko M, Lefebvre G. (2021). Parametric Regression-Based Causal Mediation Analysis of Binary Outcomes
#' and Binary Mediators: Moving Beyond the Rareness or Commonness of the Outcome. American journal of epidemiology, kwab055. Advance online publication. <doi:10.1093/aje/kwab055>
#'
#' Samoilenko M, Blais L, Lefebvre G. (2018). Comparing logistic and log-binomial models for causal mediation analyses of binary mediators and rare binary outcomes:
#' evidence to support cross-checking of mediation results in practice. Observational Studies; 2018(4):193-216.
#' @export
#' @examples
#' EXACTMED(DATA=datamed, A='X', M='M', Y='Y', a1=1, a0=0, M_COV=c('C1', 'C2'), Y_COV=c('C1', 'C2'))
#'
#' M_COV_cond <-c(C1 =0.1, C2 =0.4)
#'
#' Y_COV_cond <-c(C1 =0.1, C2 =0.4)
#'
#' EXACTMED(DATA=datamed, A='X', M='M', Y='Y', a1=1, a0=0, M_COV=c('C1', 'C2'),
#'          Y_COV=c('C1', 'C2'), M_COV_cond=M_COV_cond, Y_COV_cond=Y_COV_cond)
#'
#' C1b <- factor(sample(c("a","b","c"), nrow(datamed), replace =TRUE))
#'
#' datamed$C1 <- C1b
#'
#' M_COV_cond <-list(C1='c', C2=0.4)
#'
#' Y_COV_cond <-list(C1='c', C2=0.4)
#'
#' EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
#' Y_COV=c('C1', 'C2'), M_COV_cond=M_COV_cond, Y_COV_cond=Y_COV_cond)


EXACTMED <- function(DATA, A, M, Y, a1, a0, M_COV=NULL, Y_COV=NULL, M_COV_cond=NULL,
                     Y_COV_cond=NULL, adjusted=TRUE, interaction=TRUE, Firth = FALSE,
                     boot = FALSE, nboot=500, bootseed =1991, confcoef = 0.95,
                     hvalueM =NULL,hvalueY=NULL){



  # Input parameters checking

  if( !(is.data.frame(DATA) && !is.null(colnames(DATA)))) stop("'DATA' must be a data frame with column names")

  if(any(is.na(DATA))) stop("'DATA' contains missing values")

  if(any(duplicated(colnames(DATA)))) stop("'DATA' has duplicated column names")

  if(any(is.na(colnames(DATA))) || any(colnames(DATA) == "")) stop("'DATA' has some unnamed columns")


  if(!(is.vector(A,mode="character") && length(A)==1L && A %in% colnames(DATA) )){

    stop("'A' has to be a column name of 'DATA'")
  }



  if(!(is.vector(M,mode="character") && length(M)==1L && M %in% colnames(DATA) )){

    stop("'M' has to be a column name of 'DATA'")
  }



  if(!(is.vector(Y,mode="character") && length(Y)==1L && Y %in% colnames(DATA) )){

    stop("'Y' has to be a column name of 'DATA'")
  }



  if(!(is.null(hvalueM) || (is.atomic(hvalueM) && length(hvalueM)==1L && is.null(dim(hvalueM)) && !is.na(hvalueM)))){

    stop("Invalid type or length for input parameter 'hvalueM'")
  }

  if(!(is.null(hvalueY) || (is.atomic(hvalueY) && length(hvalueY)==1L && is.null(dim(hvalueY)) && !is.na(hvalueY)))){

    stop("Invalid type or length for input parameter 'hvalueY'")
  }


  if(!is.numeric(DATA[[A]]))  stop("Exposure must be numerical variable")


  if(length(unique(DATA[[M]])) > 2){

    stop("Mediator takes more than two different values in 'DATA'")

  }


  if(is.factor(DATA[[M]])){

    if(is.null(hvalueM)) stop("High level for the mediator must be specified")

    if(! hvalueM %in% levels(DATA[[M]])) {

      stop ("Invalid value for high level of mediator")
    }


    lv <- vector("integer", length=2L)
    hl <- which(levels(DATA[[M]])==hvalueM)
    lv[hl] <- 1L

    levels(DATA[[M]]) <- lv
    DATA[[M]] <- as.integer(as.character(DATA[[M]]))


  }else if (!(is.numeric(DATA[[M]]) &&  all(DATA[[M]] %in% c(1,0)))){

    if(is.null(hvalueM)) stop("High level for the mediator must be specified")

    if(! hvalueM %in% DATA[[M]]) {

      stop ("Invalid value for high level of mediator")
    }

    lv <- vector("integer", length=nrow(DATA))
    hl <- which(DATA[[M]]==hvalueM)
    lv[hl] <- 1L

    DATA[[M]] <- lv

  }


  if(length(unique(DATA[[Y]])) > 2){

    stop("Outcome takes more than two different values in 'DATA'")

  }


  if(is.factor(DATA[[Y]])){

    if(is.null(hvalueY)) stop("High level for the outcome must be specified")

    if(! hvalueY %in% levels(DATA[[Y]])) {

      stop ("Invalid value for high level of outcome")
    }


    lv <- vector("integer", length=2L)
    hl <- which(levels(DATA[[Y]])==hvalueY)
    lv[hl] <- 1L

    levels(DATA[[Y]]) <- lv
    DATA[[Y]] <- as.integer(as.character(DATA[[Y]]))


  }else if (!(is.numeric(DATA[[Y]]) &&  all(DATA[[Y]] %in% c(1,0)))){

    if(is.null(hvalueY)) stop("High level for the outcome must be specified")

    if(! hvalueY %in% DATA[[Y]]) {

      stop ("Invalid value for high level of outcome")
    }

    lv <- vector("integer", length=nrow(DATA))
    hl <- which(DATA[[Y]]==hvalueY)
    lv[hl] <- 1L

    DATA[[Y]] <- lv

  }


  for(i in setdiff(colnames(DATA), c(A,M,Y))){

    if(!(is.numeric(DATA[[i]]) || is.factor(DATA[[i]]))){

      DATA[[i]] <- as.factor(DATA[[i]])


    }


  }


  if(!(is.vector(a1,mode="numeric") && length(a1)==1L)) stop("'a1' has to be a real number")

  if(!(is.vector(a0,mode="numeric") && length(a0)==1L)) stop("'a0' has to be a real number")

  if(!(is.vector(confcoef,mode="numeric") && length(confcoef)==1L && 0 < confcoef && confcoef < 1)){

    stop("'confcoef' has to be a valid real number")

  }

  if (!(is.vector(boot, mode='logical') && length(boot)==1L) || is.na(boot)){

    stop("'boot' must specify a logical value")

  }

  if(boot==TRUE){

    if(!(is.vector(nboot,mode="numeric") && length(nboot)==1L && round(nboot)==nboot )){

      stop("'nboot' has to be an integer")
    }

    if(!(is.vector(bootseed,mode="numeric") && length(bootseed)==1L && round(bootseed)==bootseed )){

      stop("'bootseed' has to be an integer")

    }

  }


  if (!(is.vector(interaction, mode='logical') && length(interaction)==1L) || is.na(interaction)){

    stop("'interaction' must specify a logical value")

  }

  if (!(is.vector(Firth, mode='logical') && length(Firth)==1L) || is.na(Firth)){

    stop("'Firth' must specify a logical value")

  }

  if (!(is.vector(adjusted, mode='logical') && length(adjusted)==1L) || is.na(adjusted)){

    stop("'adjusted' must specify a logical value")

  }

  if(adjusted ==TRUE && is.null(M_COV) && is.null(Y_COV)){

    message("'EXACTMED' will compute unadjusted natural effects")

  }


  if(adjusted ==FALSE && !(is.null(M_COV) && is.null(Y_COV))){

    message("'EXACTMED' will compute unadjusted natural effects")

  }


  if(!(is.null(M_COV) || is.vector(M_COV,mode="character"))){

    stop("'M_COV' must be NULL or a vector of covariate names")

  }

  if(any(is.na(M_COV))) stop("'M_COV' has NAs")

  if(any(duplicated(M_COV))) stop("'M_COV' has duplicated covariates names")


  if(!all(M_COV %in% setdiff(colnames(DATA), c(A,M,Y)))){

    stop("'M_COV' can only contain names of covariates included in the data frame")
  }






  if(!(is.null(Y_COV) || is.vector(Y_COV,mode="character"))){

    stop("'Y_COV' must be NULL or a vector of covariate names")

  }

  if(any(is.na(Y_COV))) stop("'Y_COV' has NAs")

  if(any(duplicated(Y_COV))) stop("'Y_COV' has duplicated covariates names")


  if(!all(Y_COV %in% setdiff(colnames(DATA), c(A,M,Y)))){

    stop("'Y_COV' can only contain names of covariates included in the data frame")
  }



  if(!(is.null(M_COV_cond) || is.vector(M_COV_cond))){


    stop("'M_COV_cond' must be NULL or a vector")

  }

  if(any(is.na(names(M_COV_cond))) || any(names(M_COV_cond) == "")){

    stop("'M_COV_cond' has missing names")
  }

  if(any(duplicated(names(M_COV_cond)))) stop("'M_COV_cond' has duplicated names")


  if(!all(names(M_COV_cond) %in% M_COV)) {

    stop("The names of the elements of 'M_COV_cond' must be in 'M_COV'")
  }


  if(!(is.null(Y_COV_cond) || is.vector(Y_COV_cond))){


    stop("'Y_COV_cond' must be NULL or a vector")

  }

  if(any(is.na(names(Y_COV_cond))) || any(names(Y_COV_cond) == "")){

    stop("'Y_COV_cond' has missing names")
  }

  if(any(duplicated(names(Y_COV_cond)))) stop("'Y_COV_cond' has duplicated names")


  if(!all(names(Y_COV_cond) %in% Y_COV)) {

    stop("The names of the elements of 'Y_COV_cond' must be in 'Y_COV'")
  }


  if(!is.null(M_COV_cond)){

    if(is.null(names(M_COV_cond))) stop("'M_COV_cond' must be a named vector")

    for(i in names(M_COV_cond)){

      if(!(is.atomic(M_COV_cond[[i]]) && length(M_COV_cond[[i]]) ==1L && is.null(dim(M_COV_cond[[i]])) && !is.na(M_COV_cond[[i]]))){

        stop("'M_COV_cond' has a invalid value in the ", i," component")

      }

    }


  }

  if(!is.null(Y_COV_cond)){

    if(is.null(names(Y_COV_cond))) stop("'Y_COV_cond' must be a named vector")

    for(i in names(Y_COV_cond)){

      if(!(is.atomic(Y_COV_cond[[i]]) && length(Y_COV_cond[[i]]) ==1L && is.null(dim(Y_COV_cond[[i]])) && !is.na(Y_COV_cond[[i]]))){

        stop("'Y_COV_cond' has a invalid value in the ", i," component")

      }

    }

  }


  if(!is.null(M_COV_cond)){

    for(i in names(M_COV_cond)){

      if(i %in% Y_COV){

        if(i %in% names(Y_COV_cond)){

          if(M_COV_cond[[i]]!= Y_COV_cond[[i]]) {

            stop("Covariate ",i," has two different values specified")

          }


        }else{

          stop("Covariate ",i," has two different values specified (one implicitly)")

        }


      }

    }

  }

  if(!is.null(Y_COV_cond)){

    for(i in names(Y_COV_cond)){

      if(i %in% M_COV && !(i %in% names(M_COV_cond))){

       stop("Covariate ",i," has two different values specified (one implicitly)")

      }

    }

  }



  expit <- function(x){

    return(exp(x)/(1 + exp(x)))

  }

  if(!adjusted ==TRUE){

    M_COV <- NULL
    Y_COV <- NULL

  }


  mean_covmv <-numeric(0)

  if(!is.null(M_COV)){

    for( i in M_COV){

      if(is.factor(DATA[[i]])){


        if(i %in% names(M_COV_cond)){


          if(!(M_COV_cond[[i]] %in% levels(DATA[[i]]))){

            stop("Invalid value for ",i," covariate" )

          }

          auxv <- vector("numeric",nlevels(DATA[[i]])-1L)

          auxl <- which(levels(DATA[[i]])== M_COV_cond[[i]])-1

          if(auxl!=0L) {auxv[auxl] <- 1}

          mean_covmv <- c(mean_covmv,auxv)

        }else{

          freqcat <- summary(DATA[[i]])[-1]/nrow(DATA)

          names(freqcat) <- NULL

          mean_covmv <- c(mean_covmv,freqcat)

        }


      }else{

        if(i %in% names(M_COV_cond)){

          if(!(is.vector(M_COV_cond[[i]],mode="numeric") && length(M_COV_cond[[i]])==1L)){

            stop("Invalid value for ",i," covariate" )

          }


          mean_covmv <- c(mean_covmv,M_COV_cond[[i]] )

        }else{

          mean_covmv <- c(mean_covmv, mean(DATA[[i]]))
        }

      }

    }

  }

  mean_covyv <-numeric(0)

  if(!is.null(Y_COV)){

    for( i in Y_COV){

      if(is.factor(DATA[[i]])){


        if(i %in% names(Y_COV_cond)){

          if(!(Y_COV_cond[[i]] %in% levels(DATA[[i]]))){

            stop("Invalid value for ",i," covariate" )

          }


          auxv <- vector("numeric",nlevels(DATA[[i]])-1)

          auxl <- which(levels(DATA[[i]])== Y_COV_cond[[i]])-1

          if(auxl!=0L) {auxv[auxl] <- 1}


          mean_covyv <- c(mean_covyv,auxv)

        }else{

          freqcat <- summary(DATA[[i]])[-1]/nrow(DATA)

          names(freqcat) <- NULL

          mean_covyv <- c(mean_covyv,freqcat)

        }


      }else{

        if(i %in% names(Y_COV_cond)){

          if(!(is.vector(Y_COV_cond[[i]],mode="numeric") && length(Y_COV_cond[[i]])==1L)){

            stop("Invalid value for ",i," covariate" )

          }

          mean_covyv <- c(mean_covyv,Y_COV_cond[[i]] )

        }else{

          mean_covyv <- c(mean_covyv, mean(DATA[[i]]))
        }

      }

    }

  }


  names(mean_covmv) <- NULL
  names(mean_covyv) <- NULL

  # Beta coefficients estimation (logistic regression model for binary mediator M)
  # and Theta coefficients estimation (logistic regression model for binary outcome Y)

  Mform  <- as.formula(paste(M,"~",paste(c(A,M_COV), collapse = " + ")))

  if(interaction == TRUE){

    Yform <- as.formula(paste(Y,"~",paste(c(paste(A,"*",M),Y_COV), collapse = " + ")))

  } else{

    Yform <- as.formula(paste(Y,"~",paste(c(A, M, Y_COV), collapse = " + ")))

  }

  Yform <- terms(Yform, keep.order = TRUE)

  if(Firth == TRUE){

    Mreg <- logistf(Mform, data= DATA)
    Yreg <- logistf(Yform, data= DATA)

  } else{

    Mreg <- glm(Mform, data= DATA, family =binomial(link = "logit"))
    Yreg <- glm(Yform, data= DATA, family =binomial(link = "logit"))

  }


  betacoef <- Mreg$coefficients
  thetacoef <- Yreg$coefficients

  names(betacoef) <- NULL
  names(thetacoef) <- NULL

  # Function 'gg' for Nested probabilities P(Y(a,M(b)) =1|C=c) and gradient computation


  if(boot == FALSE){

    gg <- function(a,b, betav,covmv,thetav,covyv, interaction){

      if(interaction ==TRUE){

        probY1M1 <- expit(thetav[1] + thetav[3] +
                            (thetav[2] + thetav[4])*a + as.numeric(thetav[-(1:4)]%*%covyv))

        probY1M0 <- expit(thetav[1] + thetav[2]*a + as.numeric(thetav[-(1:4)]%*%covyv))

      } else{

        probY1M1 <- expit(thetav[1] + thetav[3] +
                            thetav[2]*a + as.numeric(thetav[-(1:3)]%*%covyv))

        probY1M0 <- expit(thetav[1] + thetav[2]*a + as.numeric(thetav[-(1:3)]%*%covyv))

      }


      probM1 <- expit(betav[1] + betav[2]*b + as.numeric(betav[-(1:2)]%*%covmv))
      probM0 <- 1- probM1

      Dgb0 <- probM1*probM0*(probY1M1 - probY1M0)
      Dgb1 <- b*Dgb0
      VDgb2 <- Dgb0*covmv

      Dgt0 <- probY1M1*(1-probY1M1)*probM1 + probY1M0*(1-probY1M0)*probM0
      Dgt1 <- a*Dgt0
      Dgt2 <- probY1M1*(1-probY1M1)*probM1


      if(interaction ==TRUE){

        Dgt3 <- a*Dgt2
        Dgt4 <- Dgt0*covyv

      } else{

        Dgt3 <- Dgt0*covyv
        Dgt4 <- NULL

      }

      result <- vector("list", length =2)

      result[[1]] <- probY1M1*probM1 + probY1M0*probM0
      result[[2]] <- c(Dgb0, Dgb1, VDgb2, Dgt0, Dgt1, Dgt2, Dgt3, Dgt4)

      return(result)

    }


    # Function 'gg_cde' for  probabilities P(Y(a,m) =1|C=c) and gradient computation


    gg_cde <- function(a,m,thetav,covyv, interaction){

      if(interaction == TRUE){

        P <- expit(thetav[1] + thetav[2]*a + thetav[3]*m +
                     thetav[4]*a*m + as.numeric(thetav[-(1:4)]%*%covyv))

        Dgt0 <- P*(1-P)
        Dgt1 <- a*Dgt0
        Dgt2 <- m*Dgt0
        Dgt3 <- a*Dgt2
        Dgt4 <- Dgt0*covyv


      }else{

        P <- expit(thetav[1] + thetav[2]*a + thetav[3]*m +
                     as.numeric(thetav[-(1:3)]%*%covyv))

        Dgt0 <- P*(1-P)
        Dgt1 <- a*Dgt0
        Dgt2 <- m*Dgt0
        Dgt3 <- Dgt0*covyv
        Dgt4 <- NULL

      }

      result <- vector("list", length =2)

      result[[1]] <- P
      result[[2]] <- c(Dgt0, Dgt1, Dgt2, Dgt3, Dgt4)

      return(result)


    }


    # Nested probabilities P(Y(a,M(b)) =1|C=c) and gradient computation

    gg10 <- gg(a1, a0, betacoef, mean_covmv, thetacoef,mean_covyv,interaction)
    gg00 <- gg(a0, a0, betacoef, mean_covmv, thetacoef,mean_covyv,interaction)
    gg11 <- gg(a1, a1, betacoef, mean_covmv, thetacoef,mean_covyv,interaction)

    # Natural effects computation

    P10 <- gg10[[1]]
    P00 <- gg00[[1]]
    P11 <- gg11[[1]]

    ORd <- (P10/(1-P10))/(P00/(1-P00))
    ORi <- (P11/(1-P11))/(P10/(1-P10))
    ORt <- ORd*ORi

    RRd <- P10/P00
    RRi <- P11/P10
    RRt <- RRd*RRi

    RDd <- P10 - P00
    RDi <- P11 - P10
    RDt <- RDd + RDi

    # Confidence intervals computation

    Sigmabeta <- vcov(Mreg)
    Sigmatheta <- vcov(Yreg)

    l1<- length(betacoef)
    l2 <- length(thetacoef)

    l <- l1 + l2

    Sigma <- matrix(0, nrow = l, ncol = l)

    Sigma[1:l1,1:l1] <- Sigmabeta
    Sigma[(l1+1):l, (l1+1):l] <- Sigmatheta

    confcoefint <-1-(1-confcoef)/2

    int0 <- exp(-qnorm(confcoefint))
    int1 <-exp(qnorm(confcoefint))

    gradlnORd <- gg10[[2]]/(gg10[[1]]*(1-gg10[[1]])) -  gg00[[2]]/(gg00[[1]]*(1-gg00[[1]]))
    gradlnORi <- gg11[[2]]/(gg11[[1]]*(1-gg11[[1]])) -  gg10[[2]]/(gg10[[1]]*(1-gg10[[1]]))
    gradlnORt <- gradlnORd + gradlnORi

    selnORd <- sqrt(as.numeric(gradlnORd%*%Sigma%*%gradlnORd))
    selnORi <- sqrt(as.numeric(gradlnORi%*%Sigma%*%gradlnORi))
    selnORt <- sqrt(as.numeric(gradlnORt%*%Sigma%*%gradlnORt))

    CI_ORd <- ORd*c(int0, int1)^selnORd
    CI_ORi <- ORi*c(int0, int1)^selnORi
    CI_ORt <- ORt*c(int0, int1)^selnORt


    gradlnRRd <- gg10[[2]]/gg10[[1]] -  gg00[[2]]/gg00[[1]]
    gradlnRRi <- gg11[[2]]/gg11[[1]] -  gg10[[2]]/gg10[[1]]
    gradlnRRt <- gradlnRRd + gradlnRRi

    selnRRd <- sqrt(as.numeric(gradlnRRd%*%Sigma%*%gradlnRRd))
    selnRRi <- sqrt(as.numeric(gradlnRRi%*%Sigma%*%gradlnRRi))
    selnRRt <- sqrt(as.numeric(gradlnRRt%*%Sigma%*%gradlnRRt))

    CI_RRd <- RRd*c(int0, int1)^selnRRd
    CI_RRi <- RRi*c(int0, int1)^selnRRi
    CI_RRt <- RRt*c(int0, int1)^selnRRt


    gradRDd <- gg10[[2]] -  gg00[[2]]
    gradRDi <- gg11[[2]] -  gg10[[2]]
    gradRDt <- gradRDd + gradRDi

    seRDd <- sqrt(as.numeric(gradRDd%*%Sigma%*%gradRDd))
    seRDi <- sqrt(as.numeric(gradRDi%*%Sigma%*%gradRDi))
    seRDt <- sqrt(as.numeric(gradRDt%*%Sigma%*%gradRDt))

    CI_RDd <- RDd + seRDd*log(c(int0, int1))
    CI_RDi <- RDi + seRDi*log(c(int0, int1))
    CI_RDt <- RDt + seRDt*log(c(int0, int1))


    # Probabilities P(Y(a,m) =1|C=c) and gradient computation (m=0)

    gg_cde1m0 <- gg_cde(a1, m=0, thetacoef, mean_covyv, interaction)
    gg_cde0m0 <- gg_cde(a0, m=0, thetacoef, mean_covyv, interaction)

    #Controled effects computation (m=0)

    P1m0 <- gg_cde1m0[[1]]
    P0m0 <- gg_cde0m0[[1]]

    ORm0 <- (P1m0/(1-P1m0))/(P0m0/(1-P0m0))
    RRm0 <- P1m0/P0m0
    RDm0 <- P1m0 - P0m0

    # Confidence intervals computation (m=0)

    gradlnORm0 <- gg_cde1m0[[2]]/(P1m0*(1-P1m0)) -
      gg_cde0m0[[2]]/(P0m0*(1-P0m0))

    gradlnRRm0 <- gg_cde1m0[[2]]/P1m0 -  gg_cde0m0[[2]]/P0m0
    gradRDm0 <- gg_cde1m0[[2]] -  gg_cde0m0[[2]]

    selnORm0 <- sqrt(as.numeric(gradlnORm0%*%Sigmatheta%*%gradlnORm0))
    selnRRm0 <- sqrt(as.numeric(gradlnRRm0%*%Sigmatheta%*%gradlnRRm0))
    seRDm0 <- sqrt(as.numeric(gradRDm0%*%Sigmatheta%*%gradRDm0))


    CI_ORm0 <- ORm0*c(int0, int1)^selnORm0
    CI_RRm0 <- RRm0*c(int0, int1)^selnRRm0
    CI_RDm0 <- RDm0 + seRDm0*log(c(int0, int1))


    # Probabilities P(Y(a,m) =1|C=c) and gradient computation (m=1)

    gg_cde1m1 <- gg_cde(a1, m=1, thetacoef, mean_covyv, interaction)
    gg_cde0m1 <- gg_cde(a0, m=1, thetacoef, mean_covyv, interaction)

    #Controled effects computation (m=1)

    P1m1 <- gg_cde1m1[[1]]
    P0m1 <- gg_cde0m1[[1]]

    ORm1 <- (P1m1/(1-P1m1))/(P0m1/(1-P0m1))
    RRm1 <- P1m1/P0m1
    RDm1 <- P1m1 - P0m1

    # Confidence intervals computation (m=1)

    gradlnORm1 <- gg_cde1m1[[2]]/(P1m1*(1-P1m1)) -
      gg_cde0m1[[2]]/(P0m1*(1-P0m1))

    gradlnRRm1 <- gg_cde1m1[[2]]/P1m1 -  gg_cde0m1[[2]]/P0m1
    gradRDm1 <- gg_cde1m1[[2]] -  gg_cde0m1[[2]]

    selnORm1 <- sqrt(as.numeric(gradlnORm1%*%Sigmatheta%*%gradlnORm1))
    selnRRm1 <- sqrt(as.numeric(gradlnRRm1%*%Sigmatheta%*%gradlnRRm1))
    seRDm1 <- sqrt(as.numeric(gradRDm1%*%Sigmatheta%*%gradRDm1))


    CI_ORm1 <- ORm1*c(int0, int1)^selnORm1
    CI_RRm1 <- RRm1*c(int0, int1)^selnRRm1
    CI_RDm1 <- RDm1 + seRDm1*log(c(int0, int1))



  } else{

    gg <- function(a,b, betav,covmv,thetav,covyv, interaction){

      if(interaction ==TRUE){

        probY1M1 <- expit(thetav[1] + thetav[3] +
                            (thetav[2] + thetav[4])*a + as.numeric(thetav[-(1:4)]%*%covyv))

        probY1M0 <- expit(thetav[1] + thetav[2]*a + as.numeric(thetav[-(1:4)]%*%covyv))

      } else{

        probY1M1 <- expit(thetav[1] + thetav[3] +
                            thetav[2]*a + as.numeric(thetav[-(1:3)]%*%covyv))

        probY1M0 <- expit(thetav[1] + thetav[2]*a + as.numeric(thetav[-(1:3)]%*%covyv))

      }


      probM1 <- expit(betav[1] + betav[2]*b + as.numeric(betav[-(1:2)]%*%covmv))
      probM0 <- 1- probM1


      return(probY1M1*probM1 + probY1M0*probM0)

    }


    gg_cde <- function(a,m,thetav,covyv, interaction){

      if(interaction == TRUE){

        P <- expit(thetav[1] + thetav[2]*a + thetav[3]*m +
                     thetav[4]*a*m + as.numeric(thetav[-(1:4)]%*%covyv))

      }else{

        P <- expit(thetav[1] + thetav[2]*a + thetav[3]*m +
                     as.numeric(thetav[-(1:3)]%*%covyv))

      }

      return(P)

    }

    P10 <- gg(a1, a0, betacoef, mean_covmv, thetacoef,mean_covyv,interaction)
    P00 <- gg(a0, a0, betacoef, mean_covmv, thetacoef,mean_covyv,interaction)
    P11 <- gg(a1, a1, betacoef, mean_covmv, thetacoef,mean_covyv,interaction)

    ORd <- (P10/(1-P10))/(P00/(1-P00))
    ORi <- (P11/(1-P11))/(P10/(1-P10))
    ORt <- ORd*ORi

    RRd <- P10/P00
    RRi <- P11/P10
    RRt <- RRd*RRi

    RDd <- P10 - P00
    RDi <- P11 - P10
    RDt <- RDd + RDi

    P1m0 <- gg_cde(a1, m=0, thetacoef, mean_covyv, interaction)
    P0m0 <- gg_cde(a0, m=0, thetacoef, mean_covyv, interaction)

    ORm0 <- (P1m0/(1-P1m0))/(P0m0/(1-P0m0))
    RRm0 <- P1m0/P0m0
    RDm0 <- P1m0 - P0m0


    P1m1 <- gg_cde(a1, m=1, thetacoef, mean_covyv, interaction)
    P0m1 <- gg_cde(a0, m=1, thetacoef, mean_covyv, interaction)

    ORm1 <- (P1m1/(1-P1m1))/(P0m1/(1-P0m1))
    RRm1 <- P1m1/P0m1
    RDm1 <- P1m1 - P0m1


    set.seed(bootseed)

    n <- nrow(DATA)

    ORboot <- matrix(0,nboot,3)
    RRboot <- matrix(0,nboot,3)
    RDboot <- matrix(0,nboot,3)

    ORm0boot <- vector("numeric",length=nboot)
    RRm0boot <- vector("numeric",length=nboot)
    RDm0boot <- vector("numeric",length=nboot)

    ORm1boot <- vector("numeric",length=nboot)
    RRm1boot <- vector("numeric",length=nboot)
    RDm1boot <- vector("numeric",length=nboot)

    progress_bar <- txtProgressBar(min=0, max=nboot, style = 3, char="=")

    for(i in 1:nboot){


      vboot<- sample(1:n, n, replace=TRUE)

      DATAboot <- DATA[vboot,]

      if(Firth == TRUE){

        Mreg <- logistf(Mform, data= DATAboot)
        Yreg <- logistf(Yform, data= DATAboot)

      } else{

        Mreg <- glm(Mform, data= DATAboot, family =binomial(link = "logit"))
        Yreg <- glm(Yform, data= DATAboot, family =binomial(link = "logit"))

      }

      betacoef <- Mreg$coefficients
      thetacoef <- Yreg$coefficients

      names(betacoef) <- NULL
      names(thetacoef) <- NULL

      P10 <- gg(a1, a0, betacoef, mean_covmv, thetacoef,mean_covyv,interaction)
      P00 <- gg(a0, a0, betacoef, mean_covmv, thetacoef,mean_covyv,interaction)
      P11 <- gg(a1, a1, betacoef, mean_covmv, thetacoef,mean_covyv,interaction)

      ORboot[i,1] <- (P10/(1-P10))/(P00/(1-P00))
      ORboot[i,2] <- (P11/(1-P11))/(P10/(1-P10))
      ORboot[i,3] <- ORboot[i,1]*ORboot[i,2]

      RRboot[i,1] <- P10/P00
      RRboot[i,2] <- P11/P10
      RRboot[i,3] <- RRboot[i,1]*RRboot[i,2]

      RDboot[i,1] <- P10 - P00
      RDboot[i,2] <- P11 - P10
      RDboot[i,3] <- RDboot[i,1] + RDboot[i,2]


      P1m0 <- gg_cde(a1, m=0, thetacoef, mean_covyv, interaction)
      P0m0 <- gg_cde(a0, m=0, thetacoef, mean_covyv, interaction)

      ORm0boot[i] <- (P1m0/(1-P1m0))/(P0m0/(1-P0m0))
      RRm0boot[i] <- P1m0/P0m0
      RDm0boot[i] <- P1m0 - P0m0


      P1m1 <- gg_cde(a1, m=1, thetacoef, mean_covyv, interaction)
      P0m1 <- gg_cde(a0, m=1, thetacoef, mean_covyv, interaction)

      ORm1boot[i] <- (P1m1/(1-P1m1))/(P0m1/(1-P0m1))
      RRm1boot[i] <- P1m1/P0m1
      RDm1boot[i] <- P1m1 - P0m1

      setTxtProgressBar(progress_bar, value = i)

    }

    confcoefint <-1-(1-confcoef)/2
    cisup <- confcoefint
    ciinf <- 1-confcoefint

    CI_ORd <- quantile(ORboot[,1],c(ciinf,cisup))
    CI_ORi <- quantile(ORboot[,2],c(ciinf,cisup))
    CI_ORt <- quantile(ORboot[,3],c(ciinf,cisup))

    CI_RRd <- quantile(RRboot[,1],c(ciinf,cisup))
    CI_RRi <- quantile(RRboot[,2],c(ciinf,cisup))
    CI_RRt <- quantile(RRboot[,3],c(ciinf,cisup))

    CI_RDd <- quantile(RDboot[,1],c(ciinf,cisup))
    CI_RDi <- quantile(RDboot[,2],c(ciinf,cisup))
    CI_RDt <- quantile(RDboot[,3],c(ciinf,cisup))


    CI_ORm0 <- quantile(ORm0boot,c(ciinf,cisup))
    CI_RRm0 <- quantile(RRm0boot,c(ciinf,cisup))
    CI_RDm0 <- quantile(RDm0boot,c(ciinf,cisup))

    CI_ORm1 <- quantile(ORm1boot,c(ciinf,cisup))
    CI_RRm1 <- quantile(RRm1boot,c(ciinf,cisup))
    CI_RDm1 <- quantile(RDm1boot,c(ciinf,cisup))

    close(progress_bar)

  }


  #  Results

  CIsup <- paste(confcoefint*100, '%')
  CIinf <- paste((1-confcoefint)*100, '%')

  OR  <- matrix(0, nrow =3, ncol=3)
  RR  <- matrix(0, nrow =3, ncol=3)
  RD  <- matrix(0, nrow =3, ncol=3)

  rownames(OR)  <- c("Direct effect","Indirect effect", "Total effect")
  colnames(OR) <- c("Estimate",CIinf,CIsup)

  OR[1,] <- c(ORd, CI_ORd)
  OR[2,] <- c(ORi, CI_ORi)
  OR[3,] <- c(ORt, CI_ORt)

  rownames(RR)  <- c("Direct effect","Indirect effect", "Total effect")
  colnames(RR) <- c("Estimate",CIinf,CIsup)

  RR[1,] <- c(RRd, CI_RRd)
  RR[2,] <- c(RRi, CI_RRi)
  RR[3,] <- c(RRt, CI_RRt)


  rownames(RD)  <- c("Direct effect","Indirect effect", "Total effect")
  colnames(RD) <- c("Estimate",CIinf,CIsup)

  RD[1,] <- c(RDd, CI_RDd)
  RD[2,] <- c(RDi, CI_RDi)
  RD[3,] <- c(RDt, CI_RDt)

  ContEffm0 <- matrix(0, nrow =3, ncol=3)
  ContEffm1 <- matrix(0, nrow =3, ncol=3)

  rownames(ContEffm0) <- c("OR scale","RR scale","RD scale")

  colnames(ContEffm0) <- c("Estimate",CIinf,CIsup)

  ContEffm0[1,] <- c(ORm0, CI_ORm0)
  ContEffm0[2,] <- c(RRm0, CI_RRm0)
  ContEffm0[3,] <- c(RDm0, CI_RDm0)

  rownames(ContEffm1) <- c("OR scale","RR scale","RD scale")

  colnames(ContEffm1) <- c("Estimate",CIinf,CIsup)

  ContEffm1[1,] <- c(ORm1, CI_ORm1)
  ContEffm1[2,] <- c(RRm1, CI_RRm1)
  ContEffm1[3,] <- c(RDm1, CI_RDm1)


  results <- vector("list",5)

  names(results) <- c("Natural effects in OR scale",
                      "Natural effects in RR scale",
                      "Natural effects in RD scale",
                      "Controlled direct effects (m=0)",
                      "Controlled direct effects (m=1)")

  results[[1]] <- round(OR,digits =5)
  results[[2]] <- round(RR, digits =5)
  results[[3]] <- round(RD, digits = 5)
  results[[4]] <- round(ContEffm0, digits =5)
  results[[5]] <- round(ContEffm1, digits = 5)



  return(results)


}


