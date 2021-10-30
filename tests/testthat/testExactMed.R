library(testthat)
library(ExactMed)



test_that("EXACTMED() with 'datamed' returns a list",{

  result1 <- EXACTMED(DATA=datamed, A='X', M='M', Y='Y', a1=1, a0=0, M_COV=c('C1', 'C2'),
                      Y_COV=c('C1', 'C2'))

  result2 <- EXACTMED(DATA=datamed, A='X', M='M', Y='Y', a1=1, a0=0, M_COV=c('C1', 'C2'),
                      Y_COV=c('C1', 'C2'), Firth=TRUE)

  result3 <- EXACTMED(DATA=datamed, A='X', M='M', Y='Y', a1=1, a0=0, M_COV=c('C1', 'C2'),
                      Y_COV=c('C1', 'C2'), boot =TRUE, nboot=100)

  result4 <- EXACTMED(DATA=datamed, A='X', M='M', Y='Y', a1=1, a0=0, M_COV=c('C1', 'C2'),
                      Y_COV=c('C1', 'C2'), Firth=TRUE, boot=TRUE, nboot=60)

  result5 <- EXACTMED(DATA=datamed, A='X', M='M', Y='Y', a1=1, a0=0, M_COV=c('C1', 'C2'),
                      Y_COV=c('C1', 'C2'), interaction=FALSE)

  result6 <- EXACTMED(DATA=datamed, A='X', M='M', Y='Y', a1=1, a0=0, M_COV=c('C1', 'C2'),
                      Y_COV=c('C1', 'C2'), interaction=FALSE, Firth=TRUE)

  result7 <- EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                      Y_COV=c('C1', 'C2'), interaction=FALSE, boot=TRUE, nboot=100)

  result8 <- EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                      Y_COV=c('C1', 'C2'), interaction=FALSE, Firth=TRUE, boot=TRUE, nboot=60)

  result9 <- EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                      Y_COV=c('C1', 'C2'), adjusted = FALSE)

  result10 <- EXACTMED(DATA=datamed, A='X', M='M', Y='Y', a1=1, a0=0)

  result11 <- EXACTMED(DATA=datamed, A='X', M='M', Y='Y', a1=1, a0=0, Firth=TRUE)

  result12 <- EXACTMED(DATA=datamed, A='X', M='M', Y='Y', a1=1, a0=0, boot=TRUE, nboot=100)

  result13 <- EXACTMED(DATA=datamed, A='X', M='M', Y='Y', a1=1, a0=0, Firth=TRUE, boot=TRUE, nboot=60)

  result14 <- EXACTMED(DATA=datamed, A='X', M='M', Y='Y', a1=1, a0=0, interaction=FALSE)

  result15 <- EXACTMED(DATA=datamed, A='X', M='M', Y='Y', a1=1, a0=0, interaction=FALSE, Firth=TRUE)

  result16 <- EXACTMED(DATA=datamed, A='X', M='M', Y='Y', a1=1, a0=0, interaction=FALSE, boot=TRUE, nboot=100)

  result17 <- EXACTMED(DATA=datamed, A='X', M='M', Y='Y', a1=1, a0=0, interaction=FALSE, Firth=TRUE, boot=TRUE, nboot=60)


  M_COV_cond <- c(0.3,0.2)

  names(M_COV_cond) <- c('C1','C2')


  Y_COV_cond <- c(0.3,0.2)

  names(Y_COV_cond) <- c('C1', 'C2')

  result18 <- EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                       Y_COV=c('C1', 'C2'), M_COV_cond =M_COV_cond, Y_COV_cond=Y_COV_cond)



  datamed4 <- datamed
  n <- nrow(datamed4)
  datamed4$M <- as.factor(datamed4$M)
  levels(datamed4$M) <- c(1,2)

  result19 <- EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0, hvalueM="2")

  result20 <- EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0, hvalueM=2)

  datamed4 <- datamed
  lv <- vector("integer", length=n)
  lv[which(datamed4$M==0)] <- 1
  lv[which(datamed4$M==1)] <- 2
  datamed4$M <- lv

  result21 <- EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0, hvalueM=2)


  datamed4 <- datamed
  n <- nrow(datamed4)
  datamed4$Y <- as.factor(datamed4$Y)
  levels(datamed4$Y) <- c(1,2)

  result22 <- EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0, hvalueY="2")

  result23 <- EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0, hvalueY=2)

  datamed4 <- datamed
  lv <- vector("integer", length=n)
  lv[which(datamed4$Y==0)] <- 1
  lv[which(datamed4$Y==1)] <- 2
  datamed4$Y <- lv

  result24 <- EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0, hvalueY=2)

  datamed4 <- datamed
  datamed4$C1 <- sample(c("a","b","c"), nrow(datamed4), replace =TRUE)

  result25 <- EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0, M_COV = c('C1', 'C2'), Y_COV = c('C1', 'C2'))

  M_COV_cond <- list('a',2)
  names(M_COV_cond) <- c('C1', 'C2')

  Y_COV_cond <- list('a',2)
  names(Y_COV_cond) <- c('C1', 'C2')

  result26 <- EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0,
                       M_COV = c('C1', 'C2'), Y_COV = c('C1', 'C2'),
                       M_COV_cond=M_COV_cond, Y_COV_cond=Y_COV_cond)


  M_COV_cond <- list('b',2)
  names(M_COV_cond) <- c('C1', 'C2')

  Y_COV_cond <- list('b',2)
  names(Y_COV_cond) <- c('C1', 'C2')

  result27 <- EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0,
                       M_COV = c('C1', 'C2'), Y_COV = c('C1', 'C2'),
                       M_COV_cond=M_COV_cond, Y_COV_cond=Y_COV_cond)



  expect_type(result1, "list")
  expect_type(result2, "list")
  expect_type(result3, "list")
  expect_type(result4, "list")
  expect_type(result5, "list")
  expect_type(result6, "list")
  expect_type(result7, "list")
  expect_type(result8, "list")
  expect_type(result9, "list")
  expect_type(result10, "list")
  expect_type(result11, "list")
  expect_type(result12, "list")
  expect_type(result13, "list")
  expect_type(result14, "list")
  expect_type(result15, "list")
  expect_type(result16, "list")
  expect_type(result17, "list")
  expect_type(result18, "list")
  expect_type(result19, "list")
  expect_type(result20, "list")
  expect_type(result21, "list")
  expect_type(result22, "list")
  expect_type(result23, "list")
  expect_type(result24, "list")
  expect_type(result25, "list")
  expect_type(result26, "list")
  expect_type(result27, "list")
})


test_that("EXACTMED() with 'datamed2' returns a list",{

  datamed2 <- datamed

  pY2 <- 0.1

  n <-nrow(datamed2)

  Y2 <- rbinom(n,1,pY2)

  datamed2$Y <- Y2

  result1 <- EXACTMED(DATA=datamed2, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                      Y_COV=c('C1', 'C2'))

  result2 <- EXACTMED(DATA=datamed2, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                      Y_COV=c('C1', 'C2'), boot =TRUE, nboot=100)

  result3 <- EXACTMED(DATA=datamed2, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                      Y_COV=c('C1', 'C2'), Firth=TRUE)

  result4 <- EXACTMED(DATA=datamed2, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                      Y_COV=c('C1', 'C2'), Firth=TRUE, boot=TRUE, nboot=60)

  result5 <- EXACTMED(DATA=datamed2, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                      Y_COV=c('C1', 'C2'), interaction=FALSE)

  result6 <- EXACTMED(DATA=datamed2, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                      Y_COV=c('C1', 'C2'), adjusted = FALSE)

  result7 <- EXACTMED(DATA=datamed2, A='X', M='M', Y='Y',  a1=1, a0=0)

  result8 <- EXACTMED(DATA=datamed2, A='X', M='M', Y='Y',  a1=1, a0=0, interaction =FALSE)

  result9 <- EXACTMED(DATA=datamed2, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                      Y_COV=c('C1', 'C2'), adjusted = FALSE, interaction =FALSE)

  M_COV_cond <- c(0.3,0.2)

  names(M_COV_cond) <- c('C1','C2')


  Y_COV_cond <- c(0.3,0.2)

  names(Y_COV_cond) <- c('C1', 'C2')

  result10 <- EXACTMED(DATA=datamed2, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                       Y_COV=c('C1', 'C2'),M_COV_cond =M_COV_cond,Y_COV_cond=Y_COV_cond )

  expect_type(result1, "list")
  expect_type(result2, "list")
  expect_type(result3, "list")
  expect_type(result4, "list")
  expect_type(result5, "list")
  expect_type(result6, "list")
  expect_type(result7, "list")
  expect_type(result8, "list")
  expect_type(result9, "list")
  expect_type(result10, "list")

})

test_that("Getting a message",{


  expect_message(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0))


  expect_message(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                 Y_COV=c('C1', 'C2'), adjusted = FALSE))



})


test_that("Getting an error due to incorrect type of 'DATA' parameter",{

  datamed3 <- as.matrix(datamed)

  expect_error(EXACTMED(DATA=datamed3, A='X', M='M', Y='Y',  a1=1, a0=0),
               "'DATA' must be a data frame with column names"
               )

})


test_that("Getting an error due to missing values in 'DATA'",{

  datamed4 <- datamed
  datamed4[1,2] <- NA

  expect_error(EXACTMED(DATA=datamed4, A='X', M='M', Y='Y',  a1=1, a0=0),
               "'DATA' contains missing values"
  )

})


test_that("Getting an error due to missing values in 'DATA'",{

  datamed4 <- datamed
  datamed4[1,2] <- NA

  expect_error(EXACTMED(DATA=datamed4, A='X', M='M', Y='Y',  a1=1, a0=0),
               "'DATA' contains missing values"
  )

})


test_that("Getting an error due to duplicated column names in 'DATA'",{

  datamed4 <- datamed
  colnames(datamed4)[5] <- 'C1'

  expect_error(EXACTMED(DATA=datamed4, A='X', M='M', Y='Y',  a1=1, a0=0),
               "'DATA' has duplicated column names"
  )

})

test_that("Getting an error due to an unnamed column in 'DATA'",{

  datamed4 <- datamed
  colnames(datamed4)[5] <- NA

  expect_error(EXACTMED(DATA=datamed4, A='X', M='M', Y='Y',  a1=1, a0=0),
               "'DATA' has some unnamed columns"
  )

})



test_that("Getting an error due to incorrect value for the 'A', 'M' or 'Y' parameter",{


  expect_error(EXACTMED(DATA=datamed, A='V', M='M', Y='Y',  a1=1, a0=0),
               "'A' has to be a column name of 'DATA'"
  )

  expect_error(EXACTMED(DATA=datamed, A='X', M=3, Y='Y',  a1=1, a0=0),
               "'M' has to be a column name of 'DATA'"
  )

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y= c(1,2),  a1=1, a0=0),
               "'Y' has to be a column name of 'DATA'"
  )



})




test_that("Getting an error due to an incorrect value related to the mediator, the exposure or the response",{


  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y', a1=1, a0=0, hvalueM=c(1,2)),
               "Invalid type or length for input parameter 'hvalueM'"
  )

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y', a1=1, a0=0, hvalueY=NA),
               "Invalid type or length for input parameter 'hvalueY'"
  )

  datamed4 <- datamed
  n <- nrow(datamed4)
  Xb <- factor(sample(c("a","b","c"),n, replace =TRUE))
  datamed4$X <- Xb

  expect_error(EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0),
               "Exposure must be numerical variable"
  )

  datamed4 <- datamed
  Mb <- sample(c(1, 2, 3),n, replace =TRUE)
  datamed4$M <- Mb

  expect_error(EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0),
               "Mediator takes more than two different values in 'DATA'"
  )

  datamed4 <- datamed
  Mb <- factor(sample(c(1, 2),n, replace =TRUE))
  datamed4$M <- Mb

  expect_error(EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0, hvalueM=NULL),
               "High level for the mediator must be specified"
  )

  expect_error(EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0, hvalueM="3"),
               "Invalid value for high level of mediator"
  )


  datamed4 <- datamed
  Mb <- sample(c(1, 2),n, replace =TRUE)
  datamed4$M <- Mb

  expect_error(EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0, hvalueM=NULL),
               "High level for the mediator must be specified"
  )

  expect_error(EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0, hvalueM=3),
               "Invalid value for high level of mediator"
  )


  datamed4 <- datamed
  Yb <- sample(c(1, 2, 3),n, replace =TRUE)
  datamed4$Y <- Yb

  expect_error(EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0),
               "Outcome takes more than two different values in 'DATA'"
  )


  datamed4 <- datamed
  Yb <- factor(sample(c(1, 2),n, replace =TRUE))
  datamed4$Y <- Yb

  expect_error(EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0, hvalueY=NULL),
               "High level for the outcome must be specified"
  )

  expect_error(EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0, hvalueY="3"),
               "Invalid value for high level of outcome"
  )


  datamed4 <- datamed
  Yb <- sample(c(1, 2),n, replace =TRUE)
  datamed4$Y <- Yb

  expect_error(EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0, hvalueY=NULL),
               "High level for the outcome must be specified"
  )

  expect_error(EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0, hvalueY=3),
               "Invalid value for high level of outcome"
  )


})



test_that("Getting an error due to incorrect value for a numeric parameter",{


  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=2+3i, a0=0),
               "'a1' has to be a real number"
  )

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0='hjk'),
               "'a0' has to be a real number"
  )


  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, confcoef=1.25),
               "'confcoef' has to be a valid real number"
  )


})

test_that("Getting an error due to incorrect value for a logical parameter",{


  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, interaction='k'),
               "'interaction' must specify a logical value"
  )

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, Firth = 4),
               "'Firth' must specify a logical value"
  )

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, adjusted = c(1,2)),
               "'adjusted' must specify a logical value"
  )

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, boot =NULL),
               "'boot' must specify a logical value"
  )



})


test_that("Getting an error due to incorrect value for the 'boot' or 'bootseed' parameter",{


  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, boot=TRUE, nboot=34.4),
               "'nboot' has to be an integer"
  )

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0,boot=TRUE, bootseed = NA),
               "'bootseed' has to be an integer"
  )



})



test_that("Getting an error due to incorrect value for the 'M_COV' or 'Y_COV' parameter",{


  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=3,
                             Y_COV=c('C1', 'C2')),

               "'M_COV' must be NULL or a vector of covariate names"

  )

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1',NA),
                        Y_COV=c('C1', 'C2')),

               "'M_COV' has NAs"

  )

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1','C1'),
                        Y_COV=c('C1', 'C2')),

               "'M_COV' has duplicated covariates names"

  )


  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2','C3'),
                        Y_COV=c('C1', 'C2')),

               "'M_COV' can only contain names of covariates included in the data frame"


  )


  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                        Y_COV=3),


               "'Y_COV' must be NULL or a vector of covariate names"

  )

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                        Y_COV=c('C1',NA)),

               "'Y_COV' has NAs"

  )

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                        Y_COV=c('C1','C1')),

               "'Y_COV' has duplicated covariates names"

  )

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                        Y_COV=c('C1', 'C2','C3')),

               "'Y_COV' can only contain names of covariates included in the data frame"
  )



})




test_that("Getting an error due to incorrect value for the 'M_COV_cond' or 'Y_COV_cond' parameter",{

  Y_COV_cond <- c(0.5,0.2)
  names(Y_COV_cond) <- c('C1', 'C2')

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                        Y_COV=c('C1', 'C2'),M_COV_cond =datamed, Y_COV_cond = Y_COV_cond),

               "'M_COV_cond' must be NULL or a vector"

  )


  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                        Y_COV=c('C1', 'C2'),M_COV_cond =c(0.5,0.2), Y_COV_cond = Y_COV_cond),

               "'M_COV_cond' must be a named vector"


  )

  M_COV_cond <- c(0.5,0.2)
  names(M_COV_cond) <- c('C1')

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                        Y_COV=c('C1', 'C2'),M_COV_cond =M_COV_cond, Y_COV_cond = Y_COV_cond),

               "'M_COV_cond' has missing names"


  )


  names(M_COV_cond) <- c('C1', 'C1')

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                        Y_COV=c('C1', 'C2'),M_COV_cond =M_COV_cond, Y_COV_cond = Y_COV_cond),

               "'M_COV_cond' has duplicated names"


  )


  names(M_COV_cond) <- c('C1', 'C3')

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                        Y_COV=c('C1', 'C2'),M_COV_cond =M_COV_cond, Y_COV_cond = Y_COV_cond),

               "The names of the elements of 'M_COV_cond' must be in 'M_COV'"


  )

  M_COV_cond <- list(0.5,list(0.2, 0.5))
  names(M_COV_cond) <- c('C1', 'C2')

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                        Y_COV=c('C1', 'C2'),M_COV_cond =M_COV_cond, Y_COV_cond = Y_COV_cond),

               "'M_COV_cond' has a invalid value in the "


  )



  rm(Y_COV_cond)
  M_COV_cond <- c(0.5,0.2)
  names(M_COV_cond) <- c('C1', 'C2')

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                        Y_COV=c('C1', 'C2'),M_COV_cond =M_COV_cond, Y_COV_cond = datamed),

               "'Y_COV_cond' must be NULL or a vector"

  )

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                        Y_COV=c('C1', 'C2'),M_COV_cond =M_COV_cond , Y_COV_cond = c(0.5,0.2)),

               "'Y_COV_cond' must be a named vector"


  )


  Y_COV_cond <- c(0.5,0.2)
  names(Y_COV_cond) <- c('C1')

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                        Y_COV=c('C1', 'C2'),M_COV_cond =M_COV_cond, Y_COV_cond = Y_COV_cond),

               "'Y_COV_cond' has missing names"


  )


  names(Y_COV_cond) <- c('C1', 'C1')

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                        Y_COV=c('C1', 'C2'),M_COV_cond =M_COV_cond, Y_COV_cond = Y_COV_cond),

               "'Y_COV_cond' has duplicated names"


  )


  names(Y_COV_cond) <- c('C1', 'C3')

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                        Y_COV=c('C1', 'C2'),M_COV_cond =M_COV_cond, Y_COV_cond = Y_COV_cond),

               "The names of the elements of 'Y_COV_cond' must be in 'Y_COV'"


  )

  Y_COV_cond <- list(0.5,list(0.2, 0.5))
  names(Y_COV_cond) <- c('C1', 'C2')

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                        Y_COV=c('C1', 'C2'),M_COV_cond =M_COV_cond, Y_COV_cond = Y_COV_cond),

               "'Y_COV_cond' has a invalid value in the "


  )




  Y_COV_cond <- c(0.4,0.2)
  names(Y_COV_cond) <- c('C1', 'C2')


  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                        Y_COV=c('C1', 'C2'),M_COV_cond =M_COV_cond, Y_COV_cond = Y_COV_cond),

               " has two different values specified"


  )

  Y_COV_cond <- 0.2
  names(Y_COV_cond) <- 'C2'

  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                        Y_COV=c('C1', 'C2'),M_COV_cond =M_COV_cond, Y_COV_cond = Y_COV_cond),

               " has two different values specified "


  )

  M_COV_cond <- 0.2
  names(M_COV_cond) <- 'C2'

  Y_COV_cond <- 0.3
  names(Y_COV_cond) <- 'C1'


  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C1', 'C2'),
                        Y_COV=c('C1'),M_COV_cond =M_COV_cond, Y_COV_cond = Y_COV_cond),

               " has two different values specified "


  )


  M_COV_cond <- 'a'
  names(M_COV_cond) <- 'C2'

  Y_COV_cond <- 0.3
  names(Y_COV_cond) <- 'C1'


  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C2'),
                        Y_COV=c('C1'),M_COV_cond =M_COV_cond, Y_COV_cond = Y_COV_cond),

               "Invalid value for "


  )

  M_COV_cond <- 0.3
  names(M_COV_cond) <- 'C2'

  Y_COV_cond <- 'a'
  names(Y_COV_cond) <- 'C1'


  expect_error(EXACTMED(DATA=datamed, A='X', M='M', Y='Y',  a1=1, a0=0, M_COV=c('C2'),
                        Y_COV=c('C1'),M_COV_cond =M_COV_cond, Y_COV_cond = Y_COV_cond),

               "Invalid value for "


  )



  datamed4 <- datamed
  datamed4$C1 <- sample(c("a","b","c"), nrow(datamed4), replace =TRUE)


  M_COV_cond <- 'd'
  names(M_COV_cond) <- 'C1'

  Y_COV_cond <- 2
  names(Y_COV_cond) <- 'C2'

  expect_error(EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0,
                       M_COV = c('C1'), Y_COV = c('C2'),
                       M_COV_cond=M_COV_cond, Y_COV_cond=Y_COV_cond),

               "Invalid value for "

  )


  M_COV_cond <- 2
  names(M_COV_cond) <- 'C2'

  Y_COV_cond <- 'd'
  names(Y_COV_cond) <- 'C1'

  expect_error(EXACTMED(DATA=datamed4, A='X', M='M', Y='Y', a1=1, a0=0,
                        M_COV = c('C2'), Y_COV = c('C1'),
                        M_COV_cond=M_COV_cond, Y_COV_cond=Y_COV_cond),

               "Invalid value for "

  )



})






