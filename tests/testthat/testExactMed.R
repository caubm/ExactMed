library(testthat)
library(ExactMed)

test_that("exactmed() with 'datamed' returns a list", {
  result1 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2")
  )

  result2 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    Firth = TRUE
  )

  result3 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    boot = TRUE, nboot = 100
  )

  result4 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    Firth = TRUE, boot = TRUE, nboot = 60
  )

  result5 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    interaction = FALSE
  )

  result6 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    interaction = FALSE, Firth = TRUE
  )

  result7 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    interaction = FALSE, boot = TRUE, nboot = 100
  )

  result8 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    interaction = FALSE, Firth = TRUE, boot = TRUE, nboot = 60
  )

  result9 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    adjusted = FALSE
  )

  result10 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0
  )

  result11 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    Firth = TRUE
  )

  result12 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    boot = TRUE, nboot = 100
  )

  result13 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    Firth = TRUE, boot = TRUE, nboot = 60
  )

  result14 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    interaction = FALSE
  )

  result15 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    interaction = FALSE, Firth = TRUE
  )

  result16 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    interaction = FALSE, boot = TRUE, nboot = 100
  )

  result17 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    interaction = FALSE, Firth = TRUE, boot = TRUE, nboot = 60
  )

  m_cov_cond <- c(0.3, 0.2)
  names(m_cov_cond) <- c("C1", "C2")
  y_cov_cond <- c(0.3, 0.2)
  names(y_cov_cond) <- c("C1", "C2")

  result18 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
    m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
  )

  datamed4 <- datamed
  n <- nrow(datamed4)
  datamed4$M <- as.factor(datamed4$M)
  levels(datamed4$M) <- c(1, 2)

  result19 <- exactmed(
    data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    hvalue_m = "2"
  )

  result20 <- exactmed(
    data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    hvalue_m = 2
  )

  datamed4 <- datamed
  lv <- vector("integer", length = n)
  lv[which(datamed4$M == 0)] <- 1
  lv[which(datamed4$M == 1)] <- 2
  datamed4$M <- lv

  result21 <- exactmed(
    data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    hvalue_m = 2
  )


  datamed4 <- datamed
  n <- nrow(datamed4)
  datamed4$Y <- as.factor(datamed4$Y)
  levels(datamed4$Y) <- c(1, 2)

  result22 <- exactmed(
    data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    hvalue_y = "2"
  )

  result23 <- exactmed(
    data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    hvalue_y = 2
  )

  datamed4 <- datamed
  lv <- vector("integer", length = n)
  lv[which(datamed4$Y == 0)] <- 1
  lv[which(datamed4$Y == 1)] <- 2
  datamed4$Y <- lv

  result24 <- exactmed(
    data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    hvalue_y = 2
  )

  datamed4 <- datamed
  datamed4$C1 <- sample(c("a", "b", "c"), nrow(datamed4), replace = TRUE)

  result25 <- exactmed(
    data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2")
  )

  m_cov_cond <- list("a", 2)
  names(m_cov_cond) <- c("C1", "C2")
  y_cov_cond <- list("a", 2)
  names(y_cov_cond) <- c("C1", "C2")

  result26 <- exactmed(
    data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
    m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
  )


  m_cov_cond <- list("b", 2)
  names(m_cov_cond) <- c("C1", "C2")
  y_cov_cond <- list("b", 2)
  names(y_cov_cond) <- c("C1", "C2")

  result27 <- exactmed(
    data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
    m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
  )

  result28 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"), yprevalence = 0.1
  )

  result29 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"), Firth = TRUE,
    yprevalence = 0.1
  )

  result30 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"), boot = TRUE, nboot = 100,
    yprevalence = 0.1
  )

  result31 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"), boot = TRUE, nboot = 100,
    Firth = TRUE, yprevalence = 0.1
  )

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
  expect_type(result28, "list")
  expect_type(result29, "list")
  expect_type(result30, "list")
  expect_type(result31, "list")
})

test_that("exactmed() with 'datamed2' returns a list", {
  datamed2 <- datamed
  pY2 <- 0.1
  n <- nrow(datamed2)
  Y2 <- rbinom(n, 1, pY2)
  datamed2$Y <- Y2

  result1 <- exactmed(
    data = datamed2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2")
  )

  result2 <- exactmed(
    data = datamed2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    boot = TRUE, nboot = 100
  )

  result3 <- exactmed(
    data = datamed2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
    Firth = TRUE
  )

  result4 <- exactmed(
    data = datamed2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
    Firth = TRUE, boot = TRUE, nboot = 60
  )

  result5 <- exactmed(
    data = datamed2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
    interaction = FALSE
  )

  result6 <- exactmed(
    data = datamed2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
    adjusted = FALSE
  )

  result7 <- exactmed(
    data = datamed2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0
  )

  result8 <- exactmed(
    data = datamed2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    interaction = FALSE
  )

  result9 <- exactmed(
    data = datamed2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
    adjusted = FALSE, interaction = FALSE
  )

  m_cov_cond <- c(0.3, 0.2)
  names(m_cov_cond) <- c("C1", "C2")
  y_cov_cond <- c(0.3, 0.2)
  names(y_cov_cond) <- c("C1", "C2")

  result10 <- exactmed(
    data = datamed2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
  )

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

test_that("exactmed() prints output to the console", {

  result1 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0
  )

  result2 <-exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    yprevalence = 0.1
  )

  result3 <-exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    boot = TRUE, nboot = 100
  )

  result4 <- exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    boot = TRUE, nboot = 100, yprevalence = 0.1
  )

  expect_output(print(result1))
  expect_output(print(result2))
  expect_output(print(result3))
  expect_output(print(result4))

})

test_that("Getting a message", {
  expect_message(exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0
    ))


  expect_message(exactmed(
    data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"), adjusted = FALSE
  ))
})

test_that("Getting an error due to incorrect type of 'data' parameter", {
  datamed3 <- as.matrix(datamed)

  expect_error(
    exactmed(data = datamed3, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0),
    "'data' must be a data frame with column names"
  )
})

test_that("Getting an error due to missing values in 'data'", {
  datamed4 <- datamed
  datamed4[1, 2] <- NA

  expect_error(
    exactmed(data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0),
    "'data' contains missing values"
  )
})

test_that("Getting an error due to duplicated column names in 'data'", {
  datamed4 <- datamed
  colnames(datamed4)[5] <- "C1"

  expect_error(
    exactmed(data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0),
    "'data' has duplicated column names"
  )
})

test_that("Getting an error due to an unnamed column in 'data'", {
  datamed4 <- datamed
  colnames(datamed4)[5] <- NA

  expect_error(
    exactmed(data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0),
    "'data' has some unnamed columns"
  )
})

test_that("Error messages: 'a', 'm' or 'y' parameter", {
  expect_error(
    exactmed(data = datamed, a = "V", m = "M", y = "Y", a1 = 1, a0 = 0),
    "'a' has to be a column name of 'data'"
  )

  expect_error(
    exactmed(data = datamed, a = "X", m = 3, y = "Y", a1 = 1, a0 = 0),
    "'m' has to be a column name of 'data'"
  )

  expect_error(
    exactmed(data = datamed, a = "X", m = "M", y = c(1, 2), a1 = 1, a0 = 0),
    "'y' has to be a column name of 'data'"
  )
})

test_that("Error messages: mediator, exposure or response", {
  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      hvalue_m = c(1, 2)
    ),
    "Invalid type or length for input parameter 'hvalue_m'"
  )

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      hvalue_y = NA
    ),
    "Invalid type or length for input parameter 'hvalue_y'"
  )

  datamed4 <- datamed
  n <- nrow(datamed4)
  Xb <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
  datamed4$X <- Xb

  expect_error(
    exactmed(data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0),
    "Exposure must be numerical variable"
  )

  datamed4 <- datamed
  Mb <- sample(c(1, 2, 3), n, replace = TRUE)
  datamed4$M <- Mb

  expect_error(
    exactmed(data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0),
    "Mediator takes more than two different values in 'data'"
  )

  datamed4 <- datamed
  Mb <- factor(sample(c(1, 2), n, replace = TRUE))
  datamed4$M <- Mb

  expect_error(
    exactmed(
      data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      hvalue_m = NULL
    ),
    "High level for the mediator must be specified"
  )

  expect_error(
    exactmed(
      data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      hvalue_m = "3"
    ),
    "Invalid value for high level of mediator"
  )

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      hvalue_m = "2"
    ),
    "Invalid value for high level of mediator"
  )

  datamed4 <- datamed
  Mb <- sample(c(1, 2), n, replace = TRUE)
  datamed4$M <- Mb

  expect_error(
    exactmed(
      data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      hvalue_m = NULL
    ),
    "High level for the mediator must be specified"
  )

  expect_error(
    exactmed(
      data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      hvalue_m = 3
    ),
    "Invalid value for high level of mediator"
  )

  datamed4 <- datamed
  Yb <- sample(c(1, 2, 3), n, replace = TRUE)
  datamed4$Y <- Yb

  expect_error(
    exactmed(data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0),
    "Outcome takes more than two different values in 'data'"
  )

  datamed4 <- datamed
  Yb <- factor(sample(c(1, 2), n, replace = TRUE))
  datamed4$Y <- Yb

  expect_error(
    exactmed(
      data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      hvalue_y = NULL
    ),
    "High level for the outcome must be specified"
  )

  expect_error(
    exactmed(
      data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      hvalue_y = "3"
    ),
    "Invalid value for high level of outcome"
  )

  datamed4 <- datamed
  Yb <- sample(c(1, 2), n, replace = TRUE)
  datamed4$Y <- Yb

  expect_error(
    exactmed(
      data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      hvalue_y = NULL
    ),
    "High level for the outcome must be specified"
  )

  expect_error(
    exactmed(
      data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      hvalue_y = 3
    ),
    "Invalid value for high level of outcome"
  )

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      hvalue_y = 3
    ),
    "Invalid value for high level of outcome"
  )
})

test_that("Error and warning messages: numeric parameters", {
  expect_error(
    exactmed(data = datamed, a = "X", m = "M", y = "Y", a1 = 2 + 3i, a0 = 0),
    "'a1' has to be a real number"
  )

  expect_error(
    exactmed(data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = "hjk"),
    "'a0' has to be a real number"
  )

  expect_warning(
    exactmed(data = datamed, a = "X", m = "M", y = "Y", a1 = 0, a0 = 1),
    "The value of the low level of exposure is not smaller than that of the high level"
  )

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      confcoef = 1.25
    ),
    "'confcoef' has to be a valid real number"
  )

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      yprevalence = -1.25
    ),
    "'yprevalence' must be NULL or a valid real number"
  )

})

test_that("Error messages: logical parameters", {
  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      interaction = "k"
    ),
    "'interaction' must specify a logical value"
  )

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      Firth = 4
    ),
    "'Firth' must specify a logical value"
  )

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      adjusted = c(1, 2)
    ),
    "'adjusted' must specify a logical value"
  )

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      boot = NULL
    ),
    "'boot' must specify a logical value"
  )
})

test_that("Error messages: 'boot' or 'bootseed' parameter", {
  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      boot = TRUE, nboot = 34.4
    ),
    "'nboot' has to be an integer"
  )

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      boot = TRUE, bootseed = NA
    ),
    "'bootseed' has to be an integer"
  )
})

test_that("Error messages: 'm_cov' or 'y_cov' parameter", {
  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = 3, y_cov = c("C1", "C2")
    ),
    "'m_cov' must be NULL or a vector of covariate names"
  )

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", NA), y_cov = c("C1", "C2")
    ),
    "'m_cov' has NAs"
  )

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C1"), y_cov = c("C1", "C2")
    ),
    "'m_cov' has duplicated covariates names"
  )


  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2", "C3"), y_cov = c("C1", "C2")
    ),
    "'m_cov' can only contain names of covariates included in the data frame"
  )


  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = 3
    ),
    "'y_cov' must be NULL or a vector of covariate names"
  )

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", NA)
    ),
    "'y_cov' has NAs"
  )

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C1")
    ),
    "'y_cov' has duplicated covariates names"
  )

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2", "C3")
    ),
    "'y_cov' can only contain names of covariates included in the data frame"
  )
})

test_that("Error messages: 'm_cov_cond' or 'y_cov_cond' parameter", {
  y_cov_cond <- c(0.5, 0.2)
  names(y_cov_cond) <- c("C1", "C2")

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = datamed, y_cov_cond = y_cov_cond
    ),
    "'m_cov_cond' must be NULL or a vector"
  )


  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = c(0.5, 0.2), y_cov_cond = y_cov_cond
    ),
    "'m_cov_cond' must be a named vector"
  )

  m_cov_cond <- c(0.5, 0.2)
  names(m_cov_cond) <- c("C1")

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "'m_cov_cond' has missing names"
  )


  names(m_cov_cond) <- c("C1", "C1")

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "'m_cov_cond' has duplicated names"
  )


  names(m_cov_cond) <- c("C1", "C3")

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "The names of the elements of 'm_cov_cond' must be in 'm_cov'"
  )

  m_cov_cond <- list(0.5, list(0.2, 0.5))
  names(m_cov_cond) <- c("C1", "C2")

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "'m_cov_cond' has a invalid value in the "
  )

  rm(y_cov_cond)
  m_cov_cond <- c(0.5, 0.2)
  names(m_cov_cond) <- c("C1", "C2")

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = datamed
    ),
    "'y_cov_cond' must be NULL or a vector"
  )

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = c(0.5, 0.2)
    ),
    "'y_cov_cond' must be a named vector"
  )

  y_cov_cond <- c(0.5, 0.2)
  names(y_cov_cond) <- c("C1")

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "'y_cov_cond' has missing names"
  )

  names(y_cov_cond) <- c("C1", "C1")

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "'y_cov_cond' has duplicated names"
  )

  names(y_cov_cond) <- c("C1", "C3")

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "The names of the elements of 'y_cov_cond' must be in 'y_cov'"
  )

  y_cov_cond <- list(0.5, list(0.2, 0.5))
  names(y_cov_cond) <- c("C1", "C2")

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "'y_cov_cond' has a invalid value in the "
  )

  y_cov_cond <- c(0.4, 0.2)
  names(y_cov_cond) <- c("C1", "C2")

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    " has two different values specified"
  )

  y_cov_cond <- 0.2
  names(y_cov_cond) <- "C2"

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    " has two different values specified "
  )

  m_cov_cond <- 0.2
  names(m_cov_cond) <- "C2"
  y_cov_cond <- 0.3
  names(y_cov_cond) <- "C1"

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    " has two different values specified "
  )

  m_cov_cond <- "a"
  names(m_cov_cond) <- "C2"
  y_cov_cond <- 0.3
  names(y_cov_cond) <- "C1"

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C2"), y_cov = c("C1"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "Invalid value for "
  )

  m_cov_cond <- 0.3
  names(m_cov_cond) <- "C2"
  y_cov_cond <- "a"
  names(y_cov_cond) <- "C1"

  expect_error(
    exactmed(
      data = datamed, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C2"), y_cov = c("C1"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "Invalid value for "
  )

  datamed4 <- datamed
  datamed4$C1 <- sample(c("a", "b", "c"), nrow(datamed4), replace = TRUE)

  m_cov_cond <- "d"
  names(m_cov_cond) <- "C1"
  y_cov_cond <- 2
  names(y_cov_cond) <- "C2"

  expect_error(
    exactmed(
      data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1"), y_cov = c("C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "Invalid value for "
  )

  m_cov_cond <- 2
  names(m_cov_cond) <- "C2"
  y_cov_cond <- "d"
  names(y_cov_cond) <- "C1"

  expect_error(
    exactmed(
      data = datamed4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C2"), y_cov = c("C1"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "Invalid value for "
  )
})



test_that("exactmed_c() with 'datamed_c' returns a list", {
  result1 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2")
  )

  result2 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    Firth = TRUE
  )

  result3 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    boot = TRUE, nboot = 100
  )

  result4 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    Firth = TRUE, boot = TRUE, nboot = 60
  )

  result5 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    interaction = FALSE
  )

  result6 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    interaction = FALSE, Firth = TRUE
  )

  result7 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    interaction = FALSE, boot = TRUE, nboot = 100
  )

  result8 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    interaction = FALSE, Firth = TRUE, boot = TRUE, nboot = 60
  )

  result9 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    adjusted = FALSE
  )

  result10 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0
  )

  result11 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    Firth = TRUE
  )

  result12 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    boot = TRUE, nboot = 100
  )

  result13 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    Firth = TRUE, boot = TRUE, nboot = 60
  )

  result14 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    interaction = FALSE
  )

  result15 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    interaction = FALSE, Firth = TRUE
  )

  result16 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    interaction = FALSE, boot = TRUE, nboot = 100
  )

  result17 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    interaction = FALSE, Firth = TRUE, boot = TRUE, nboot = 60
  )

  m_cov_cond <- c(0.3, 0.2)
  names(m_cov_cond) <- c("C1", "C2")
  y_cov_cond <- c(0.3, 0.2)
  names(y_cov_cond) <- c("C1", "C2")

  result18 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
    m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
  )

  datamed_c4 <- datamed_c
  n <- nrow(datamed_c4)
  datamed_c4$Y <- as.factor(datamed_c4$Y)
  levels(datamed_c4$Y) <- c(1, 2)

  result19 <- exactmed_c(
    data = datamed_c4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    hvalue_y = "2"
  )

  result20 <- exactmed_c(
    data = datamed_c4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    hvalue_y = 2
  )

  datamed_c4 <- datamed_c
  lv <- vector("integer", length = n)
  lv[which(datamed_c4$Y == 0)] <- 1
  lv[which(datamed_c4$Y == 1)] <- 2
  datamed_c4$Y <- lv

  result21 <- exactmed_c(
    data = datamed_c4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    hvalue_y = 2
  )

  datamed_c4 <- datamed_c
  datamed_c4$C1 <- sample(c("a", "b", "c"), nrow(datamed_c4), replace = TRUE)

  result22 <- exactmed_c(
    data = datamed_c4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2")
  )

  m_cov_cond <- list("a", 2)
  names(m_cov_cond) <- c("C1", "C2")
  y_cov_cond <- list("a", 2)
  names(y_cov_cond) <- c("C1", "C2")

  result23 <- exactmed_c(
    data = datamed_c4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
    m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
  )


  m_cov_cond <- list("b", 2)
  names(m_cov_cond) <- c("C1", "C2")
  y_cov_cond <- list("b", 2)
  names(y_cov_cond) <- c("C1", "C2")

  result24 <- exactmed_c(
    data = datamed_c4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
    m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
  )

  result25 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"), yprevalence = 0.1
  )

  result26 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"), Firth = TRUE,
    yprevalence = 0.1
  )

  result27 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"), boot = TRUE, nboot = 100,
    yprevalence = 0.1
  )

  result28 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"), boot = TRUE, nboot = 100,
    Firth = TRUE, yprevalence = 0.1
  )

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
  expect_type(result28, "list")
})

test_that("exactmed_c() with 'datamed_c2' returns a list", {
  datamed_c2 <- datamed_c
  pY2 <- 0.1
  n <- nrow(datamed_c2)
  Y2 <- rbinom(n, 1, pY2)
  datamed_c2$Y <- Y2

  result1 <- exactmed_c(
    data = datamed_c2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2")
  )

  result2 <- exactmed_c(
    data = datamed_c2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    boot = TRUE, nboot = 100
  )

  result3 <- exactmed_c(
    data = datamed_c2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
    Firth = TRUE
  )

  result4 <- exactmed_c(
    data = datamed_c2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
    Firth = TRUE, boot = TRUE, nboot = 60
  )

  result5 <- exactmed_c(
    data = datamed_c2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
    interaction = FALSE
  )

  result6 <- exactmed_c(
    data = datamed_c2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
    adjusted = FALSE
  )

  result7 <- exactmed_c(
    data = datamed_c2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0
  )

  result8 <- exactmed_c(
    data = datamed_c2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    interaction = FALSE
  )

  result9 <- exactmed_c(
    data = datamed_c2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
    adjusted = FALSE, interaction = FALSE
  )

  m_cov_cond <- c(0.3, 0.2)
  names(m_cov_cond) <- c("C1", "C2")
  y_cov_cond <- c(0.3, 0.2)
  names(y_cov_cond) <- c("C1", "C2")

  result10 <- exactmed_c(
    data = datamed_c2, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"),y_cov = c("C1", "C2"),
    m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
  )

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

test_that("exactmed_c() prints output to the console", {

  result1 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0
  )

  result2 <-exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    yprevalence = 0.1
  )

  result3 <-exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    boot = TRUE, nboot = 100
  )

  result4 <- exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    boot = TRUE, nboot = 100, yprevalence = 0.1
  )

  expect_output(print(result1))
  expect_output(print(result2))
  expect_output(print(result3))
  expect_output(print(result4))

})

test_that("Getting a message", {
  expect_message(exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0
  ))

  expect_message(exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    m_cov = c("C1", "C2"), y_cov = c("C1", "C2"), adjusted = FALSE
  ))

  expect_warning(exactmed_c(
    data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
    mf = 12
  ))

})

test_that("Getting an error due to incorrect type of 'data' parameter", {
  datamed_c3 <- as.matrix(datamed_c)

  expect_error(
    exactmed_c(data = datamed_c3, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0),
    "'data' must be a data frame with column names"
  )
})

test_that("Getting an error due to missing values in 'data'", {
  datamed_c4 <- datamed_c
  datamed_c4[1, 2] <- NA

  expect_error(
    exactmed_c(data = datamed_c4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0),
    "'data' contains missing values"
  )
})

test_that("Getting an error due to duplicated column names in 'data'", {
  datamed_c4 <- datamed_c
  colnames(datamed_c4)[5] <- "C1"

  expect_error(
    exactmed_c(data = datamed_c4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0),
    "'data' has duplicated column names"
  )
})

test_that("Getting an error due to an unnamed column in 'data'", {
  datamed_c4 <- datamed_c
  colnames(datamed_c4)[5] <- NA

  expect_error(
    exactmed_c(data = datamed_c4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0),
    "'data' has some unnamed columns"
  )
})

test_that("Error messages: 'a', 'm' or 'y' parameter", {
  expect_error(
    exactmed_c(data = datamed_c, a = "V", m = "M", y = "Y", a1 = 1, a0 = 0),
    "'a' has to be a column name of 'data'"
  )

  expect_error(
    exactmed_c(data = datamed_c, a = "X", m = 3, y = "Y", a1 = 1, a0 = 0),
    "'m' has to be a column name of 'data'"
  )

  expect_error(
    exactmed_c(data = datamed_c, a = "X", m = "M", y = c(1, 2), a1 = 1, a0 = 0),
    "'y' has to be a column name of 'data'"
  )
})

test_that("Error messages: mediator, exposure or response", {
  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      mf = "a"
    ),
    "'mf' must be NULL or a valid real number"
  )

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      hvalue_y = NA
    ),
    "Invalid type or length for input parameter 'hvalue_y'"
  )

  datamed_c4 <- datamed_c
  n <- nrow(datamed_c4)
  Xb <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
  datamed_c4$X <- Xb

  expect_error(
    exactmed_c(data = datamed_c4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0),
    "Exposure must be numerical variable"
  )

  datamed_c4 <- datamed_c
  Mb <- sample(c("a", "b", "c"), n, replace = TRUE)
  datamed_c4$M <- Mb

  expect_error(
    exactmed_c(data = datamed_c4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0),
    "Mediator must be numerical variable"
  )


  datamed_c4 <- datamed_c
  Yb <- sample(c(1, 2, 3), n, replace = TRUE)
  datamed_c4$Y <- Yb

  expect_error(
    exactmed_c(data = datamed_c4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0),
    "Outcome takes more than two different values in 'data'"
  )

  datamed_c4 <- datamed_c
  Yb <- factor(sample(c(1, 2), n, replace = TRUE))
  datamed_c4$Y <- Yb

  expect_error(
    exactmed_c(
      data = datamed_c4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      hvalue_y = NULL
    ),
    "High level for the outcome must be specified"
  )

  expect_error(
    exactmed_c(
      data = datamed_c4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      hvalue_y = "3"
    ),
    "Invalid value for high level of outcome"
  )

  datamed_c4 <- datamed_c
  Yb <- sample(c(1, 2), n, replace = TRUE)
  datamed_c4$Y <- Yb

  expect_error(
    exactmed_c(
      data = datamed_c4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      hvalue_y = NULL
    ),
    "High level for the outcome must be specified"
  )

  expect_error(
    exactmed_c(
      data = datamed_c4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      hvalue_y = 3
    ),
    "Invalid value for high level of outcome"
  )

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      hvalue_y = 3
    ),
    "Invalid value for high level of outcome"
  )
})

test_that("Error and warning messages: numeric parameters", {
  expect_error(
    exactmed_c(data = datamed_c, a = "X", m = "M", y = "Y", a1 = 2 + 3i, a0 = 0),
    "'a1' has to be a real number"
  )

  expect_error(
    exactmed_c(data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = "hjk"),
    "'a0' has to be a real number"
  )

  expect_warning(
    exactmed_c(data = datamed_c, a = "X", m = "M", y = "Y", a1 = 0, a0 = 1),
    "The value of the low level of exposure is not smaller than that of the high level"
  )

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      confcoef = 1.25
    ),
    "'confcoef' has to be a valid real number"
  )

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      yprevalence = -1.25
    ),
    "'yprevalence' must be NULL or a valid real number"
  )

})

test_that("Error messages: logical parameters", {
  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      interaction = "k"
    ),
    "'interaction' must specify a logical value"
  )

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      Firth = 4
    ),
    "'Firth' must specify a logical value"
  )

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      adjusted = c(1, 2)
    ),
    "'adjusted' must specify a logical value"
  )

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      boot = NULL
    ),
    "'boot' must specify a logical value"
  )
})

test_that("Error messages: 'boot' or 'bootseed' parameter", {
  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      boot = TRUE, nboot = 34.4
    ),
    "'nboot' has to be an integer"
  )

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      boot = TRUE, bootseed = NA
    ),
    "'bootseed' has to be an integer"
  )
})

test_that("Error messages: 'm_cov' or 'y_cov' parameter", {
  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = 3, y_cov = c("C1", "C2")
    ),
    "'m_cov' must be NULL or a vector of covariate names"
  )

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", NA), y_cov = c("C1", "C2")
    ),
    "'m_cov' has NAs"
  )

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C1"), y_cov = c("C1", "C2")
    ),
    "'m_cov' has duplicated covariates names"
  )


  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2", "C3"), y_cov = c("C1", "C2")
    ),
    "'m_cov' can only contain names of covariates included in the data frame"
  )


  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = 3
    ),
    "'y_cov' must be NULL or a vector of covariate names"
  )

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", NA)
    ),
    "'y_cov' has NAs"
  )

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C1")
    ),
    "'y_cov' has duplicated covariates names"
  )

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2", "C3")
    ),
    "'y_cov' can only contain names of covariates included in the data frame"
  )
})

test_that("Error messages: 'm_cov_cond' or 'y_cov_cond' parameter", {
  y_cov_cond <- c(0.5, 0.2)
  names(y_cov_cond) <- c("C1", "C2")

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = datamed_c, y_cov_cond = y_cov_cond
    ),
    "'m_cov_cond' must be NULL or a vector"
  )


  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = c(0.5, 0.2), y_cov_cond = y_cov_cond
    ),
    "'m_cov_cond' must be a named vector"
  )

  m_cov_cond <- c(0.5, 0.2)
  names(m_cov_cond) <- c("C1")

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "'m_cov_cond' has missing names"
  )


  names(m_cov_cond) <- c("C1", "C1")

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "'m_cov_cond' has duplicated names"
  )


  names(m_cov_cond) <- c("C1", "C3")

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "The names of the elements of 'm_cov_cond' must be in 'm_cov'"
  )

  m_cov_cond <- list(0.5, list(0.2, 0.5))
  names(m_cov_cond) <- c("C1", "C2")

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "'m_cov_cond' has a invalid value in the "
  )

  rm(y_cov_cond)
  m_cov_cond <- c(0.5, 0.2)
  names(m_cov_cond) <- c("C1", "C2")

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = datamed_c
    ),
    "'y_cov_cond' must be NULL or a vector"
  )

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = c(0.5, 0.2)
    ),
    "'y_cov_cond' must be a named vector"
  )

  y_cov_cond <- c(0.5, 0.2)
  names(y_cov_cond) <- c("C1")

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "'y_cov_cond' has missing names"
  )

  names(y_cov_cond) <- c("C1", "C1")

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "'y_cov_cond' has duplicated names"
  )

  names(y_cov_cond) <- c("C1", "C3")

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "The names of the elements of 'y_cov_cond' must be in 'y_cov'"
  )

  y_cov_cond <- list(0.5, list(0.2, 0.5))
  names(y_cov_cond) <- c("C1", "C2")

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "'y_cov_cond' has a invalid value in the "
  )

  y_cov_cond <- c(0.4, 0.2)
  names(y_cov_cond) <- c("C1", "C2")

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    " has two different values specified"
  )

  y_cov_cond <- 0.2
  names(y_cov_cond) <- "C2"

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1", "C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    " has two different values specified "
  )

  m_cov_cond <- 0.2
  names(m_cov_cond) <- "C2"
  y_cov_cond <- 0.3
  names(y_cov_cond) <- "C1"

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1", "C2"), y_cov = c("C1"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    " has two different values specified "
  )

  m_cov_cond <- "a"
  names(m_cov_cond) <- "C2"
  y_cov_cond <- 0.3
  names(y_cov_cond) <- "C1"

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C2"), y_cov = c("C1"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "Invalid value for "
  )

  m_cov_cond <- 0.3
  names(m_cov_cond) <- "C2"
  y_cov_cond <- "a"
  names(y_cov_cond) <- "C1"

  expect_error(
    exactmed_c(
      data = datamed_c, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C2"), y_cov = c("C1"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "Invalid value for "
  )

  datamed_c4 <- datamed_c
  datamed_c4$C1 <- sample(c("a", "b", "c"), nrow(datamed_c4), replace = TRUE)

  m_cov_cond <- "d"
  names(m_cov_cond) <- "C1"
  y_cov_cond <- 2
  names(y_cov_cond) <- "C2"

  expect_error(
    exactmed_c(
      data = datamed_c4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C1"), y_cov = c("C2"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "Invalid value for "
  )

  m_cov_cond <- 2
  names(m_cov_cond) <- "C2"
  y_cov_cond <- "d"
  names(y_cov_cond) <- "C1"

  expect_error(
    exactmed_c(
      data = datamed_c4, a = "X", m = "M", y = "Y", a1 = 1, a0 = 0,
      m_cov = c("C2"), y_cov = c("C1"),
      m_cov_cond = m_cov_cond, y_cov_cond = y_cov_cond
    ),
    "Invalid value for "
  )
})

