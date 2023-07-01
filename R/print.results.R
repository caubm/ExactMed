#' @noRd
#' @importFrom utils head
#' @export


print.results <- function(x, ...) {
  if(length(x) == 7) {
    cat("\n")
    cat("Natural effects on OR scale: \n")
    cat("\n")
    print(x[[1]])

    cat("\n")
    cat("Natural effects on RR scale: \n")
    cat("\n")
    print(x[[2]])

    cat("\n")
    cat("Natural effects on RD scale: \n")
    cat("\n")
    print(x[[3]])

    cat("\n")
    cat("Controlled direct effect (m=0): \n")
    cat("\n")
    print(x[[4]])

    cat("\n")
    cat("Controlled direct effect (m=1): \n")
    cat("\n")
    print(x[[5]])

    if(!("coeftest" %in% class(x[[6]]))) {
      cat("\n")
      cat("\n")
      cat("Mediator model: \n")
      cat("\n")
      print(summary(x[[6]]))

      cat("\n")
      cat("Outcome model: \n")
      cat("\n")
      print(summary(x[[7]]))
    } else {
      cat("\n")
      cat("\n")
      cat("Mediator model: \n")
      cat("\n")
      print(x[[6]])

      cat("\n")
      cat("Outcome model: \n")
      cat("\n")
      print(x[[7]])
    }
  } else {
    cat("\n")
    cat("Natural effects on OR scale: \n")
    cat("\n")
    print(x[[1]])

    cat("\n")
    cat("Natural effects on RR scale: \n")
    cat("\n")
    print(x[[2]])

    cat("\n")
    cat("Natural effects on RD scale: \n")
    cat("\n")
    print(x[[3]])

    cat("\n")
    cat("Controlled direct effect (m=0): \n")
    cat("\n")
    print(x[[4]])

    cat("\n")
    cat("Controlled direct effect (m=1): \n")
    cat("\n")
    print(x[[5]])

    cat("\n")
    cat("First bootstrap replications of natural effects on OR scale: \n")
    cat("\n")
    print(head(x[[6]], n = 3L))

    cat("\n")
    cat("First bootstrap replications of natural effects on RR scale: \n")
    cat("\n")
    print(head(x[[7]], n = 3L))

    cat("\n")
    cat("First bootstrap replications of natural effects on RD scale: \n")
    cat("\n")
    print(head(x[[8]], n = 3L))

    cat("\n")
    cat("First bootstrap replications of controlled direct effect (m=0) on OR scale: \n")
    cat("\n")
    print(head(x[[9]], n = 3L))

    cat("\n")
    cat("First bootstrap replications of controlled direct effect (m=0) on RR scale: \n")
    cat("\n")
    print(head(x[[10]], n = 3L))

    cat("\n")
    cat("First bootstrap replications of controlled direct effect (m=0) on RD scale: \n")
    cat("\n")
    print(head(x[[11]], n = 3L))

    cat("\n")
    cat("First bootstrap replications of controlled direct effect (m=1) on OR scale: \n")
    cat("\n")
    print(head(x[[12]], n = 3L))

    cat("\n")
    cat("First bootstrap replications of controlled direct effect (m=1) on RR scale: \n")
    cat("\n")
    print(head(x[[13]], n = 3L))

    cat("\n")
    cat("First bootstrap replications of controlled direct effect (m=1) on RD scale: \n")
    cat("\n")
    print(head(x[[14]], n = 3L))

    cat("\n")
    cat("\n")
    cat("Mediator model: \n")
    cat("\n")
    print(summary(x[[16]]))

    cat("\n")
    cat("Outcome model: \n")
    cat("\n")
    print(summary(x[[17]]))

  }
  invisible(x)
}





