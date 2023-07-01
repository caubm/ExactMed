#' @noRd
#' @importFrom utils head
#' @export


print.results_cat <- function(x, ...) {
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
    cat("Controlled direct effect (m='", x[[7]], "'): \n", sep = "")
    cat("\n")
    print(x[[4]])

    if(!("coeftest" %in% class(x[[5]]))) {
      cat("\n")
      cat("\n")
      cat("Mediator model: \n")
      cat("\n")
      print(summary(x[[5]]))

      cat("\n")
      cat("Outcome model: \n")
      cat("\n")
      print(summary(x[[6]]))
    } else {
      cat("\n")
      cat("\n")
      cat("Mediator model: \n")
      cat("\n")
      print(x[[5]])

      cat("\n")
      cat("Outcome model:", "\n")
      cat("\n")
      print(x[[6]])
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
    cat("Controlled direct effect (m='", x[[14]], "'): \n", sep = "")
    cat("\n")
    print(x[[4]])

    cat("\n")
    cat("First bootstrap replications of natural effects on OR scale: \n")
    cat("\n")
    print(head(x[[5]], n = 3L))

    cat("\n")
    cat("First bootstrap replications of natural effects on RR scale: \n")
    cat("\n")
    print(head(x[[6]], n = 3L))

    cat("\n")
    cat("First bootstrap replications of natural effects on RD scale: \n")
    cat("\n")
    print(head(x[[7]], n = 3L))

    cat("\n")
    cat("First bootstrap replications of controlled direct effect on OR scale: \n")
    cat("\n")
    print(head(x[[8]], n = 3L))

    cat("\n")
    cat("First bootstrap replications of controlled direct effect on RR scale: \n")
    cat("\n")
    print(head(x[[9]], n = 3L))

    cat("\n")
    cat("First bootstrap replications of controlled direct effect on RD scale: \n")
    cat("\n")
    print(head(x[[10]], n = 3L))

    cat("\n")
    cat("\n")
    cat("Mediator model: \n")
    cat("\n")
    print(summary(x[[12]]))

    cat("\n")
    cat("Outcome model: \n")
    cat("\n")
    print(summary(x[[13]]))

  }
  invisible(x)
}


# print.results <- function(x, ...) {
#   for(i in 1:length(x)) {
#     cat("\n")
#     cat(names(x)[i], ":", "\n", sep = "")
#     if(grepl("boot", names(x)[i])) {
#       cat("\n")
#       print(head(x[[i]]))
#     } else {
#       cat("\n")
#       print(x[[i]])
#     }
#   }
#   invisible(x)
# }




