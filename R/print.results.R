#' @noRd
#' @importFrom utils head
#' @export

print.results <- function(x, ...) {
  for(i in 1:length(x)) {
    cat("\n")
    cat(names(x)[i], ":", "\n", sep = "")
    if(grepl("boot", names(x)[i])) {
      cat("\n")
      print(head(x[[i]]))
    } else {
      cat("\n")
      print(x[[i]])
    }
  }
  invisible(x)
}
