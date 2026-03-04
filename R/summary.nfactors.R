#' Display a summary of the results of \code{factor_retention()}.
#'
#' @param object Object of class inheriting from 'factor_retention'.
#' @param ... Argument to be passed to or from other methods.
#'
#' @returns Summary of the number of factors retained using various methods.
#' @export
#'
#' @examples
#'\dontrun{
#' results <- factor_retention(X)
#' summary(results)
#'}
summary.nfactors <- function(object, ...){
  if(!is.null(object$parallel_error)){
    cat("Parallel analysis failed:\n")
    cat(object$parallel_error, "\n\n")
  } else {
    cat(sprintf(
      "Number of factors by parallel analysis is: %s\n",
      object$parallel
    ))
  }

  cat(sprintf("Number of factors by scree test is: %s\n",
              object$scree))

  cat(sprintf("Number of factors by Kaiser rule is: %s\n",
              object$kaiser))

}
