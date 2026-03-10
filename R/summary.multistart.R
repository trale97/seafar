#' Display a summary of the results of \code{seafar_multistart()}.
#'
#' @param object Object of class inheriting from 'multistart'.
#' @param disp The default is \code{"loadings"} which returns only the estimated loadings matrix.
#'             Otherwise if \code{"full"}, function returns both the estimated loadings and factor scores matrices.
#' @param ...  Argument to be passed to or from other methods.
#' @export
#' @examples
#'\dontrun{
#' summary(big5_result, disp = "full")
#'}
summary.multistart <- function(object, disp = "loadings", ...){
  if (missing(disp)){
    disp = "loadings"
  }

  if (disp == "loadings"){
    cat(sprintf("\nThe number of nonzero loadings is: %s\n",
                sum(round(object$loadings,3) != 0)))
    cat(sprintf("\nThe estimated loadings matrix is \n"))
    print(head(round(object$loadings,3),10))
  } else if (disp == "full") {
    cat(sprintf("\nThe number of nonzero loadings is: %s\n",
                sum(round(object$loadings,3) != 0)))

    cat(sprintf("\nThe estimated loadings matrix is \n"))
    print(head(round(object$loadings,3),10))

    cat(sprintf("\nThe estimated factor scores matrix is \n"))
    print(object$scores)
  }

}
