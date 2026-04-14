#' Display a summary of the results of \code{seafar_multistart()}.
#'
#' @param object Object of class inheriting from 'multistart'.
#' @param disp The default is \code{"loadings"} which returns only the estimated loadings matrix.
#'             Otherwise if \code{"full"}, function returns both the estimated loadings and factor scores matrices.
#' @param nrow Number of rows of the loading matrix to be displayed, default is 10.
#' @param ...  Argument to be passed to or from other methods.
#' @export
#' @examples
#'\dontrun{
#' summary(big5_result, disp = "full")
#'}
summary.multistart <- function(object, disp = "loadings", nrow = 10, ...){
  if (missing(disp)){
    disp = "loadings"
  }

  load_fmt <- formatC(object$loadings, format = "f", digits = 3, drop0trailing = TRUE)

  if (disp == "loadings"){
    cat(sprintf("\nThe number of nonzero loadings is: %s\n",
                sum(round(object$loadings,3) != 0)))
    cat(sprintf("\nThe estimated loadings matrix is \n"))
    print(noquote(utils::head(load_fmt, nrow)))
  } else if (disp == "full") {
    cat(sprintf("\nThe number of nonzero loadings is: %s\n",
                sum(round(object$loadings,3) != 0)))

    cat(sprintf("\nThe estimated loadings matrix is \n"))
    print(noquote(utils::head(load_fmt, nrow)))

    cat(sprintf("\nThe estimated factor scores matrix is \n"))
    print(object$scores)
  }

}
