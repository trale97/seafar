#' Multistart procedure for seafar.
#'
#' @param data Data matrix of standardized items NxJ.
#' @param nfactors Number of factors Q.
#' @param C Number of nonzero loadings.
#' @param eps Convergence criterion based on difference in loss between iterates.
#' @param maxiter Maximum number of iterations of the AO procedure.
#' @param INIT Method to initializing loadings.
#' @param initloadings Initial loading matrix if available.
#' @param orthogonal Orthogonal or non-orthogonal factors, default is FALSE.
#' @param nstarts Number of starts.
#' @param show_progress Option of print progress bar, default is FALSE.
#'
#' @returns
#'\item{loadings}{The best estimated loading matrix.}
#'\item{scores}{The best estimated factor score matrix.}
#'\item{PVE}{A list of vectors of PVE of each starting value.}
#'\item{Loss}{A vector of loss values of the best starting value.}
#'
#' @export
#'
#' @examples
#'\dontrun{
#' big5_result <- seafar_multistart(
#'   scale(big5),
#'   nfactors = 5,
#'   C = 240,
#'   INIT = 'random',
#'   orthogonal = TRUE,
#'   nstarts = 50)
#'}
seafar_multistart <- function(data,
                              nfactors,
                              C,
                              maxiter = 50,
                              eps = 10^-4,
                              INIT,
                              initloadings,
                              orthogonal = FALSE,
                              nstarts = 50,
                              show_progress = FALSE) {

  Pout3d <- list()
  Hout3d <- list()
  LOSS <- array()
  PVE <- list()
  LOSSvec <- list()
  if (show_progress == TRUE){
    pb <- txtProgressBar(min = 0, max = nstarts, style = 3)
  }


  valid_index <- 0  # counts only successful runs

  for (n in 1:nstarts) {

    # catch when seafar throws an error to skip that start
    result <- tryCatch(
      {
        seafar(data = data,
               nfactors = nfactors,
               C = C,
               maxiter = maxiter,
               eps = eps,
               INIT = INIT,
               initloadings = initloadings,
               orthogonal = orthogonal)
      },
      error = function(e) {
        return(NULL)
      }
    )

    # Skip failed runs
    if (is.null(result)) {
      if (show_progress) setTxtProgressBar(pb, n)
      next
    }

    # Store successful run
    valid_index <- valid_index + 1
    Pout3d[[valid_index]] <- result$loadings
    Hout3d[[valid_index]] <- result$scores
    LOSS[valid_index] <- result$Residual
    PVE[[valid_index]] <- result$PVE
    LOSSvec[[valid_index]] <- 1 - result$PVE

    if (show_progress) setTxtProgressBar(pb, n)
  }

  if (show_progress == TRUE) close(pb)

  # choose solution with lowest loss value
  k <- which(LOSS == min(LOSS))
  if (length(k)>1){
    pos <- sample(1:length(k), 1)
    k <- k[pos]
  }

  return_varselect <- list()
  return_varselect$loadings <- Pout3d[[k]]
  return_varselect$scores <- Hout3d[[k]]
  return_varselect$PVE <- PVE
  return_varselect$Loss <- LOSS[k]

  attr(return_varselect, "class") <- "multistart"

  return(return_varselect)
}

#' Display a summary of the results of \code{seafar_multistart()}.
#'
#' @param object Object of class inheriting from 'multistart'.
#' @param disp The default is \code{"loadings"} which returns only the estimated loadings matrix.
#'             Otherwise if \code{"full"}, function returns both the estimated loadings and factor scores matrices.
#' @param ...  Argument to be passed to or from other methods.
#'
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
    print(round(object$loadings,3))
  } else if (disp == "full") {
    cat(sprintf("\nThe estimated loadings matrix is \n"))
    print(round(object$loadings,3))

    cat(sprintf("\nThe estimated factor scores matrix is \n"))
    print(object$scores)
  }

}
