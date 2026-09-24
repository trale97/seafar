#' Multistart procedure for seafar.
#'
#' @param data A data frame or matrix (NxJ). Data are always centered internally.
#' @param nfactors Number of factors Q.
#' @param C Number of nonzero loadings.
#' @param eps Convergence criterion based on difference in loss between iterates.
#' @param maxiter Maximum number of iterations of the AO procedure.
#' @param INIT Method to initializing loadings.
#' @param initloadings Initial loading matrix if available.
#' @param orthogonal Orthogonal or non-orthogonal factors, default is FALSE.
#' @param nstarts Number of starts.
#' @param show_progress Option of print progress bar, default is FALSE.
#' @param standardize Logical. If \code{TRUE} (default), items are scaled to unit variance (N denominator)
#'   after centering. Centering is applied regardless.
#'
#' @returns
#' \item{loadings}{The best estimated loading matrix.}
#' \item{scores}{The best estimated factor score matrix.}
#' \item{PVE}{A list of vectors of PVE of each starting value.}
#' \item{Loss}{A vector of loss values of the best starting value.}
#' \item{center}{Item means used to center the data.}
#' \item{scale}{Item standard deviations used to scale the data (1s if \code{standardize = FALSE}).}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' big5_result <- seafar_multistart(
#'   big5,
#'   nfactors = 5,
#'   C = 240,
#'   INIT = "random",
#'   orthogonal = TRUE,
#'   nstarts = 50
#' )
#' }
seafar_multistart <- function(data,
                              nfactors,
                              C,
                              maxiter = 50,
                              eps = 10^-4,
                              INIT = "semirational",
                              initloadings = NULL,
                              orthogonal = FALSE,
                              nstarts = 50,
                              show_progress = FALSE,
                              standardize = TRUE) {
  Pout3d <- list()
  Hout3d <- list()
  LOSS <- array()
  PVE <- list()
  LOSSvec <- list()
  if (show_progress == TRUE) {
    pb <- txtProgressBar(min = 0, max = nstarts, style = 3)
  }


  valid_index <- 0 # counts only successful runs

  for (n in 1:nstarts) {
    # catch when seafar throws an error to skip that start
    result <- tryCatch(
      {
        seafar(
          data = data,
          nfactors = nfactors,
          C = C,
          maxiter = maxiter,
          eps = eps,
          INIT = INIT,
          initloadings = initloadings,
          orthogonal = orthogonal,
          standardize = standardize
        )
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
    xcenter <- result$center # same for every start; taken from a successful run
    xscale <- result$scale

    if (show_progress) setTxtProgressBar(pb, n)
  }

  if (show_progress == TRUE) close(pb)

  if (valid_index == 0) {
    stop("all ", nstarts, " starts failed; run seafar() once to see the error")
  }
  if (valid_index < nstarts) {
    warning(nstarts - valid_index, " of ", nstarts, " starts failed and were skipped")
  }

  # choose solution with lowest loss value
  k <- which(LOSS == min(LOSS))
  if (length(k) > 1) {
    pos <- sample(1:length(k), 1)
    k <- k[pos]
  }

  return_varselect <- list()
  return_varselect$loadings <- Pout3d[[k]]
  return_varselect$scores <- Hout3d[[k]]
  return_varselect$PVE <- PVE
  return_varselect$Loss <- LOSS[k]
  return_varselect$center <- xcenter
  return_varselect$scale <- xscale

  attr(return_varselect, "class") <- "multistart"

  return(return_varselect)
}
