#' Model selection for SEAFAR using Index of Sparseness with warm start
#'
#' @param data A NxJ matrix of standardized items.
#' @param nfactors Number of factors.
#' @param maxiter Maximum number of iterations for the AO procedure.
#' @param eps Convergence criterion based on difference in loss between iterates.
#' @param INIT Method to initialize loading matrix.
#' @param orthogonal Orthogonal or non-orthogonal factors, default is FALSE.
#' @param card.length Number of candidate cardinality values to test.
#' @param show_progress Option of print progress bar, default is FALSE.
#'
#' @returns A list with
#' \item{value}{IS value.}
#' \item{vaf}{Variance accounted for.}
#' \item{propzero}{Proportion of zero loadings.}
#' \item{smallestP}{Smallest nonzero loading.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' IS_result <- is.seafar_wst(
#'   data = X,
#'   nfactors = 5,
#'   INIT = "random",
#'   orthogonal = TRUE,
#'   card.length = 100
#' )
#' }
is.seafar_wst <- function(data,
                          nfactors,
                          eps = 1e-4,
                          maxiter = 50,
                          INIT,
                          orthogonal = FALSE,
                          card.length,
                          show_progress = FALSE) {
  IS <- list()
  n <- dim(data)[1]
  J <- dim(data)[2]
  nrcoef <- J * nfactors

  # obtain initial loading matrix
  loadings <- seafar_init(data, nfactors, C = nrcoef, INIT = INIT)
  pvepca <- (n / (n - 1)) * sum(rowSums(loadings^2)) / J # ! depends on scaling+orth factors
  PVE <- pvepca
  propzero <- 0
  min_nzero_loading <- NA
  isvalue <- 0
  if (nfactors * (J - 1) > card.length) {
    regpath <- round(seq(((J * nfactors) - 1), nfactors * 3, length = card.length))
  } else {
    regpath <- seq(((J * nfactors) - 1), nfactors * 3)
  }

  if (show_progress == TRUE) {
    pb <- txtProgressBar(min = 0, max = length(regpath), style = 3)
  }


  for (i in 1:length(regpath)) {
    card <- regpath[i]
    seafa_result <- seafar(data, nfactors, C = card, initloadings = loadings, INIT = INIT, orthogonal = orthogonal)
    loadings <- seafa_result$loadings
    # scores <- seafa_result$scores
    pveseafa <- seafa_result$PVE[length(seafa_result$PVE)]
    PVE <- c(PVE, pveseafa)
    propzero <- c(propzero, 1 - card / nrcoef)
    is <- pvepca * pveseafa * (1 - card / nrcoef)
    isvalue <- c(isvalue, is)
    minl <- ifelse(sum(colSums(loadings != 0)) > card, 0,
      min(abs(loadings[loadings != 0]))
    )
    min_nzero_loading <- c(min_nzero_loading, minl)

    if (show_progress == TRUE) {
      setTxtProgressBar(pb, i)
    }
  }

  if (show_progress == TRUE) {
    close(pb)
  }

  cardinality <- c(nrcoef, regpath)

  IS$cardinality <- cardinality
  IS$value <- isvalue
  IS$pve <- PVE
  IS$propzero <- propzero
  IS$smallestP <- min_nzero_loading
  IS$selcard <- cardinality[which.max(isvalue)]
  attr(IS, "class") <- "ISwarmstart"

  return(IS)
}

#' Display a summary of the results of \code{is.seafar_wst()}.
#'
#' @param object Object of class inheriting from 'ISwarmstart'.
#' @param ... Argument to be passed to or from other methods.
#'
#' @returns Summary of the results.
#' @export
#'
#' @examples
#' \dontrun{
#' summary(IS_result)
#' }
summary.ISwarmstart <- function(object, ...) {
  cat(sprintf(
    "The cardinality selected by the Index of Sparseness is: %s\n",
    object$cardinality[which.max(object$value)]
  ))

  cat(sprintf(
    "The PEV with this cardinality is: %.3f\n",
    object$pve[which.max(object$value)]
  ))

  cat(sprintf(
    "The PEV of the unconstrained model (no zero loadings) is: %.3f\n",
    object$pve[1]
  ))
}
