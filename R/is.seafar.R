#' Model selection for SEAFAR using the Index of Sparseness.
#'
#' @param data A NxJ matrix of standardized items.
#' @param nfactors Number of factors.
#' @param C Number of nonzero loadings.
#' @param maxiter Maximum number of iterations for the AO procedure.
#' @param eps Convergence criterion based on difference in loss between iterates.
#' @param INIT Method to initialize loading matrix.
#' @param orthogonal Orthogonal or non-orthogonal factors, default is FALSE.
#' @param nstarts Number of starts.
#'
#' @importFrom stats sd
#'
#' @returns A list with
#' \item{value}{IS value.}
#' \item{vaf}{Variance accounted for.}
#' \item{propzero}{Proportion of zero loadings.}
#' \item{smallestP}{Smallest nonzero loading.}
#' \item{maxsdP}{Maximum standard deviation of loading.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' IS <- is.seafar_original(
#'   data = X,
#'   nfactors = 5,
#'   C = 240,
#'   INIT = "random",
#'   orthogonal = TRUE,
#'   nstarts = 50
#' )
#' }
#'
is.seafar_original <- function(data,
                               nfactors,
                               C,
                               maxiter = 50,
                               eps = 10^-4,
                               INIT,
                               orthogonal = FALSE,
                               nstarts) {
  J <- dim(data)[2]
  seafar_result <- seafar_multistart(
    data = data,
    nfactors = nfactors,
    C = C,
    maxiter = maxiter,
    eps = eps,
    INIT = INIT,
    orthogonal = orthogonal,
    nstarts = nstarts,
    show_progress = FALSE
  )
  Hmat <- seafar_result$scores
  Pmat <- seafar_result$loadings

  vzero <- sum(data^2)
  d <- svd(data)$d
  va <- sum((d[1:nfactors])^2)
  vs <- sum((Hmat %*% t(Pmat))^2)
  nrzeqcoef <- sum(round(Pmat, 3) == 0)
  nrcoef <- J * nfactors

  IS <- list()
  IS$value <- va * vs / vzero^2 * nrzeqcoef / nrcoef
  IS$vaf <- vs / vzero
  IS$propzero <- nrzeqcoef / nrcoef

  IS$smallestP <- ifelse(sum(rowSums(Pmat != 0)) < sum(C),
    0, min(abs(Pmat[Pmat != 0]))
  )
  IS$maxsdP <- max(apply(Pmat, 2, sd))

  return(IS)
}


#' Model selection for SEAFAR using modified Index of Sparseness
#' (Van Deun et al., 2025).
#'
#' @param data A NxJ matrix of standardized items.
#' @param nfactors Number of factors.
#' @param maxiter Maximum number of iterations for the AO procedure.
#' @param eps Convergence criterion based on difference in loss between iterates.
#' @param INIT Method to initialize loading matrix.
#' @param orthogonal Orthogonal or non-orthogonal factors, default is FALSE.
#' @param nstarts Number of starts.
#' @param TOL Decimal points for the loadings.
#' @param THR Thresholding selection for the smallest nonzero loading.
#'
#' @returns The selected cardinality value.
#' @export
#'
#' @examples
#' \dontrun{
#' selcard <- is.seafar(
#'   data = X,
#'   nfactors = 5,
#'   INIT = "random",
#'   orthogonal = TRUE,
#'   nstarts = 50,
#'   TOL = 2,
#'   THR = 0.15
#' )
#' }
is.seafar <- function(data,
                      nfactors,
                      maxiter = 50,
                      eps = 10^-4,
                      INIT,
                      orthogonal = FALSE,
                      nstarts,
                      TOL,
                      THR) {
  J <- dim(data)[2]

  col_names <- c("K", "IS", "PEV", "Prop0", "MinNonZeroL", "MaxSDL")
  rows <- list()

  # --- first set of cardinalities ---
  cardvec <- round(seq(3, J, length.out = 100))

  for (k in seq_along(cardvec)) {
    a <- tryCatch(
      is.seafar_original(
        data = data,
        nfactors = nfactors,
        C = rep(cardvec[k], nfactors),
        maxiter = 20,
        eps = 10^-4,
        INIT = INIT,
        orthogonal = orthogonal,
        nstarts = nstarts
      ),
      error = function(e) NULL
    )

    if (is.null(a)) next

    rows[[length(rows) + 1]] <- c(
      cardvec[k], a$value, a$vaf, a$propzero,
      a$smallestP, a$maxsdP
    )
  }

  K <- length(rows)

  # --- second set of cardinalities ---
  if (nfactors * (J - 1) > 100) {
    cardvec <- round(seq(3 * nfactors, J * nfactors - 1, length.out = 100))
  } else {
    cardvec <- seq(3 * nfactors, J * nfactors - 1, by = 1)
  }

  for (l in seq_along(cardvec)) {
    a <- tryCatch(
      is.seafar_original(
        data = data,
        nfactors = nfactors,
        C = cardvec[l],
        maxiter = 20,
        eps = 10^-4,
        INIT = INIT,
        orthogonal = orthogonal,
        nstarts = nstarts
      ),
      error = function(e) NULL
    )

    if (is.null(a)) next

    rows[[length(rows) + 1]] <- c(
      cardvec[l], a$value, a$vaf, a$propzero,
      a$smallestP, a$maxsdP
    )
  }

  # --- combine successful rows only ---
  if (length(rows) > 0) {
    avec <- do.call(rbind, rows)
    avec <- as.matrix(avec)
    colnames(avec) <- col_names
    avec <- round(avec, 3)
  } else {
    avec <- matrix(nrow = 0, ncol = 6)
    colnames(avec) <- col_names
  }

  round(avec, 4)

  KL <- dim(avec)[1]

  placeholder <- seq(1:KL)
  indexnonzeroL <- placeholder[avec[, "MinNonZeroL"] < THR][1] - 1 # THR holding selection
  if (indexnonzeroL < K) {
    placeholder2 <- placeholder[(K + 1):KL]
    indexnonzeroL2 <- placeholder2[avec[(K + 1):KL, "MinNonZeroL"] < THR][1] - 1 # THR holding selection
    if (avec[indexnonzeroL2, 1] < avec[indexnonzeroL, 1] * nfactors) {
      indexnonzeroL <- indexnonzeroL2
    } else if (avec[indexnonzeroL2, 1] == avec[indexnonzeroL, 1] * nfactors) {
      if (round(avec[indexnonzeroL2, 3], THR) > round(avec[indexnonzeroL, 3], THR)) {
        indexnonzeroL <- indexnonzeroL2
      }
    }
  }
  selcardinality <- avec[indexnonzeroL, 1]
  if (indexnonzeroL < K + 1) {
    selcardinality <- c(rep(avec[indexnonzeroL, 1], nfactors))
  }

  # TOL = 2
  samepve <- round(avec[, 3], TOL) == round(avec[indexnonzeroL, 3], TOL)

  index <- indexnonzeroL
  if (is.na(indexnonzeroL) || max(round(avec[samepve, 2], 3)) > round(avec[indexnonzeroL, 2], 3)) {
    index <- placeholder[avec[, 2] == max(avec[samepve, 2])][1]
    selcardinality <- avec[index, 1]
    if (index < K) {
      selcardinality <- rep(avec[index, 1], nfactors)
    }
  }
  attr(selcardinality, "class") <- "is_seafar"

  return(selcardinality)
}

#' Display a summary of the results of \code{is.seafar()}.
#'
#' @param object Object of class inheriting from 'is_seafar'.
#' @param ... Argument to be passed to or from other methods.
#'
#' @returns Summary of the results.
#' @export
#'
#' @examples
#' \dontrun{
#' summary(IS_result)
#' }
summary.is_seafar <- function(object, ...) {
  cat(sprintf(
    "The cardinality selected by the Index of Sparseness is: %s\n",
    object
  ))
}
