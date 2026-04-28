#' Function to retain number of factors/components using various methods.
#'
#' @param data A NxJ matrix of standardized items.
#' @param large.data Logical. If TRUE, use methods suitable for very large datasets.
#'
#' @returns Number of factors/components.

#' @importFrom utils capture.output
#' @importFrom stats cor

#' @export
#'
#' @examples
#' \dontrun{
#' data(big5, package = "qgraph")
#' X <- scale(big5)
#' a <- factor_retention(X, large.data = FALSE)
#' }
factor_retention <- function(data,
                             large.data = FALSE) {
  if (large.data == FALSE) {
    result <- factor_small(data)
  } else {
    result <- factor_large(data)
  }

  attr(result, "class") <- "nfactors"
  return(result)
}

#' Function to retain number of factors/components for traditional size data set using various methods.
#'
#' @param data A NxJ matrix of standardized items.
#'
#' @returns Number of factors/components.
#' @export
#'
#' @examples
#' \dontrun{
#' data(big5, package = "qgraph")
#' X <- scale(big5)
#' a <- factor_small(X)
#' }
factor_small <- function(data) {
  Q_parallel <- tryCatch(
    {
      tmp <- utils::capture.output(
        Q <- psych::fa.parallel(data, fa = "fa", plot = FALSE)
      )
      Q$nfact
    },
    error = function(e) {
      NA
    }
  )

  # scree
  pX <- PCAtools::pca(data, removeVar = 0.1)
  Q_elbow <- PCAtools::findElbowPoint(pX$variance)

  # Kaiser
  kaiser <- EFA.dimensions::EMPKC(data, verbose = FALSE)
  Q_kaiser <- kaiser$NfactorsEMPKC

  number_factors <- list(
    parallel = Q_parallel,
    scree = Q_elbow,
    kaiser = Q_kaiser
  )

  return(number_factors)
}

#' Function to retain number of factors/components for very large data set using various methods.
#'
#' @param data A NxJ matrix of standardized items.
#'
#' @returns Number of factors/components.
#' @export
#'
#' @examples
#' \dontrun{
#' data(big5, package = "qgraph")
#' X <- scale(big5)
#' a <- factor_large(X)
#' }
factor_large <- function(data) {
  # parallel analysis components
  horn <- PCAtools::parallelPCA(data)
  Q_parallel_comp <- horn$n

  # scree
  pX <- PCAtools::pca(data, removeVar = 0.1)
  Q_elbow <- PCAtools::findElbowPoint(pX$variance)

  number_factors <- list(
    parallel = Q_parallel_comp,
    scree = Q_elbow
  )

  return(number_factors)
}
