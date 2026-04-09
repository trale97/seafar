#' Function to retain number of factors using various methods.
#'
#' @param data A NxJ matrix of standardized items.
#'
#' @returns Number of factors.

#' @importFrom utils capture.output
#' @importFrom stats cor

#' @export
#'
#' @examples
#'\dontrun{
#' data(big5, package = "qgraph")
#' X <- scale(big5)
#' a <- factor_retention(X)
#'}
factor_retention <- function(data){
  # default values
  Q_parallel <- NA
  Q_parallel_comp <- NA
  parallel_error <- NULL

  res <- tryCatch(
    {
      tmp <- utils::capture.output(
        Q <- psych::fa.parallel(data, plot = FALSE)
      )
      list(nfact = Q$nfact, ncomp = Q$ncomp)
    },
    error = function(e){
      parallel_error <<- conditionMessage(e)
      NULL
    }
  )

  if(!is.null(res)){
    Q_parallel <- res$nfact
    Q_parallel_comp <- res$ncomp
  }

  # using scree plot
  pX <- PCAtools::pca(data, removeVar = .1)
  # Kaiser
  kaiser <- EFA.dimensions::EMPKC(data, verbose = FALSE)
  Q_kaiser <- kaiser$NfactorsEMPKC
  Q_elbow <- PCAtools::findElbowPoint(pX$variance)

  # factors retention using kaiser rule
  ev <- eigen(cor(data))$values
  Q_kaiser <- sum(ev > 1)

  number_factors <- list()
  number_factors$parallel <- Q_parallel
  number_factors$parallel_comp <- Q_parallel_comp
  number_factors$scree <- Q_elbow
  number_factors$kaiser <- Q_kaiser
  number_factors$parallel_error <- parallel_error
  attr(number_factors, "class") <- "nfactors"

  return(number_factors)
}


