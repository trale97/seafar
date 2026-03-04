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
  tmp <- capture.output(
    Q <- psych::fa.parallel(data, plot = F)
  )
  Q_parallel <- Q$nfact
  Q_parallel_comp <- Q$ncomp

  # using scree plot
  pX <- PCAtools::pca(data, removeVar = .1)
  Q_elbow <- PCAtools::findElbowPoint(pX$variance)

  # factors retention using kaiser rule
  ev <- eigen(cor(data))$values
  Q_kaiser <- sum(ev > 1)

  number_factors <- list()
  number_factors$parallel <- Q_parallel
  number_factors$parallel_comp <- Q_parallel_comp
  number_factors$scree <- Q_elbow
  number_factors$kaiser <- Q_kaiser
  attr(number_factors, "class") <- "nfactors"

  return(number_factors)
}


