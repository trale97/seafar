#' Model selection for SEAFAR with LASSO penalty using the Index of Sparseness.
#'
#' @param data A data frame containing the dataset (standardized).
#' @param nfactors The number of factors.
#' @param lambda LASSO penalty tuning parameter.
#' @param maxiter Maximum number of iterations of the AO procedure.
#' @param eps Convergence criterion based on difference in loss between iterates.
#' @param INIT Method to initialize loadings.
#'
#' @returns A list with
#' \item{value}{IS value.}
#' \item{vaf}{Variance accounted for.}
#' \item{propzero}{Proportion of zero loadings.}
#' \item{smallestP}{Smallest nonzero loading.}
#' \item{maxsdP}{Maximum standard deviation of loading.}
#' @export
#'
#' @examples
#' \dontrun{
#' IS <- is.seafar_lasso(
#'   data = X,
#'   nfactors = 5,
#'   lambda = 100,
#'   INIT = "mixed",
#'   nstarts = 50
#' )
#' }
#'
is.seafar_lasso <- function(data,
                            nfactors,
                            lambda,
                            maxiter,
                            eps,
                            INIT,
                            nstarts) {
  J <- dim(data)[2]

  VarSelect0 <- seafar_lasso_multistart(
    data = data,
    nfactors = nfactors,
    lambda = 0,
    maxiter = maxiter,
    eps = eps,
    INIT = INIT,
    nstarts = nstarts
  )
  P_hat0 <- VarSelect0$loadings
  T_hat0 <- VarSelect0$scores

  V_oo <- sum(data^2)
  V_s <- sum((T_hat0 %*% t(P_hat0))^2)

  VarSelect <- seafar_lasso_multistart(
    data = data,
    nfactors = nfactors,
    lambda = lambda,
    maxiter = maxiter,
    eps = eps,
    INIT = INIT,
    nstarts = nstarts
  )
  P_hat <- VarSelect$loadings
  T_hat <- VarSelect$scores

  card <- sum(P_hat != 0)

  V_a <- sum((T_hat %*% t(P_hat))^2)
  IS <- list()
  IS$value <- (V_a * V_s / V_oo^2) * (sum(round(P_hat, 3) == 0) / (J * nfactors))
  IS$vaf <- V_a / V_oo
  IS$propzero <- sum(round(P_hat, 3) == 0) / (J * nfactors)

  IS$smallestP <- ifelse(sum(rowSums(P_hat != 0)) < sum(card),
    0, min(abs(P_hat[P_hat != 0]))
  )
  IS$maxsdP <- max(apply(P_hat, 2, sd))
  return(IS)
}
