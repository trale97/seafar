#' Function to calculate the IS for each LASSO penalty parameter.
#'
#' @param data Data matrix of standardized items NxJ.
#' @param nfactors The number of factors.
#' @param lambda LASSO penalty tuning parameter.
#' @param maxiter Maximum number of iterations of the AO procedure.
#' @param eps Convergence criterion based on difference in loss between iterates.
#' @param INIT Method to initialize loadings.
#' @param nstarts Number of starts.
#'
#' @returns A list with
#'\item{value}{IS value.}
#'\item{vaf}{Variance accounted for.}
#'\item{propzero}{Proportion of zero loadings.}
#'\item{smallestP}{Smallest nonzero loading.}
#'\item{maxsdP}{Maximum standard deviation of loading.}
#' @export
#'
#' @examples
#'\dontrun{
#' IS <- IS_lasso(
#'   data = X,
#'   nfactors = 5,
#'   lambda = 100,
#'   INIT = 'mixed',
#'   nstarts = 50)
#'}
#'
IS_lasso <- function(data,
                     nfactors,
                     lambda,
                     maxiter = 50,
                     eps = 10^-4,
                     INIT,
                     nstarts){
  J <- dim(data)[2]

  VarSelect0 <- seafar_lasso_multistart(data = data,
                                        nfactors = nfactors,
                                        lambda = 0,
                                        maxiter = maxiter,
                                        eps = eps,
                                        INIT = INIT,
                                        nstarts = nstarts)
  P_hat0 <- VarSelect0$loadings
  T_hat0 <- VarSelect0$scores

  V_oo <- sum(data^2)
  V_s <- sum((T_hat0%*%t(P_hat0))^2)

  VarSelect <- seafar_lasso_multistart(data = data,
                                       nfactors = nfactors,
                                       lambda = lambda,
                                       maxiter = maxiter,
                                       eps = eps,
                                       INIT = INIT,
                                       nstarts = nstarts)
  P_hat <- VarSelect$loadings
  T_hat <- VarSelect$scores

  card <- sum(P_hat != 0)

  V_a <- sum((T_hat %*% t(P_hat))^2)
  IS <- list()
  IS$value <- (V_a * V_s / V_oo^2) * (sum(round(P_hat,3) == 0) /(J*nfactors))
  IS$vaf <- V_a/V_oo
  IS$propzero <- sum(round(P_hat,3) == 0)/(J*nfactors)

  IS$smallestP <- ifelse(sum(rowSums(P_hat != 0)) < sum(card),
                         0, min(abs(P_hat[P_hat != 0]))
  )
  IS$maxsdP <- max(apply(P_hat, 2, sd))
  return(IS)
}

#' Model selection procedure with IS to choose the LASSO penalty paramter.
#'
#' @param data Data matrix of standardized items NxJ.
#' @param nfactors The number of factors.
#' @param max_lambda Maximum LASSO penalty parameter.
#' @param maxiter Maximum number of iterations of the AO procedure.
#' @param eps Convergence criterion based on difference in loss between iterates.
#' @param INIT Method to initialize loading matrix.
#' @param nstarts Number of starts.
#'
#' @returns The selected LASSO penalty parameter.
#' @export
#'
#' @examples
#'\dontrun{
#' lambda <- is.seafar_lasso(
#'   data = X,
#'   nfactors = 5,
#'   max_lambda = 100,
#'   INIT = 'mixed',
#'   nstarts = 50)
#'}
is.seafar_lasso <- function(data,
                     nfactors,
                     max_lambda,
                     maxiter = 50,
                     eps = 10^-4,
                     INIT,
                     nstarts) {
  N <- dim(data)[2]
  I <- dim(data)[1]

  lasso_vec <- seq(0.1*N, max_lambda, length.out = 50)
  K <- length(lasso_vec)

  avec <- matrix(nrow = K, ncol = 6)
  colnames(avec) <- c( "lambda", "IS", "PEV", "Prop0", "MinNonZeroL", "MaxSDL")
  for (k in 1:K) {
    # retry loop
    success <- FALSE
    while (!success) {
      res <- tryCatch(
        IS_lasso(
          data = data,
          nfactors = nfactors,
          lambda = max_lambda,
          maxiter = maxiter,
          eps = eps,
          INIT = INIT,
          nstarts = nstarts
        ),
        error = function(e) {
          message(sprintf("k = %d: IS_lasso failed (lambda = %g), retrying...",
                          k, lasso_vec[k]))
          NULL
        }
      )
      if (!is.null(res)) {
        a       <- res
        success <- TRUE
      }
      # else it loops and tries again
    }
    avec[k, ] <- c(
      lasso_vec[k], a$value, a$vaf, a$propzero,
      a$smallestP, a$maxsdP
    )
  }

  sel_lasso <- lasso_vec[which.max(avec[,2])]

  return(sel_lasso)
}



