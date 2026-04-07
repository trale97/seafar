#' Title Sparse exploratory approximate factor analysis using LASSO penalty
#'
#' @param data A data frame containing the dataset (standardized).
#' @param nfactors The number of factors.
#' @param lambda LASSO penalty tuning parameter.
#' @param maxiter Maximum number of iterations of the AO procedure.
#' @param eps Convergence criterion based on difference in loss between iterates.
#' @param INIT Method to initialize loadings.
#'
#' @return Factor loading and factor score matrices.
#' @examples
#'\dontrun{
#' big5_seafa <- seafar_lasso(ocean, 5, lambda = 200, INIT = "svd")
#'}
seafar_lasso <- function(data,
                         nfactors,
                         lambda,
                         maxiter = 50,
                         eps = 10^-4,
                         initloadings,
                         INIT = 'mixed'){
  converged <- FALSE
  N <- dim(data)[1]
  J <- dim(data)[2]
  ssx <- sum(data^2)
  convAO <- 0
  iter <- 1
  Lossc <- 1
  Lossvec <- Lossc   #Used to compute convergence criterium
  #1. Initialize loading matrix
  if (missing(initloadings)) {
    loadings <- seafar_init_l1(data, nfactors, INIT)
  } else {
    loadings <- initloadings
  }

  svd1 <- svd(data, nfactors, nfactors)
  scores <- svd1$u
  diffT <- 0
  diffP <- 0
  while (convAO == 0) {
    iter0 <- 1
    Losst <- 1
    Lossvec0 <- c()
    stopcritT0 <- 0
    #Lossvec1 <- 1
    #2.1. Update factor scores
    while(stopcritT0 == 0){
      Lossu1old <- ssres(data,scores,loadings)/ssx
      for (q in 1:nfactors){
        E <- data - scores %*% t(loadings)
        Er <- E + scores[,q] %*% t(loadings[,q])
        num <- Er %*% loadings[,q]
        scores[,q] <- sqrt(N) * num / sqrt(sum(num^2))
      }
      #t(scores)%*%scores
      #Calculate loss
      Lossu0 <- LOSS(data,scores,loadings, lambda)/ssx
      Lossvec0 <- c(Lossvec0,Lossu0)
      # check convergence
      if (iter0 > maxiter) {
        stopcritT0 <- 1
      }
      if (abs(Losst-Lossu0) < eps){
        stopcritT0 <- 1
      }
      iter0 <- iter0 + 1
      Losst <- Lossu0
    }
    #Loss <- ssres(DATA, scores, loadings)/ssx
    Lossu <- LOSS(data,scores,loadings, lambda)/ssx

    #2. Update loadings
    #Lossu1old <- LOSS(DATA,scores,loadings,lambda)

    for (q in 1:nfactors){
      E <- data - scores%*%t(loadings)
      Er <- E+scores[,q]%*%t(loadings[,q])
      crosstEr <- t(Er)%*%scores[,q]
      loadings[,q]<-sign(crosstEr)*apply(cbind(abs(crosstEr)-lambda/2,0),1,max)/N
    }

    #Calculate loss
    Lossu <- LOSS(data,scores,loadings,lambda)/ssx
    Lossvec <- c(Lossvec,Lossu)
    if (iter > maxiter) {
      convAO <- 1
    }
    #if (Lossc-Lossu < -1e-12) {
    #  warning('Increase in Loss')
    #  break
    #}
    if (abs(Lossc-Lossu) < eps) {
      convAO <- 1
    }
    iter <- iter + 1
    Lossc <- Lossu
  }
  if (iter < maxiter) {
    converged <- TRUE
  }

  result <- list('scores' = scores, 'loadings' = loadings, 'PVE' = 1-Lossvec, 'Residual' = Lossu*ssx, 'converged' = converged)

  attr(result, "class") <- "lasso"
  return(result)
}


#' Multistart procedure for seafar with the LASSO penalty.
#'
#' @param data A data frame containing the dataset (standardized).
#' @param nfactors The number of factors.
#' @param lambda LASSO penalty tuning parameter.
#' @param maxiter Maximum number of iterations of the AO procedure.
#' @param eps Convergence criterion based on difference in loss between iterates.
#' @param INIT Method to initialize loadings.
#' @param nstarts Number of starting values.
#'
#' @returns
#'\item{loadings}{The best estimated loading matrix.}
#'\item{scores}{The best estimated factor score matrix.}
#'\item{Lossvec}{A list of vectors of loss values of each starting value.}
#'\item{Loss}{A vector of loss values of the best starting value.}
#'\item{converged}{A scalar where 1 is converged and 0 is not converged.}
#'
#' @export
#'
#' @examples
#'\dontrun{
#' big5_result <- seafar_lasso_multistart(
#'   scale(big5),
#'   nfactors = 5,
#'   lambda = 10,
#'   INIT = 'random',
#'   nstarts = 50)
#'}
seafar_lasso_multistart <- function(data,
                                    nfactors,
                                    lambda,
                                    maxiter = 50,
                                    eps = 10^-4,
                                    initloadings,
                                    INIT,
                                    nstarts){
  if(missing(nstarts)){
    nstarts <- 20
  }

  Pout3d <- list()
  Tout3d <- list()
  LOSS <- array()
  LOSSvec <- list()
  converged <- array()

  for (n in 1:nstarts){
    result <- seafar_lasso(data, nfactors, lambda, maxiter, eps, initloadings, INIT)

    Pout3d[[n]] <- result$loadings
    Tout3d[[n]] <- result$scores
    LOSS[n] <- result$Residual
    LOSSvec[[n]] <- 1-result$PVE
    converged[n] <- result$converged
  }

  # check how many times the minimum loss was achieved
  best <- min(LOSS)
  tol <- 1e-2
  n_best <- sum(abs(LOSS - best) < tol)
  n_distinct <- length(unique(round(LOSS, 2)))

  # choose solution with lowest loss value
  k <- which(LOSS == min(LOSS))
  if (length(k)>1){
    pos <- sample(1:length(k), 1)
    k <- k[pos]
  }

  return_varselect <- list()
  return_varselect$loadings <- Pout3d[[k]]
  return_varselect$scores <- Tout3d[[k]]
  return_varselect$Lossvec <- LOSSvec
  return_varselect$Loss <- LOSS[k]
  return_varselect$all_losses <- LOSS
  return_varselect$n_best <- n_best
  return_varselect$n_distinct <- n_distinct
  return_varselect$converged <- converged[k]

  attr(return_varselect, "class") <- "lasso_multistart"

  return(return_varselect)
}

#' Display a summary of the results of \code{seafar_lasso_multistart()}.
#'
#' @param object Object of class inheriting from 'lasso_multistart'.
#' @param disp The default is \code{"loadings"} which returns only the estimated loadings matrix.
#'             Otherwise if \code{"full"}, function returns both the estimated loadings and factor scores matrices.
#' @param nrow Number of rows of the loading matrix to be displayed, default is 10.
#' @param ...  Argument to be passed to or from other methods.
#' @export
#' @examples
#'\dontrun{
#' summary(big5_result, disp = "full")
#'}
summary.lasso_multistart<- function(object, disp = "loadings", nrow = 10, ...){
  if (missing(disp)){
    disp = "loadings"
  }

  if (disp == "loadings"){
    cat(sprintf("\nThe number of nonzero loadings is: %s\n",
                sum(round(object$loadings,3) != 0)))
    cat(sprintf("\nThe estimated loadings matrix is \n"))
    print(utils::head(round(object$loadings,3), nrow))
  } else if (disp == "full") {
    cat(sprintf("\nThe number of nonzero loadings is: %s\n",
                sum(round(object$loadings,3) != 0)))

    cat(sprintf("\nThe estimated loadings matrix is \n"))
    print(utils::head(round(object$loadings,3),10))

    cat(sprintf("\nThe estimated factor scores matrix is \n"))
    print(object$scores)
  }

}
