test_that("seafar returns a seafar object with expected components", {
  set.seed(1)

  N <- 80
  J <- 12
  nf <- 3
  C  <- 18  # total nonzero loadings target (scalar C case)

  X <- matrix(rnorm(N * J), nrow = N, ncol = J)
  X <- scale(X)

  res <- seafar(X, nfactors = nf, C = C, INIT = "svd", orthogonal = FALSE)

  testthat::expect_s3_class(res, "seafar")
  testthat::expect_type(res, "list")
  testthat::expect_true(all(c("scores", "loadings", "PVE", "Residual") %in% names(res)))

  testthat::expect_true(is.matrix(res$loadings))
  testthat::expect_true(is.matrix(res$scores))

  testthat::expect_equal(dim(res$loadings), c(J, nf))
  testthat::expect_equal(dim(res$scores), c(N, nf))

  testthat::expect_type(res$PVE, "double")
  testthat::expect_true(all(is.finite(res$PVE)))
  testthat::expect_true(all(res$PVE <= 1 + 1e-8))
  testthat::expect_true(all(res$PVE >= -1e-8))

  testthat::expect_type(res$Residual, "double")
  testthat::expect_true(is.finite(res$Residual))
  testthat::expect_true(res$Residual >= -1e-8)

  # For scalar C branch: hard thresholding should make exactly C nonzeros (unless ties)
  nnz <- sum(abs(res$loadings) > 0)
  testthat::expect_equal(nnz, C)
})
