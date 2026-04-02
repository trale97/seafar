test_that("seafar_multistart for orthogonal factors works", {
  big5_orthogonal <- seafar_multistart(data = ocean_std,
                                       nfactors = 5,
                                       C = 240,
                                       orthogonal = T)


  #summary(big5_multistart)

  Pmat_orthogonal <- big5_orthogonal$loadings

  expect_s3_class(big5_orthogonal, "multistart")
  expect_type(big5_orthogonal, "list")
  expect_true(all(c("scores", "loadings", "PVE", "Loss") %in% names(big5_orthogonal)))

  expect_equal(sum(Pmat_orthogonal != 0), 240)
  expect_equal(dim(Pmat_orthogonal)[2], 5)
  expect_equal(dim(Pmat_orthogonal)[1], 240)

  n <- nrow(big5_orthogonal$scores)
  G <- crossprod(big5_orthogonal$scores) / n
  expect_equal(G, diag(ncol(big5_orthogonal$scores)), tolerance = 1e-8)
})

test_that("seafar_multistart for correlated factors works", {
  big5_corr <- seafar_multistart(data = ocean_std,
                                       nfactors = 5,
                                       C = 240,
                                       nstarts = 10)


  #summary(big5_multistart)

  Pmat_corr <- big5_corr$loadings

  expect_s3_class(big5_corr, "multistart")
  expect_type(big5_corr, "list")
  expect_true(all(c("scores", "loadings", "PVE", "Loss") %in% names(big5_corr)))

  expect_equal(sum(Pmat_corr != 0), 240)
  expect_equal(dim(Pmat_corr)[2], 5)
  expect_equal(dim(Pmat_corr)[1], 240)

})






