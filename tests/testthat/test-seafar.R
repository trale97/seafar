test_that("seafar works", {
  big5_ortho <- seafar(data = ocean_std,
                       nfactors = 5,
                       C = 240,
                       orthogonal = T,
                       INIT = 'rational')

  Pmat_ortho <- big5_ortho$loadings

  expect_s3_class(big5_ortho, "seafar")
  expect_type(big5_ortho, "list")
  expect_true(all(c("scores", "loadings", "PVE", "Residual") %in% names(big5_ortho)))

  expect_equal(sum(Pmat_ortho != 0), 240)
  expect_equal(dim(Pmat_ortho)[2], 5)
  expect_equal(dim(Pmat_ortho)[1], 240)

  n <- nrow(big5_ortho$scores)
  G <- crossprod(big5_ortho$scores) / n
  expect_equal(G, diag(ncol(big5_ortho$scores)), tolerance = 1e-8)
})
