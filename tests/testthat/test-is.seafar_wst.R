testthat::skip_if_not_installed("qgraph")

data("big5", package = "qgraph")
big5_std <- as.matrix(scale(big5))

stopifnot(is.matrix(big5_std))

test_that("IS with warm starts works", {
  set.seed(10)

  IS_warmstart <- is.seafar_wst(data = big5_std,
                                nfactors = 5,
                                INIT = 'rational',
                                card.length = 100)

  expect_s3_class(IS_warmstart, "ISwarmstart")
  expect_type(IS_warmstart, "list")
  expect_true(all(c("cardinality", "value", "pve", "propzero", "smallestP", "selcard") %in% names(IS_warmstart)))

  expect_equal(IS_warmstart$selcard, 290)
})
