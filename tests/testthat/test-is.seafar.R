testthat::skip_if_not_installed("qgraph")

data("big5", package = "qgraph")
big5_std <- as.matrix(scale(big5))

stopifnot(is.matrix(big5_std))

test_that("IS original works", {
  set.seed(10)

  IS <- is.seafar_original(
    data = big5_std,
    nfactors = 5,
    C = 240,
    INIT = "mixed",
    orthogonal = TRUE,
    nstarts = 5
  )

  expect_type(IS, "list")
  expect_true(all(c("value", "vaf", "propzero", "smallestP", "maxsdP") %in% names(IS)))
  expect_true(is.numeric(IS$value))
})

test_that("IS modified works", {
  set.seed(10)

  IS_mod <- is.seafar(
    data = big5_std,
    nfactors = 5,
    INIT = "svd",
    orthogonal = TRUE,
    nstarts = 1,
    TOL = 3,
    THR = .2
  )

  expect_s3_class(IS_mod, "is_seafar")

  expect_true(
    is.numeric(IS_mod) &&
      (length(IS_mod) == 1 || length(IS_mod) > 1)
  )

  expect_equal(unname(IS_mod), rep(46, 5))
})
