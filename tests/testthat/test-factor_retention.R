testthat::skip_if_not_installed("qgraph")

data("big5", package = "qgraph")
big5_std <- as.matrix(scale(big5))

stopifnot(is.matrix(big5_std))

test_that("factor retention works", {
  set.seed(10)

  Q <- factor_retention(data = big5_std)

  expect_s3_class(Q, "nfactors")
  expect_type(Q, "list")
  expect_true(all(c("parallel", "parallel_comp", "scree", "kaiser") %in% names(Q)))

  expect_true(all(vapply(Q, is.numeric, logical(1))))
})
