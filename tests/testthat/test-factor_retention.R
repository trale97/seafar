test_that("factor retention works", {
  set.seed(10)

  Q <- factor_retention(data = ocean_std)

  expect_s3_class(Q, "nfactors")
  expect_type(Q, "list")
  expect_true(all(c("parallel", "parallel_comp", "scree", "kaiser", "parallel_error") %in% names(Q)))

  expect_equal(Q$parallel, 18)
  expect_equal(Q$parallel_comp, 16)
  expect_equal(unname(Q$scree), 5)
  expect_equal(Q$kaiser, 66)

})
