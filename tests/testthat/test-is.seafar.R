test_that("IS original works", {
  set.seed(10)

  IS <- is.seafar_original(data = ocean_std,
                           nfactors = 5,
                           C = 240,
                           INIT = 'mixed',
                           orthogonal = TRUE,
                           nstarts = 5)

  expect_type(IS, "list")
  expect_true(all(c("value", "vaf", "propzero", "smallestP", "maxsdP") %in% names(IS_warmstart)))
  expect_equal(IS$value, 0.03785803)

})

test_that("IS modified works", {
  set.seed(10)

  IS_mod <- is.seafar(data = ocean_std,
                      nfactors = 5,
                      INIT = 'svd',
                      orthogonal = TRUE,
                      nstarts = 1,
                      TOL = 3,
                      THR = .2)

  expect_s3_class(IS_mod, "is_seafar")

  expect_true(
    is.numeric(IS_mod) &&
      (length(IS_mod) == 1 || length(IS_mod) > 1)
  )

  expect_equal(unname(IS_mod), rep(46,5))

})

