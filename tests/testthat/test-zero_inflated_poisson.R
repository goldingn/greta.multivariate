test_that("zero inflated poisson distribution has correct density", {

  check_tf_version <- greta::.internals$utils$misc$check_tf_version
  skip_if_not(check_tf_version())
  source("helpers.r")

  compare_distribution(greta.multivariate::zero_inflated_poisson,
                       dzipois,
                       parameters = list(theta = 0.2, lambda = 2),
                       x = rpois(100, 2))

})
