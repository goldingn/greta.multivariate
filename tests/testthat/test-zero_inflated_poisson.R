test_that("zero inflated poisson distribution has correct density", {

  check_tf_version <- greta::.internals$utils$misc$check_tf_version
  skip_if_not(check_tf_version())
  source("helpers.r")

  compare_distribution(greta.multivariate::zero_inflated_poisson,
                       dzipois,
                       parameters = list(theta = 0.2, lambda = 2),
                       x = rpois(100, 2))

})

test_that("zero_inflated_poisson can generate new samples",{

  check_tf_version <- greta::.internals$utils$misc$check_tf_version
  skip_if_not(check_tf_version())

  y <- as_data(rzipois(100, 0.3, 2))
  theta <- greta::beta(3, 3)
  lbd <- normal(0, 1, truncation = c(0, Inf))
  distribution(y) <- zero_inflated_poisson(theta, lbd, dim = 100)
  m <- model(theta, lbd)
  d <- mcmc(m, warmup = 10, n_samples = 10)

  cc <- calculate(y, values = d, nsim = 1)
  # test fail because greta.multivariate:::sample does not exist
  expect_type(cc, "list")

})
