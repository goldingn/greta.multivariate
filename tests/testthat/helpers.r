compare_distribution <- function(greta_fun, r_fun, parameters, x,
                                 dim = NULL, multivariate = FALSE,
                                 tolerance = 1e-4) {
  # calculate the absolute difference in the log density of some data between
  # greta and a r benchmark.
  # 'greta_fun' is the greta distribution constructor function (e.g. normal())
  # 'r_fun' is the r density function, which must have argument 'log'
  # both of these functions must take the same parameters in the same order
  # 'parameters' is an optionally named list of numeric parameter values
  # x is the vector of values at which to evaluate the log density

  # define greta distribution, with fixed values
  greta_log_density <- greta_density(greta_fun, parameters, x,
                                     dim, multivariate)
  # get R version
  r_log_density <- log(do.call(r_fun, c(list(x), parameters)))

  # return absolute difference
  compare_op(r_log_density, greta_log_density, tolerance)

}

# evaluate the log density of x, given 'parameters' and a distribution
# constructor function 'fun'
greta_density <- function(fun, parameters, x,
                          dim = NULL, multivariate = FALSE) {

  if (is.null(dim))
    dim <- NROW(x)

  # add the output dimension to the arguments list
  dim_list <- list(dim = dim)

  # if it's a multivariate distribution name it n_realisations
  if (multivariate)
    names(dim_list) <- "n_realisations"

  # don't add it for wishart & lkj, which don't mave multiple realisations
  is_wishart <- identical(names(parameters), c("df", "Sigma"))
  is_lkj <- identical(names(parameters), c("eta", "dimension"))
  if (is_wishart | is_lkj)
    dim_list <- list()

  parameters <- c(parameters, dim_list)

  # evaluate greta distribution
  dist <- do.call(fun, parameters)
  distrib_node <- get_node(dist)$distribution

  # set density
  x_ <- as.greta_array(x)
  distrib_node$remove_target()
  distrib_node$add_target(get_node(x_))

  # create dag
  dag <- greta:::dag_class$new(list(x_))
  dag$define_tf()
  dag$set_tf_data_list("batch_size", 1L)
  dag$build_feed_dict()

  # get the log density as a vector
  dag$on_graph(
    result <- dag$evaluate_density(distrib_node, get_node(x_))
  )
  assign("test_density", result, dag$tf_environment)

  density <- dag$tf_sess_run(test_density)
  as.vector(density)

}

# zero-inflated distribution from rethinking package
dzipois <- function(x , theta , lambda , log=FALSE ) {
     ll <- ifelse( x==0 , theta + (1-theta)*exp(-lambda) , (1-theta)*dpois(x,lambda,FALSE) )
     if(log){
       return(log(ll))
     }
     else {
       return(ll)
     }
}

# get_node
get_node <- function(x){
  attr(x, "node")
}

compare_op <- function(r_out, greta_out, tolerance = 1e-4) {
  difference <- as.vector(abs(r_out - greta_out))
  expect_true(all(difference < tolerance))
}
