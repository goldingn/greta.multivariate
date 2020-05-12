#' @name zero_inflated_poisson
#' @title Zero inflated (poisson) distribution
#'
#' @description greta probability distribution for a zero-inflated poisson distribution
#'
#' @details Zero inflated distribution with:
#'      if(y = 0)
#'      \deqn{theta + (1 - theta) * Poisson(lambda)}
#'      else
#'      \deqn{(1 - theta) * Poisson(lambda)}
#'
#' @param theta, scalar for the zero-inflation parameter
#' @param lambda, scalar the expected value of the poisson distribution
#' @param dim a scalar giving the number of rows in the resulting greta array

#'
#' @importFrom R6 R6Class
#' @export

zero_inflated_poisson <- function (theta, lambda, dim = 1)
  distrib('zero_inflated_poisson', theta, lambda, dim)

zero_inflated_poisson_distribution <- R6Class(
  "zero_inflated_poisson_distribution",
  inherit = .internals$nodes$node_classes$distribution_node,
  public = list(
    initialize = function(theta, lambda, dim) {
      theta <- as.greta_array(theta)
      lambda <- as.greta_array(lambda)
      # add the nodes as children and parameters
      dim <- greta::.internals$utils$checks$check_dims(theta, lambda, target_dim = dim)
      super$initialize("zero_inflated_poisson", dim, discrete = TRUE)
      self$add_parameter(theta, "theta")
      self$add_parameter(lambda, "lambda")
    },

    tf_distrib = function(parameters, dag) {
      theta <- parameters$theta
      lambda <- parameters$lambda
      log_prob <- function(x) {

        tf$log(theta * tf$cast(tf$nn$relu(fl(1) - x), tf$float32) + (fl(1) - theta) * tf$pow(lambda, x) * tf$exp(-lambda) / tf$exp(tf$lgamma(x + fl(1))))
      }

      list(log_prob = log_prob, cdf = NULL, log_cdf = NULL)
    },

    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL
  )
)
