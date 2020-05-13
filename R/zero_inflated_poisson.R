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
#'
#'
#'@example
#' \dontrun{
#'
#' y <- rpois(100, 2)
#' y[rbinom(100, 1, 0.3)] <- 0
#' y <- as_data(y)

#' theta <- beta(3, 3)
#' lbd <- normal(0, 1, truncation = c(0, Inf))
#' distribution(y) <- zero_inflated_poisson(theta, lbd)
#'
#'}
#'
#'
#' @importFrom R6 R6Class
#' @export

zero_inflated_poisson <- function (theta, lambda, dim = 1)
  distrib('zero_inflated_poisson', theta, lambda, dim)

zero_inflated_poisson_distribution <- R6Class(
  "zero_inflated_poisson_distribution",
  inherit = greta::.internals$nodes$node_classes$distribution_node,
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

        tf$math.log(theta * tf$nn$relu(fl(1) - x) + (fl(1) - theta) * tf$pow(lambda, x) * tf$exp(-lambda) / tf$exp(tf$math.lgamma(x + fl(1))))
      }

      sample <- function(seed) {

        binom <- tfp$distributions$Binomial(total_count = 1,
                                         probs = theta)

        pois <- tfp$distributions$Poisson(rate = lambda)

        zi <- binom$sample(seed = seed)
        lbd <- pois$sample(seed = seed)

        (fl(1) - zi) * lbd

      }

      list(log_prob = log_prob, sample = sample, cdf = NULL, log_cdf = NULL)
    },

    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL
  )
)
