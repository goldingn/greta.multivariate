#' @title Multivariate Modelling with greta
#' @name greta.multivariate
#'
#' @description A greta extension providing distributions and functions for
#' multivariate modelling (modelling multivariate response variables).
#'
#' @docType package
#' @import tensorflow
#' @importFrom greta .internals
#'
NULL

distrib <- greta::.internals$nodes$constructors$distrib
as.greta_array <- greta::.internals$greta_arrays$as.greta_array
tf_iprobit <- greta::.internals$tensors$tf_iprobit
fl <- greta::.internals$utils$misc$fl
get_seed <- greta::.internals$utils$misc$get_seed
tf_as_float <- greta::.internals$tensors$tf_as_float
tf_rowsums <- greta:::tf_rowsums

tfp <- reticulate::import("tensorflow_probability", delay_load = FALSE)
