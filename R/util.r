.x_aes <- c("x", "xend", "xmax", "xmin")
.y_aes <- c("y", "yend", "ymax", "ymin")

#' Get the first name in an expression
#' 
#' first_name retrieves the first name used in an expression. To be used with 
#' screening mappings for plyr or regular computation.
#' @keywords internal
#' @param expr an \code{\link{expression}} or \code{\link{call}} from which 
#' names are to be extracted
#' @return character
#' @export
first_name <- function(expr) {
  names <- all.names(expr)
  names <- names[names != "["]
  firsts <- names[1]
  firsts[is.na(firsts)] <- ""
  firsts
}

#' Get all mappings related to plotting on the x axis
#' 
#' get_xs retrieves all mappings that need to be altered for plotting on a new x axis
#'
#' @keywords internal
#' @param data a data frame
#' @export
get_xs <- function(data) {
  names(data)[names(data) %in% .x_aes]
}

#' Get all mappings related to plotting on the y axis
#'
#' get_ys retrieves all mappings that need to be altered for plotting on a new y axis
#'
#' @keywords internal
#' @param data a data frame
#' @export
get_ys <- function(data) {
  names(data)[names(data) %in% .y_aes]
}

#' Clone a ggplot2 layer object
#' 
#' layer_clone returns an identical object to the input. This object can be 
#' manipulated without unintentionally affecting other instances of the layer 
#' through proto based referencing behaviour.
#' 
#' @keywords internal
#' @param layer a ggplot2 layer object, a glayer object, or a list of such 
#' objects
#' @return a ggplot2 layer object, a glayer object, or a list of such objects
#' @export
layer_clone <- function(layer) {
  UseMethod("layer_clone")
}

#' @S3method layer_clone sp_layer
layer_clone.sp_layer <- function(layer) {
  f <- get("plot_clone", envir = asNamespace("ggplot2"))
  sp_layer(f(ggplot2::ggplot() + layer)$layers[[1]])
}

#' @S3method layer_clone proto
layer_clone.proto <- function(layer) {
  f <- get("plot_clone", envir = asNamespace("ggplot2"))
  f(ggplot2::ggplot() + layer)$layers[[1]]
}

#' @S3method layer_clone list
layer_clone.list <- function(layer) {
  lapply(layer, layer_clone)
}

#' Remove NULL objects from a list
#' 
#' null.omit removes the NULL elements from a list and returns the remaining 
#' objects as a more concise list.
#' 
#' @keywords internal
#' @param lst a list
#' @export
null_omit <- function(lst) {
  if (is.null(lst)) {
    return(NULL)
  }
  lst[!(unlist(lapply(lst, is.null)))]
}

my_cumsum <- function(vec) {
  vec[is.na(vec)] <- 0
  cumsum(vec)
}

"%:::%" <- function(p, f) {
  message(p, ":::", f)
  get(f, envir = asNamespace(p))
}