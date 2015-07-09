setOldClass(c("gg", "ggplot"))
setOldClass(c("proto", "environment"))

#' sp_layer class
#'
#' sp_layers are layers made with ggsubplot methods. They are equivalent to the
#' layers made by ggplot2 functions in all ways except that they contain extra
#' information that is used to divide the data into subplots and locate those
#' subplots witihn the layer when plotting.
#'
#' @export
#' @keywords internal
setClass("sp_layer", slots = list(layer = "proto"))

#' ggsubplot class
#'
#' a ggsubplot object is a ggplot object that has been extended to include methods
#' for embedding subplots when plotting.
#'
#' @export ggsubplot
#' @keywords internal
setClass("ggsubplot", contains = "gg")
