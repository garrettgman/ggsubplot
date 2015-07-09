setOldClass(c("gg", "ggplot"))
setOldClass(c("proto", "environment"))

check_sp_layer <- function(object) {
  errors <- character()
  if (!proto::is.proto(object@layer)) {
    msg <- "sp_layer must be a proto object."
    errors <- c(errors, msg)
  }
  if (!("embed" %in% ls(object@layer))) {
    msg <- "sp_layer should contain an `embed' variable. Try building with geom_subplot() or geom_subplot2d()"
    errors <- c(errors, msg)
  }
  if (length(errors) == 0)
    TRUE
  else
    errors
}


#' sp_layer class
#'
#' sp_layers are layers made with ggsubplot methods. They are equivalent to the
#' layers made by ggplot2 functions in all ways except that they contain extra
#' information that is used to divide the data into subplots and locate those
#' subplots witihn the layer when plotting.
#'
#' @export
#' @keywords internal
setClass("sp_layer", representation(layer = "proto"), validity = check_sp_layer)


check_ggsubplot <- function(object) {
  errors <- character()
  if (!is(object@.Data, "ggplot") & !is(object@.Data, "gg")) {
    msg <- "ggsubplot must be a gg or ggplot object."
    errors <- c(errors, msg)
  }
  if (length(errors) == 0)
    TRUE
  else
    errors
}

#' ggsubplot class
#'
#' a ggsubplot object is a ggplot object that has been extended to include methods
#' for embedding subplots when plotting.
#'
#' @export ggsubplot
#' @keywords internal
setClass("ggsubplot",
  contains = "gg",
  validity = check_ggsubplot
)
