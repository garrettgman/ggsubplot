#' @include ggsubplot-class.r
NULL

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

setOldClass(c("proto", "environment"))

#' sp_layer class
#'
#' sp_layers are layers made with ggsubplot methods. They are equivalent to the
#' layers made by ggplot2 functions in all ways except that they contain extra
#' information that is used to divide the data into subplots and locate those
#' subplots witihn the layer when plotting.
#'
#' @export
setClass("sp_layer", representation(layer = "proto"), validity = check_sp_layer)

#' @export
#' @rdname sp_layer-class
setMethod("show", signature(object = "sp_layer"), function(object) {
	print(object@layer)
})

#' @export
#' @rdname sp_layer-class
setMethod("c", signature(x = "sp_layer"), function(x, ...){
	# c(get_layer(x), unlist(lapply(list(...), get_layer)))
	stop("object of type 'sp_layer' is not subsettable")
})

#' @export
#' @rdname sp_layer-class
setMethod("rep", signature(x = "sp_layer"), function(x, ...){
	stop("object of type 'sp_layer' is not subsettable")
})

#' @export
#' @rdname sp_layer-class
setMethod("[", signature(x = "sp_layer"),
	function(x, i, j, ..., drop = TRUE) {
    	new("sp_layer", layer = x@layer[i])
	}
)

#' @export
#' @rdname sp_layer-class
setMethod("[<-", signature(x = "sp_layer"), function(x, i, j, ..., value) {
  	x@layer[i] <- value
	x
})

#' @export
#' @rdname sp_layer-class
setMethod("$", signature(x = "sp_layer"), function(x, name) {
	slot(x, "layer")[[name]]
})

#' @export
#' @rdname sp_layer-class
setMethod("$<-", signature(x = "sp_layer"), function(x, name, value) {
	slot(x, "layer")[[name]] <- value
	x
})

#' @export
#' @rdname sp_layer-class
setMethod("+", signature(e1 = "ggplot", e2 = "sp_layer"),
	function(e1, e2) {
		ggsubplot(e1 + e2@layer)
	}
)

#' @export
#' @rdname sp_layer-class
setMethod("+", signature(e1 = "gg", e2 = "sp_layer"),
  function(e1, e2) {
    ggsubplot(e1 + e2@layer)
  }
)

#' @export
#' @rdname sp_layer-class
setMethod("+", signature(e1 = "ggsubplot", e2 = "sp_layer"),
	function(e1, e2) {
		ggsubplot(e1@.Data + e2@layer)
	}
)

#' Create a sp_layer object
#'
#' sp_layer gives a ggplot2 layer object the S4 class sp_layer, see
#' \code{\link{sp_layer-class}}. ggplot layer objects are usually non-specific
#' \code{\link[proto]{proto}} class objects. A layer should contain an embed
#' variable before being given the class 'sp_layer.' See the function bodies of
#' \code{\link{geom_subplot}} and \code{\link{geom_subplot2d}} for examples.
#'
#' @noRd
#' @param layer a proto object that can be used as a layer by the
#' \code{\link[ggplot2]{ggplot2}} package (i.e, ggplot() + layer should return a
#' graph).
sp_layer <- function(layer) {
	new("sp_layer", layer = layer)
}

is.sp_layer <- function(x) {
  "embed" %in% ls(x)
}
