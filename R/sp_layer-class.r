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

#' environment S4 class
#'
#' @name environment-class
#' @aliases environment
#'
#' @exportClass environment
NULL

#' proto S4 class
#'
#' @name proto-class
#' @aliases proto
#'
#' @exportClass proto
setOldClass(c("proto", "environment"))

#' sp_layer class
#'
#' sp_layers are layers made with ggsubplot methods. They are equivalent to the
#' layers made by ggplot2 functions in all ways except that they contain extra
#' information that is used to divide the data into subplots and locate those
#' subplots witihn the layer when plotting.
#'
#' @name sp_layer-class
#' @rdname sp_layer-class
#' @exportClass sp_layer
#' @aliases show,sp_layer-method
#' @aliases c,sp_layer-method
#' @aliases rep,sp_layer-method
#' @aliases ls,sp_layer-method
#' @aliases [,sp_layer-method
#' @aliases [<-,sp_layer-method
#' @aliases $,sp_layer-method
#' @aliases $<-,sp_layer-method
#' @aliases +,ggplot,sp_layer-method
#' @aliases +,gg,sp_layer-method
#' @aliases +,ggsubplot,sp_layer-method
#' @aliases ggtransform,sp_layer-method
setClass("sp_layer", representation(layer = "proto"), validity = check_sp_layer)

#' @export
setMethod("show", signature(object = "sp_layer"), function(object) {
	print(object@layer)
})

#' @export
setMethod("c", signature(x = "sp_layer"), function(x, ...){
	# c(get_layer(x), unlist(lapply(list(...), get_layer)))
	stop("object of type 'sp_layer' is not subsettable")
})

#' @export
setMethod("rep", signature(x = "sp_layer"), function(x, ...){
	stop("object of type 'sp_layer' is not subsettable")
})

#' @exportMethod "["
setMethod("[", signature(x = "sp_layer"),
	function(x, i, j, ..., drop = TRUE) {
    	new("sp_layer", layer = x@layer[i])
	}
)

#' @exportMethod "[<-"
setMethod("[<-", signature(x = "sp_layer"), function(x, i, j, ..., value) {
  	x@layer[i] <- value
	x
})


#' @exportMethod "$"
setMethod("$", signature(x = "sp_layer"), function(x, name) {
	slot(x, "layer")[[name]]
})

#' @exportMethod "$<-"
setMethod("$<-", signature(x = "sp_layer"), function(x, name, value) {
	slot(x, "layer")[[name]] <- value
	x
})

#' @exportMethod "+"
setMethod("+", signature(e1 = "ggplot", e2 = "sp_layer"),
	function(e1, e2) {
		ggsubplot(e1 + e2@layer)
	}
)

#' @exportMethod "+"
setMethod("+", signature(e1 = "gg", e2 = "sp_layer"),
  function(e1, e2) {
    ggsubplot(e1 + e2@layer)
  }
)

#' @exportMethod "+"
setMethod("+", signature(e1 = "ggsubplot", e2 = "sp_layer"),
	function(e1, e2) {
		ggsubplot(e1@.Data + e2@layer)
	}
)

setGeneric("ls")

setMethod("ls", signature(name = "sp_layer"),
	function(name, pos = -1, envir = as.environment(pos), all.names = FALSE, pattern) {
		ls(slot(name, "layer"), all.names)
})

#' Create a sp_layer object
#'
#' sp_layer gives a ggplot2 layer object the S4 class sp_layer, see
#' \code{\link{sp_layer-class}}. ggplot layer objects are usually non-specific
#' \code{\link[proto]{proto}} class objects. A layer should contain an embed
#' variable before being given the class 'sp_layer.' See the function bodies of
#' \code{\link{geom_subplot}} and \code{\link{geom_subplot2d}} for examples.
#'
#' @export sp_layer
#' @param layer a proto object that can be used as a layer by the
#' \code{\link[ggplot2]{ggplot2}} package (i.e, ggplot() + layer should return a
#' graph).
sp_layer <- function(layer) {
	new("sp_layer", layer = layer)
}

#' Is an object (functionally) a sp_layer?
#'
#' Tests whether an object is or ever was a sp_layer.
#' @param x an R object
#' @return logical
is.sp_layer <- function(x) {
  "embed" %in% ls(x)
}
