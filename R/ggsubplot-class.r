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

#' list S4 class
#' 
#' @name list-class
#' @aliases list
#'
#' @exportClass list
NULL

#' ggplot S4 class
#' 
#' @name ggplot-class
#' @aliases ggplot
#'
#' @exportClass ggplot
setOldClass(c("ggplot", "list"))

#' gg S4 class
#' 
#' @name gg-class
#' @aliases gg
#'
#' @exportClass gg
setOldClass(c("gg", "ggplot"))

#' ggsubplot class
#'
#' a ggsubplot object is a ggplot object that has been extended to include methods 
#' for embedding subplots when plotting. 
#'
#' @name ggsubplot-class
#' @rdname ggsubplot-class
#' @exportClass ggsubplot
#' @aliases show,ggsubplot-method
#' @aliases print,ggsubplot-method
#' @aliases show,ggsubplot-method
setClass("ggsubplot", contains = c("gg", "ggplot"), validity = check_ggsubplot)

#' @export
setMethod("show", signature(object = "ggsubplot"), function(object){
	print(object)
})

#' @S3method print ggsubplot
print.ggsubplot <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  ("ggplot2" %:::% "set_last_plot")(x)
    if (newpage) 
        grid::grid.newpage()
    data <- ggsubplot_build(x)
    gtable <- ggplot2::ggplot_gtable(data)
    if (is.null(vp)) {
        grid::grid.draw(gtable)
    }
    else {
        if (is.character(vp)) 
            grid::seekViewport(vp)
        else grid::pushViewport(vp)
        grid::grid.draw(gtable)
        grid::upViewport()
    }
    invisible(data)
}


#' Create a ggsubplot object
#' 
#' glyph_plot gives a ggplot object the S4 class `ggsubplot', see 
#' \code{\link{ggsubplot-class}}. ggsubplot denotes ggplot objects that contain 
#' extra information to be used to embed subplots when plotting. ggsubplot 
#' objects have similar, but different print and build methods than ggplot2 
#' objects.
#' 
#' @param ggplot a gg or ggplot object
#' @export ggsubplot
ggsubplot <- function(ggplot) {
	new("ggsubplot", .Data = ggplot)
}

