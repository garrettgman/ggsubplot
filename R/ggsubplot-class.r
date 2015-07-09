
#' @export
#' @rdname ggsubplot-class
setMethod("show", signature(object = "ggsubplot"), function(object){
	print(object)
})

#' @export
#' @rdname ggsubplot-class
print.ggsubplot <- function(x, newpage = is.null(vp), vp = NULL, ...) {
  f <- get("set_last_plot", envir = asNamespace("ggplot2"))
  f(x)
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

